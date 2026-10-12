import { EntityCache, type CacheState } from '../../core/graph/cache.ts'
import { findInContext, foreignOf, itemOf, lensOf, moduleInfos, viewOf } from '../../core/present.ts'
import { activity } from '../../core/modules/slack/view.ts'
import { byMade, claudeIds } from '../../core/modules/claude/view.ts'
import { entityScheme, type Entity, type ModuleInfo, type NoteValues, type Outcome, type View, type ViewRow } from '../../core/types.ts'
import type { Api } from './api.ts'
import type { Environment } from './environment.ts'
import * as S from './state.ts'
import { splitText } from '../../core/outline.ts'

export interface Snapshot {
  state: S.State
  modules: ModuleInfo[]
  /** The view on screen, as a tree, with the selection and any edit laid over it. */
  view: View | null
  shown: S.ShownView
  /** What each open entity peek shows, by entity id. */
  peekViews: Record<string, View>
  /** What the side panel shows, if it is open. */
  sideView: View | null
  /** What the chat on screen has open in its context pane, if anything. */
  contextView: View | null
  /** The rows of each window but the main view (`side`, or a peek's key), with its selection and any edit laid over them. */
  windowShown: Record<string, S.ShownView>
  /** An item named on screen (a pill), presented. Reading one asks for it. */
  item(id: string): Entity | null
  /** What is under way for each entity: `older`, an action's id, `hide`. */
  working: Record<string, string[]>
  /** The time, to the second, for anything that says how long ago. */
  now: number
  /** A passing message (what went wrong, say), until it times out. */
  toast: string | null
  /**
   * A request to put the keyboard in the find field: a nonce, made by Ctrl+F
   * and cleared once the field takes it. A signal, not state: a view that opens
   * with its find already set doesn't take the keyboard.
   */
  findFocus: number
  /**
   * A request (a nonce, 0 for none) to bring the main view's selected row to
   * the middle of the screen once its layout settles: made when the side
   * panel opens and squeezes the view.
   */
  centre: number
}

const storageKey = 'detail-views.state'
/** How often everything in the cache is looked at again, to load whatever has gone stale. */
const revisitEvery = 20_000
const noRows: S.ShownView = { rows: [], keys: [], selectedPath: [], selectedIndex: -1 }

/**
 * Rows a view's walk may reach at first. Raised, doubling, as the view scrolls
 * near its end (or doesn't fill the screen): see `loadMore`.
 */
export const pageSize = 200

/**
 * The app without a screen: latent state, the entity cache, every effect.
 *
 * Everything shown is derived: `viewOf` walks a view's tree over the cache,
 * and `markRows` lays the selection and any edit over it. The walk is
 * remembered per view against what can change its shape (its root, the folds,
 * the cache), and pointedly not the selection: moving the cursor re-marks the
 * rows and never walks the tree again. Nothing here asks the core what to show.
 */
export class Session {
  readonly cache: EntityCache
  private readonly api: Api
  private readonly env: Environment
  private readonly listeners = new Set<() => void>()
  private snapshot: Snapshot
  /** The pointer's peek, or null for the main view. */
  private hovering: string | null = null
  private peekOpening: ReturnType<typeof setTimeout> | null = null
  private peekClosing: ReturnType<typeof setTimeout> | null = null
  private deriving = false
  /** The last walk of each view, and what it was walked against. */
  private readonly walks = new Map<string, { root: string; folds: S.State['folds']; cache: CacheState; limit: number; view: View }>()
  /**
   * How far each view's walk may go, and the query it was raised for. Runtime
   * only. A budget belongs to its query, not its view: one raised a long way
   * to fill a screen off a narrow find isn't inherited once the find clears.
   */
  private readonly budgets = new Map<string, { shape: string; limit: number }>()

  private shapeOf(root: string, state: S.State): string {
    return JSON.stringify([root, state.folds, state.finds[root] ?? null])
  }

  private limitOf(root: string, state: S.State): number {
    const held = this.budgets.get(root)
    return held && held.shape === this.shapeOf(root, state) ? held.limit : pageSize
  }

  /**
   * Walks a view further: when it is scrolled near its end, or its rows don't
   * fill the screen. The ceiling doubles rather than growing by a page: the
   * limit is on the walk, not on the rows a find keeps, so a narrow find over a
   * wide tree would otherwise re-walk once per page until it found anything.
   */
  loadMore(root: string = S.focused(this.state)): void {
    const view = this.walks.get(root)?.view
    if (!view || view.complete) return
    const at = this.limitOf(root, this.state)
    this.budgets.set(root, { shape: this.shapeOf(root, this.state), limit: at + Math.max(pageSize, at) })
    this.publish(this.derive(this.state))
  }
  /** The last thing shown for each item, so an item that hasn't changed keeps its identity and its row doesn't redraw. */
  private readonly items = new Map<string, { json: string; entity: Entity }>()
  /** Likewise for rows, by key. */
  private readonly rowsShown = new Map<string, ViewRow>()

  constructor(api: Api, env: Environment) {
    this.api = api
    this.env = env
    this.cache = new EntityCache({ scan: (ids) => api.scan(ids), load: (request) => api.load(request), foreign: foreignOf })
    const state = S.restore(env.load(storageKey), 'slack')
    this.snapshot = { state, modules: moduleInfos, view: null, shown: noRows, peekViews: {}, sideView: null, contextView: null, windowShown: {}, item: () => null, working: {}, now: Date.now(), findFocus: 0, centre: 0, toast: null }
    this.snapshot = this.derive(state)
  }

  async start(): Promise<() => void> {
    const stopCache = this.cache.subscribe(() => this.scheduleDerive())
    // Whatever arrived before this listened (the first loads can beat it at start) is shown now.
    this.scheduleDerive()
    const stopChanges = this.api.onChange((changed) => this.cache.invalidate(changed))
    const revisit = setInterval(() => this.cache.revisit(), revisitEvery)
    const clock = setInterval(() => this.publish({ ...this.snapshot, now: Date.now() }), 1000)
    return () => {
      stopCache()
      stopChanges()
      clearInterval(revisit)
      clearInterval(clock)
    }
  }

  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  get = (): Snapshot => this.snapshot

  private publish(next: Snapshot): void {
    if (next.state !== this.snapshot.state) this.env.save(storageKey, S.persisted(next.state))
    this.snapshot = next
    for (const listener of this.listeners) listener()
  }

  private update(next: S.State): void {
    const state = S.settleWindow(next)
    if (state === this.snapshot.state) return
    this.publish(this.derive(state))
  }

  /** One derivation per burst of cache changes, however many there were. */
  private scheduleDerive(): void {
    if (this.deriving) return
    this.deriving = true
    queueMicrotask(() => {
      this.deriving = false
      this.publish(this.derive(this.snapshot.state))
    })
  }

  /** An item as last shown, if nothing about it has changed. */
  private stable = (entity: Entity): Entity => {
    const json = JSON.stringify(entity)
    const known = this.items.get(entity.id)
    if (known?.json === json) return known.entity
    this.items.set(entity.id, { json, entity })
    return entity
  }

  private stableRow(row: ViewRow): ViewRow {
    const next = { ...row, entity: this.stable(row.entity), parent: row.parent && this.stable(row.parent), above: row.above && this.stable(row.above) }
    const known = this.rowsShown.get(row.key)
    if (
      known &&
      known.entity === next.entity &&
      known.parent === next.parent &&
      known.above === next.above &&
      known.depth === next.depth &&
      known.open === next.open &&
      known.hasChildren === next.hasChildren &&
      known.loading === next.loading
    ) {
      return known
    }
    this.rowsShown.set(row.key, next)
    return next
  }

  /**
   * A view's walk: remembered while its root, the folds, the cache and its
   * budget are the same. Pointedly not the selection: that is laid over it.
   */
  private walk(root: string, state: S.State): View {
    const cache = this.cache.get()
    const limit = this.limitOf(root, state)
    const known = this.walks.get(root)
    if (known && known.folds === state.folds && known.cache === cache && known.limit === limit) return known.view
    const walked = viewOf(root, this.cache.source(cache), { folds: state.folds, limit })
    const view = { ...walked, root: walked.root && this.stable(walked.root), rows: walked.rows.map((row) => this.stableRow(row)) }
    this.walks.set(root, { root, folds: state.folds, cache, limit, view })
    return view
  }

  /** Everything shown, from latent state and the cache. Reading is what asks for what is missing. */
  private derive(state: S.State): Snapshot {
    const previous = this.snapshot
    const root = S.focused(state)
    const view = this.filtered(this.walk(root, state), state.finds[root] ?? null)
    const shown = S.markRows(view, state, root, root === S.focused(previous.state) ? previous.shown : undefined)
    const peekViews: Record<string, View> = {}
    for (const peek of state.peeks) if (peek.target.kind === 'entity') peekViews[peek.target.id] ??= this.walk(peek.target.id, state)
    const sideView = state.side ? this.walk(state.side, state) : null
    const opened = state.chatContext[root] ?? null
    const contextView = opened ? this.walk(opened, state) : null
    const windowShown: Record<string, S.ShownView> = {}
    const mark = (window: string, root: string, view: View) => (windowShown[window] = S.markRows(view, state, root, previous.windowShown[window]))
    for (const peek of state.peeks) if (peek.target.kind === 'entity') mark(peek.key, peek.target.id, peekViews[peek.target.id])
    if (state.side && sideView) mark('side', state.side, sideView)
    // Walks for views no longer anywhere are dropped.
    const live = new Set([root, ...Object.keys(peekViews), ...(state.side ? [state.side] : []), ...(opened ? [opened] : [])])
    for (const key of this.walks.keys()) if (!live.has(key)) this.walks.delete(key)
    for (const key of this.budgets.keys()) if (!live.has(key)) this.budgets.delete(key)
    const cache = this.cache.get()
    const item =
      previous.view && this.itemsFrom === cache
        ? previous.item
        : (() => {
            const source = this.cache.source(cache)
            const found = new Map<string, Entity | null>()
            return (id: string): Entity | null => {
              if (!found.has(id)) {
                const entity = itemOf(id, source)
                found.set(id, entity && this.stable(entity))
              }
              return found.get(id)!
            }
          })()
    this.itemsFrom = cache
    return { ...previous, state, view, shown, peekViews, sideView, contextView, windowShown, item }
  }

  private itemsFrom: CacheState | null = null
  private lastFilter: { view: View; find: string | null; filtered: View } | null = null

  /** A view under its find, remembered, so a cursor move doesn't filter it again. */
  private filtered(view: View, find: string | null): View {
    const last = this.lastFilter
    if (last && last.view === view && last.find === find) return last.filtered
    const filtered = S.filterView(view, find)
    this.lastFilter = { view, find, filtered }
    return filtered
  }

  private isWorking(id: string, what: string): boolean {
    return this.snapshot.working[id]?.includes(what) ?? false
  }

  /** Marks something under way on an entity until it settles, so its button can say so. */
  private async working<T>(id: string, what: string, run: () => Promise<T>): Promise<T> {
    const mark = (on: boolean) => {
      const now = (this.snapshot.working[id] ?? []).filter((other) => other !== what)
      const working = { ...this.snapshot.working, [id]: on ? [...now, what] : now }
      if (!working[id].length) delete working[id]
      this.publish({ ...this.snapshot, working })
    }
    mark(true)
    try {
      return await run()
    } finally {
      mark(false)
    }
  }

  /**
   * Shows what a write did straight away, and any failure on the entity it
   * was for. A write clears what was undone: redoing it would land it after.
   */
  private settle(id: string, outcome: Outcome): void {
    this.cache.apply(outcome.events)
    this.cache.setError(id, outcome.error)
    if (outcome.events.length) this.update(S.clearUndone(this.state))
  }

  // --- Undo -------------------------------------------------------------------------

  /** Ctrl+Z: takes my last action off the store (within five minutes), and out of the cache. */
  async undo(): Promise<void> {
    const { events } = await this.api.undo()
    if (!events.length) return
    this.cache.remove(events)
    this.update(S.pushUndone(this.state, events))
  }

  /** Ctrl+Y: writes the last undone action back, exactly as it was. */
  async redo(): Promise<void> {
    const step = this.state.undone[this.state.undone.length - 1]
    if (!step) return
    this.update(S.popUndone(this.state))
    const outcome = await this.api.redo(step)
    this.cache.apply(outcome.events)
  }

  // --- Find -------------------------------------------------------------------------

  /** Ctrl+F: opens the view's find field, or puts the keyboard back in it. */
  openFind(): void {
    const root = S.focused(this.state)
    const next = this.state.finds[root] == null ? S.setFind(this.state, '') : this.state
    this.publish({ ...this.derive(next), findFocus: this.findRequests += 1 })
  }

  private findRequests = 0

  /** The find field took the keyboard: the request is spent. */
  findFocused(): void {
    if (this.snapshot.findFocus) this.publish({ ...this.snapshot, findFocus: 0 })
  }

  setFind(text: string): void {
    this.update(S.setFind(this.state, text))
  }

  hasFind(): boolean {
    return this.state.finds[S.focused(this.state)] != null
  }

  clearFind(): void {
    this.update(S.setFind(this.state, null))
  }

  private get state(): S.State {
    return this.snapshot.state
  }

  /** The rows of the window the keys act in. */
  private activeShown(): S.ShownView {
    const window = this.state.window
    return window === null ? this.snapshot.shown : (this.snapshot.windowShown[window] ?? noRows)
  }

  /** The selected row of the window the keys act in, or null. */
  selected(): ViewRow | null {
    return S.selectedRow(this.activeShown())
  }

  /** The keys go to a window: the main view (null), `side`, or a peek, by its key. A transient peek they leave closes, unless the pointer is in it. */
  focusWindow(window: string | null): void {
    if (this.state.window === window) return
    this.update(S.keepHovered(S.focusWindow(this.state, window), this.hovering))
  }

  /** The view's root, presented. */
  root(): Entity | null {
    return this.snapshot.view?.root ?? null
  }

  // --- The stack and the cursor -------------------------------------------------

  /** Pushes a view of `id`, and tells Recents. */
  navigate(id: string): void {
    if (S.focused(this.state) === id) return
    this.update(S.navigate(this.state, id))
    void this.api.visit(id)
  }

  /** Opens an item in the context pane of the chat on screen, or (null) goes back to its overviews. */
  openInChatContext(id: string | null): void {
    this.update(S.openInChatContext(this.state, S.focused(this.state), id))
  }

  /**
   * Tab (Shift+Tab) in a chat's prompt box: the next (previous) chat, newest
   * made first, round from this one. It takes this chat's place on the
   * stack, so going through them doesn't pile them up.
   */
  nextChat(delta: 1 | -1): void {
    const here = S.focused(this.state)
    const source = this.cache.source(this.cache.get())
    const chats = byMade(
      lensOf(source)
        .children(claudeIds.root)
        .flatMap((id) => {
          const item = itemOf(id, source)
          return item?.type === 'claude.session' ? [item] : []
        }),
    )
    const n = chats.length
    const at = chats.findIndex((chat) => chat.id === here)
    const start = at >= 0 ? at : delta > 0 ? -1 : n
    const chat = n ? chats[(((start + delta) % n) + n) % n] : null
    if (!chat || chat.id === here) return
    this.update(S.replace(this.state, chat.id))
    void this.api.visit(chat.id)
  }

  /** Enters a module: the stack starts again at its root. */
  enterModule(root: string): void {
    this.update(S.enter(this.state, root))
  }

  openModule(index: number): void {
    const module = this.snapshot.modules[index]
    if (module) this.enterModule(module.root)
  }

  /** Shift+A: pops the stack, or closes the side panel or peek the keys are in. */
  back(): void {
    const window = this.state.window
    if (window === 'side') this.closeSide()
    else if (window !== null) this.closePeek(window)
    else this.update(S.back(this.state))
  }

  forward(): void {
    this.update(S.forward(this.state))
  }

  /** Pops back to the view at `at` in the stack. */
  goTo(at: number): void {
    this.update(S.goTo(this.state, at))
  }

  move(delta: number): void {
    this.update(S.move(this.state, this.activeShown(), delta))
  }

  select(path: string[]): void {
    this.update(S.select(this.state, path))
  }

  /** `a`: the selection moves to the selected row's parent in this view. */
  selectParent(): void {
    const row = this.selected()
    if (row && row.path.length > 1) this.select(row.path.slice(0, -1))
  }

  /** `d`: a new view rooted at the selected row. */
  open(): void {
    const row = this.selected()
    if (row && row.depth > 0) this.navigate(row.entity.id)
  }

  /** Opens or folds the selected row (its children walked or not). Nothing else. */
  fold(open: boolean): void {
    const row = this.selected()
    if (!row || row.depth === 0) return
    this.update(S.fold(this.state, row.entity.id, open))
  }

  /** `3`: the selected entity, as JSON, over everything. */
  inspect(): void {
    const row = this.selected()
    if (row) this.update(S.inspect(this.state, row.entity.id))
  }

  closeInspector(): void {
    this.update(S.inspect(this.state, null))
  }

  /** What the inspector shows: the entity as the cache rolls it up (values, links, times). */
  inspected(): { id: string; json: string } | null {
    const id = this.state.inspecting
    if (!id) return null
    const entity = this.cache.get().entries[id]?.entity
    return { id, json: JSON.stringify(entity ?? { id, unknown: true }, null, 2) }
  }

  // --- Claude ----------------------------------------------------------------------

  /** Shows a passing message for a few seconds. */
  showToast(text: string): void {
    this.publish({ ...this.snapshot, toast: text })
    if (this.toastTimer) clearTimeout(this.toastTimer)
    this.toastTimer = setTimeout(() => this.publish({ ...this.snapshot, toast: null }), 4000)
  }

  private toastTimer: ReturnType<typeof setTimeout> | null = null

  /** Shift+K: the form for a new Claude session. The last directory used is filled in. */
  openClaudeDialog(): void {
    const cwds = this.snapshot.item('claude')?.data.cwds
    const last = Array.isArray(cwds) && typeof cwds[0] === 'string' ? cwds[0] : ''
    this.update(S.setDialog(this.state, { kind: 'claude', name: '', cwd: last, worktree: false }))
  }

  setDialog(fields: Partial<S.SessionDialog>): void {
    const dialog = this.state.dialog
    if (dialog) this.update(S.setDialog(this.state, { ...dialog, ...fields }))
  }

  hasDialog(): boolean {
    return this.state.dialog !== null
  }

  cancelDialog(): void {
    this.update(S.setDialog(this.state, null))
  }

  /** Makes the session the form describes, under the Claude root, and pushes its view: its chat, ready for a prompt. */
  submitDialog(): void {
    const dialog = this.state.dialog
    if (!dialog) return
    this.update(S.setDialog(this.state, null))
    const root = S.focused(this.state)
    void this.working(root, 'claude', () =>
      this.api.claudeCreate({ name: dialog.name, cwd: dialog.cwd, worktree: Boolean(dialog.cwd.trim()) && dialog.worktree }),
    ).then((outcome) => {
      this.settle('claude', outcome)
      if (outcome.error) this.showToast(outcome.error)
      const made = outcome.events.find((event) => event.type === 'link' && event.sourceId === 'claude')
      if (made?.type === 'link') this.navigate(made.destinationId)
    })
  }

  /** Opens or closes the phone's list of every action. */
  setPhoneMenu(open: boolean): void {
    this.update(S.setPhoneMenu(this.state, open))
  }

  /** Sends a URL to the browser (the phone's way of following a link). */
  openExternal(url: string): void {
    this.api.openExternal(url)
  }

  /** Loads the view's root again from its service, fresh or not. */
  refresh(): void {
    this.cache.refresh(S.focused(this.state))
  }

  // --- Editing in place ------------------------------------------------------------

  /** `e`: edit the selected row's text, starting from what it shows. */
  startEdit(): void {
    const row = this.selected()
    if (!row) return
    const text = row.entity.data.text
    this.update(S.startEdit(this.state, row.path, typeof text === 'string' ? text : ''))
  }

  /**
   * Enter: a new note under the selected row, typed in a box just below it.
   * The row isn't opened until the note is written, so its children don't
   * spill out under the box. `/` makes it a heading, `?` a checkbox.
   */
  startCreate(values?: NoteValues): void {
    const row = this.selected()
    if (!row) return
    this.update(S.startCreate(this.state, row.path, values))
  }

  setEditDraft(draft: string): void {
    this.update(S.setEditDraft(this.state, draft))
  }

  hasEdit(): boolean {
    return this.state.edit !== null && this.state.edit.root === S.activeRoot(this.state)
  }

  /** Writes the edit: the row's text, or a new note. An empty box changes nothing. */
  commitEdit(): void {
    const edit = this.state.edit
    if (!edit) return
    this.update(S.endEdit(this.state))
    const id = edit.path[edit.path.length - 1]
    if (!edit.draft.trim()) return
    if (edit.mode === 'edit') {
      void this.api.setText(id, edit.draft).then((outcome) => this.settle(id, outcome))
      return
    }
    // Now the row opens, so the new note shows, selected, among its siblings.
    if (edit.path.length > 1) this.update(S.fold(this.state, id, true))
    void this.api.create(id, edit.draft, edit.values).then((outcome) => {
      this.settle(id, outcome)
      const made = outcome.events.find((event) => event.type === 'link')
      if (made?.type === 'link' && S.activeRoot(this.state) === edit.root) this.select([...edit.path, made.destinationId])
    })
  }

  cancelEdit(): void {
    this.update(S.endEdit(this.state))
  }

  // --- Links ---------------------------------------------------------------------

  /**
   * Backspace: takes the selected row out from under its parent in this view;
   * at the view's root there is no parent here, so nothing happens. The cursor
   * moves to the next sibling, or to the parent if it was the last.
   */
  unlinkSelected(): void {
    const row = this.selected()
    if (!row || row.path.length < 2) return
    const parent = row.path[row.path.length - 2]
    const depth = row.path.length
    let next: string[] = row.path.slice(0, -1)
    const shown = this.activeShown()
    for (const other of shown.rows.slice(shown.selectedIndex + 1)) {
      if (other.kind !== 'entity') continue
      if (other.row.path.length < depth) break
      if (other.row.path.length === depth) {
        next = other.row.path
        break
      }
    }
    this.select(next)
    void this.api.unlink(parent, row.entity.id).then((outcome) => this.settle(parent, outcome))
  }

  /**
   * `x` (move), `r` (link to), `Shift+R` (link from), `c` (share a context): the first press marks
   * the selected row; moving to another row, in any view, and pressing the
   * same key again finishes it there. Escape gives up.
   */
  pick(tool: S.PickTool): void {
    const picking = this.state.picking
    const row = this.selected()
    if (picking?.tool === tool) {
      this.update(S.endPick(this.state))
      if (row) this.finishPick(picking, row.entity.id)
      return
    }
    if (!row) return
    if (tool === 'move' && row.path.length < 2) return
    this.update(S.startPick(this.state, tool, row.path))
  }

  cancelPick(): void {
    this.update(S.endPick(this.state))
  }

  private finishPick(picking: S.Pick, target: string): void {
    const subject = picking.path[picking.path.length - 1]
    const done = (id: string) => (outcome: Outcome) => this.settle(id, outcome)
    if (picking.tool === 'move') {
      const from = picking.path[picking.path.length - 2]
      void this.api.move(subject, from, target).then(done(target))
    } else if (picking.tool === 'link') {
      void this.api.link(subject, target).then(done(subject))
    } else if (picking.tool === 'context') {
      void this.api.context(subject, target).then(done(target))
    } else {
      void this.api.link(target, subject).then(done(target))
    }
  }

  /**
   * Hovering something peekable opens a peek after a moment, so passing over
   * one doesn't. `origin` is the peek the thing is in (null: the main view);
   * the new one stacks on it.
   */
  hoverPeek(target: S.PeekTarget, anchor: S.Rect, origin: string | null): void {
    this.hovering = origin
    this.holdPeek()
    if (this.peekOpening) clearTimeout(this.peekOpening)
    const top = S.transientPeek(this.state)
    if (top && top.parent === origin && S.sameTarget(top.target, target)) return
    this.peekOpening = setTimeout(() => {
      this.peekOpening = null
      const key = `${Date.now().toString(36)}${Math.random().toString(36).slice(2, 6)}`
      this.update(S.showPeek(this.state, { key, target, anchor, rect: null, pinned: false, parent: origin }))
    }, 350)
  }

  /** The pointer went into a peek. */
  enterPeek(key: string): void {
    // Any close already scheduled still runs; it keeps this peek and those
    // under it, and closes whatever was stacked above.
    this.hovering = key
  }

  /**
   * The pointer left a trigger (`window` null) or a peek window. After a
   * moment, transient peeks it is no longer in or under close.
   */
  leavePeek(window: string | null): void {
    if (this.peekOpening) clearTimeout(this.peekOpening)
    this.peekOpening = null
    // Leaving a trigger leaves the pointer in the same window; leaving a
    // window leaves it on the main view until it enters another.
    if (window !== null && this.hovering === window) this.hovering = null
    if (this.peekClosing) clearTimeout(this.peekClosing)
    this.peekClosing = setTimeout(() => {
      this.peekClosing = null
      this.update(S.keepHovered(this.state, this.hovering))
    }, 300)
  }

  holdPeek(): void {
    if (this.peekClosing) clearTimeout(this.peekClosing)
    this.peekClosing = null
  }

  /** `null` closes the top transient peek. */
  closePeek(key: string | null): void {
    this.holdPeek()
    this.update(S.closePeek(this.state, key))
  }

  placePeek(key: string, rect: S.Rect): void {
    this.holdPeek()
    this.update(S.placePeek(this.state, key, rect))
  }

  raisePeek(key: string): void {
    this.update(S.raisePeek(this.state, key))
  }

  /** A link to the browser, an entity onto the trail. */
  openPeek(target: S.PeekTarget): void {
    if (target.kind === 'url') this.api.openExternal(target.url)
    else this.navigate(target.id)
    this.closePeek(null)
  }

  view(ref: string | null): void {
    this.update(S.view(this.state, ref))
  }

  // --- The side panel ----------------------------------------------------------------

  /**
   * Ctrl+Enter: up the path to the selected row (the stack, then the view's
   * rows), the first item with a context opens the first other item in that
   * context in the panel on the right: mostly, the Claude session attached to
   * it. Says so when there is none.
   */
  openSide(): void {
    const found = this.findInContext(null)
    if (!found) return this.showToast('Nothing in context here')
    this.showSide(found)
  }

  /** The nearest item of `type` in a context up the path to the selected row: the stack, then the view's rows. */
  private findInContext(type: 'claude.session' | null): string | null {
    const path = [...this.state.trail.slice(0, this.state.at), ...(this.snapshot.shown.selectedPath.length ? this.snapshot.shown.selectedPath : [S.focused(this.state)])]
    return findInContext(path, type, lensOf(this.cache.source(this.cache.get())))
  }

  private showSide(id: string): void {
    const opening = this.state.side === null
    this.update(S.setSide(this.state, id))
    // The view narrows as the panel opens: where I was goes to its middle.
    if (opening) this.publish({ ...this.snapshot, centre: (this.centreRequests += 1) })
  }

  private centreRequests = 0

  /** Dragging the side panel's edge. */
  resizeSide(width: number): void {
    this.update(S.setSideWidth(this.state, Math.round(width)))
  }

  closeSide(): void {
    this.update(S.setSide(this.state, null))
  }

  /** A window's prompt box (`side`, or a peek's key) took the keyboard, or gave it up. */
  composeWindow(window: string, composing: boolean): void {
    this.update(S.setPrompting(this.state, window, composing))
  }

  /** Escape in a window's prompt box: the keys go back to the main view. */
  leavePrompt(): void {
    this.focusWindow(null)
  }

  /** Typing in a window's prompt box: drafted by the window's item. */
  setWindowDraft(window: string, text: string): void {
    const id = S.rootOf(this.state, window)
    if (id) this.update(S.setDraftFor(this.state, id, text))
  }

  /** Ctrl+Enter in a window's prompt box: closes that window. */
  closeWindow(): void {
    const window = this.state.window
    if (window === 'side') this.closeSide()
    else if (window !== null) this.closePeek(window)
  }

  /** The session `k` sends to: the one open in the side panel, else the nearest in context. */
  private sessionToSend(): string | null {
    if (this.snapshot.sideView?.root?.type === 'claude.session') return this.state.side
    return this.findInContext('claude.session')
  }

  /** Whether `k` has a session to send to. */
  canSendToSession(): boolean {
    return this.selected() !== null && this.sessionToSend() !== null
  }

  /**
   * `k`: the selected item goes into a session's next prompt, as a reference
   * (`[](entity:<id>)`, drawn as its pill, and a space) after what is there. The session
   * opens in the side panel if it isn't already, and its box takes the keyboard.
   */
  sendToSession(): void {
    const row = this.selected()
    const id = this.sessionToSend()
    if (!row || !id) return
    if (this.state.side !== id) this.showSide(id)
    const draft = this.state.drafts[id] ?? ''
    // A space after it, so I can type straight on.
    const reference = `[](${entityScheme}${row.entity.id}) `
    this.update(S.setPrompting(S.setDraftFor(this.state, id, draft && !/\s$/.test(draft) ? `${draft} ${reference}` : `${draft}${reference}`), 'side', true))
  }

  /** Sends what is in the prompt box of the window the keys are in to its item (a Claude session's next prompt), and empties it. */
  async sendWindow(): Promise<void> {
    const window = this.state.window
    const id = window === null ? null : S.rootOf(this.state, window)
    const text = id ? (this.state.drafts[id] ?? '') : ''
    const view = window === 'side' ? this.snapshot.sideView : id ? this.snapshot.peekViews[id] : null
    if (!id || !text.trim() || view?.compose == null) return
    this.update(S.setDraftFor(this.state, id, ''))
    this.settle(id, await this.api.submit(id, text))
  }


  // --- Actions on the root, and Slack's own -----------------------------------------

  compose(composing: boolean): void {
    const peek = this.state.acting?.peek
    this.update(S.setComposing(this.state, composing))
    // A peek held open for its action closes, if the pointer has left it.
    if (peek && !this.state.acting) this.leavePeek(null)
  }

  setDraft(text: string): void {
    this.update(S.setDraft(this.state, text))
  }

  /** Starts an action on the view's root, or on a peek's (by its key). */
  startAction(action: string, peek: string | null = null): void {
    const target = peek === null ? null : this.state.peeks.find((one) => one.key === peek)?.target
    if (peek !== null && target?.kind !== 'entity') return
    const on = target?.kind === 'entity' ? target.id : S.focused(this.state)
    const view = peek === null ? this.snapshot.view : this.snapshot.peekViews[on]
    if (view?.actions.some((offered) => offered.id === action && !offered.disabled)) {
      this.update(S.startAction(this.state, { action, on, peek }))
    }
  }

  /** Confirms the action waiting on the prompt, with whatever was typed. */
  async perform(): Promise<void> {
    if (!this.state.acting) return
    const { action, on: id, peek } = this.state.acting
    const text = this.state.drafts[S.draftKey(this.state)] ?? ''
    const view = peek === null ? this.snapshot.view : this.snapshot.peekViews[id]
    const copy = view?.actions.find((offered) => offered.id === action)?.copy
    this.update(S.setComposing(S.setDraft(this.state, ''), false))
    if (peek) this.leavePeek(null)
    const outcome = await this.working(id, action, () => this.api.perform(id, action, text))
    this.settle(id, outcome)
    if (copy && !outcome.error) {
      await this.api.copy(copy).then(
        () => this.showToast('Copied'),
        () => this.showToast("Couldn't copy"),
      )
    }
  }

  /** Whether the view's composer is waiting (a Slack token, say). */
  composerOpen(): boolean {
    return this.snapshot.view?.compose != null && this.state.acting === null
  }

  /** Sends what is in the view's composer, and empties it. */
  async send(): Promise<void> {
    const id = S.focused(this.state)
    const text = this.state.drafts[S.draftKey(this.state)] ?? ''
    if (!text.trim()) return
    this.update(S.setDraft(this.state, ''))
    this.settle(id, await this.api.submit(id, text))
  }

  /** A click on a row's box: selects the row and ticks it. */
  toggleAt(path: string[]): void {
    this.select(path)
    this.toggle()
  }

  /** Whether Space has a box to tick on the selected row. */
  /** Whether the selected row's text has headings, bullets or blocks to split out. */
  canSplit(): boolean {
    const text = this.selected()?.entity.data.text
    return typeof text === 'string' && splitText(text) !== null
  }

  /** `b`: the selected row's text split into its first part and notes under it; the row opens to show them. */
  split(): void {
    const row = this.selected()
    if (!row || !this.canSplit()) return
    const id = row.entity.id
    if (row.depth > 0) this.update(S.fold(this.state, id, true))
    void this.api.split(id).then((outcome) => {
      this.settle(id, outcome)
      if (outcome.error) this.showToast(outcome.error)
    })
  }

  canToggle(): boolean {
    const row = this.selected()
    return row?.entity.type === 'task' || typeof row?.entity.data.open === 'boolean'
  }

  /** Space: ticks or unticks the selected row's box (a checkbox note, or a task from before notes). */
  toggle(): void {
    const row = this.selected()
    if (!row) return
    const id = row.entity.id
    if (row.entity.type === 'task') void this.api.toggle(id).then((outcome) => this.settle(id, outcome))
    else if (typeof row.entity.data.open === 'boolean') void this.api.setValue(id, 'open', !row.entity.data.open).then((outcome) => this.settle(id, outcome))
  }

  /** Whether Slack's watch is paused. */
  slackPaused(): boolean {
    return this.snapshot.item('slack')?.data['watch.paused'] === true
  }

  /** Pauses Slack's watch, or resumes it. */
  pauseSlack(): void {
    const paused = !this.slackPaused()
    void this.working('slack', 'pause', () => this.api.pauseSlack(paused)).then((outcome) => this.settle('slack', outcome))
  }

  /** Loads the view's root further back: a conversation's history, a thread, or all of Slack. */
  older(): void {
    const id = S.focused(this.state)
    if (this.isWorking(id, 'older')) return
    void this.working(id, 'older', () => this.api.older(id)).then((outcome) => this.settle(id, outcome))
  }

  /**
   * Wiping the cache store takes three presses: two to confirm, the third
   * wipes. Only the cache: what I made is never touched, and everything in the
   * cache loads again.
   */
  wipeCache(): void {
    const stage = this.state.wiping
    if (stage < 2) {
      this.update(S.setWiping(this.state, stage === 0 ? 1 : 2))
      return
    }
    this.update(S.setWiping(this.state, 0))
    void this.api.clearCache().then((outcome) => this.showToast(outcome.error ?? 'Cache wiped'))
  }

  cancelWipe(): void {
    this.update(S.setWiping(this.state, 0))
  }

  /**
   * What `m` marks read: the path to the selected row (or the view's root),
   * when something on it has had something come into it.
   */
  readTarget(): string[] | null {
    const row = this.selected()
    const path = row?.path ?? [S.focused(this.state)]
    const item = this.snapshot.item
    return path.some((id) => {
      const entity = item(id)
      return entity !== null && activity(entity) !== null
    })
      ? path
      : null
  }

  markRead(): void {
    const path = this.readTarget()
    const id = path?.[path.length - 1]
    if (path && id) void this.api.markRead(path).then((outcome) => this.settle(id, outcome))
  }

}
