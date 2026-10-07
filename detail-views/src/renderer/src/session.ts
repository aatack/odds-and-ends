import { EntityCache, type CacheState } from '../../core/graph/cache.ts'
import { foreignOf, itemOf, moduleInfos, viewOf } from '../../core/present.ts'
import type { Entity, ModuleInfo, NoteValues, Outcome, View, ViewRow } from '../../core/types.ts'
import type { Api } from './api.ts'
import type { Environment } from './environment.ts'
import * as S from './state.ts'

export interface Snapshot {
  state: S.State
  modules: ModuleInfo[]
  /** The view on screen, as a tree, with the selection and any edit laid over it. */
  view: View | null
  shown: S.ShownView
  /** What each open entity peek shows, by entity id. */
  peekViews: Record<string, View>
  /** An item named on screen (a pill), presented. Reading one asks for it. */
  item(id: string): Entity | null
  /** What is under way for each entity: `older`, an action's id, `hide`. */
  working: Record<string, string[]>
  /** The time, to the second, for anything that says how long ago. */
  now: number
  /**
   * A request to put the keyboard in the find field: a nonce, made by Ctrl+F
   * and cleared once the field takes it. A signal, not state: a view that opens
   * with its find already set doesn't take the keyboard.
   */
  findFocus: number
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
    this.snapshot = { state, modules: moduleInfos, view: null, shown: noRows, peekViews: {}, item: () => null, working: {}, now: Date.now(), findFocus: 0 }
    this.snapshot = this.derive(state)
  }

  async start(): Promise<() => void> {
    const stopCache = this.cache.subscribe(() => this.scheduleDerive())
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

  private update(state: S.State): void {
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
    // Walks for views no longer anywhere are dropped.
    const live = new Set([root, ...Object.keys(peekViews)])
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
    return { ...previous, state, view, shown, peekViews, item }
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

  /** The selected row, or null. */
  selected(): ViewRow | null {
    return S.selectedRow(this.snapshot.shown)
  }

  /** The view's root, presented. */
  root(): Entity | null {
    return this.snapshot.view?.root ?? null
  }

  // --- The stack and the cursor -------------------------------------------------

  navigate(id: string): void {
    this.update(S.navigate(this.state, id))
  }

  /** Enters a module: the stack starts again at its root. */
  enterModule(root: string): void {
    this.update(S.enter(this.state, root))
  }

  openModule(index: number): void {
    const module = this.snapshot.modules[index]
    if (module) this.enterModule(module.root)
  }

  back(): void {
    this.update(S.back(this.state))
  }

  forward(): void {
    this.update(S.forward(this.state))
  }

  /** Pops back to the view at `at` in the stack. */
  goTo(at: number): void {
    this.update(S.goTo(this.state, at))
  }

  move(delta: number): void {
    this.update(S.move(this.state, this.snapshot.shown, delta))
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
    return this.state.edit !== null && this.state.edit.root === S.focused(this.state)
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
      if (made?.type === 'link' && S.focused(this.state) === edit.root) this.select([...edit.path, made.destinationId])
    })
  }

  cancelEdit(): void {
    this.update(S.endEdit(this.state))
  }

  // --- Links ---------------------------------------------------------------------

  /**
   * Backspace: takes the selected row out from under its parent in this view;
   * at the view's root there is no parent here, so nothing happens. The cursor
   * moves to the row above.
   */
  unlinkSelected(): void {
    const row = this.selected()
    if (!row || row.path.length < 2) return
    const parent = row.path[row.path.length - 2]
    const rows = this.snapshot.shown.rows
    const above = rows.slice(0, this.snapshot.shown.selectedIndex).reverse().find((other) => other.kind === 'entity')
    if (above?.kind === 'entity') this.select(above.row.path)
    void this.api.unlink(parent, row.entity.id).then((outcome) => this.settle(parent, outcome))
  }

  /**
   * `x` (move), `r` (link to), `Shift+R` (link from): the first press marks
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


  // --- Actions on the root, and Slack's own -----------------------------------------

  compose(composing: boolean): void {
    this.update(S.setComposing(this.state, composing))
  }

  setDraft(text: string): void {
    this.update(S.setDraft(this.state, text))
  }

  startAction(action: string): void {
    if (this.snapshot.view?.actions.some((offered) => offered.id === action && !offered.disabled)) {
      this.update(S.startAction(this.state, action))
    }
  }

  /** Confirms the action waiting on the prompt, with whatever was typed. */
  async perform(): Promise<void> {
    const action = this.state.acting
    if (!action) return
    const id = S.focused(this.state)
    const text = this.state.drafts[S.draftKey(this.state)] ?? ''
    this.update(S.setComposing(S.setDraft(this.state, ''), false))
    this.settle(id, await this.working(id, action, () => this.api.perform(id, action, text)))
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

  /** Whether Space has a box to tick on the selected row. */
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

  /**
   * Hides the whole chat a message is in: the selected message, or the view's
   * root when that is one. Works for a chat that isn't listed too (a public
   * channel I'm not in), so none of its threads is listed again.
   */
  hideChatOfMessage(): void {
    const row = this.selected()
    const message = row?.entity.type === 'slack.message' ? row.entity : this.root()
    const chat = message?.type === 'slack.message' ? message.data.conversation : null
    if (typeof chat !== 'string') return
    const id = S.focused(this.state)
    void this.working(id, 'hide', () => this.api.unlink('slack', chat)).then((outcome) => this.settle('slack', outcome))
  }

  /** Loads the view's root further back: a conversation's history, a thread, or all of Slack. */
  older(): void {
    const id = S.focused(this.state)
    if (this.isWorking(id, 'older')) return
    void this.working(id, 'older', () => this.api.older(id)).then((outcome) => this.settle(id, outcome))
  }

  markRead(): void {
    const id = S.focused(this.state)
    void this.api.markRead(id).then((outcome) => this.settle(id, outcome))
  }
}
