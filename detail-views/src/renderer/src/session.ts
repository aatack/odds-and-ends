import { EntityCache, type CacheState } from '../../core/graph/cache.ts'
import { findInContext, foreignOf, itemOf, lensOf, moduleInfos, viewOf } from '../../core/present.ts'
import { activity } from '../../core/modules/slack/view.ts'
import { byMade, claudeIds } from '../../core/modules/claude/view.ts'
import { answering, isTaskTopic, sendTargets, snoozeLength, topicsIds, type TopicDisplay, unsent, waitCandidates, waiting, waitsOf, type SendTarget, type Wait } from '../../core/modules/topics/view.ts'
import { entityScheme, type Entity, type ModuleInfo, type NoteValues, type Outcome, type View, type ViewRow } from '../../core/types.ts'
import type { Api } from './api.ts'
import type { Environment } from './environment.ts'
import * as S from './state.ts'
import { splitText } from '../../core/outline.ts'

/** What a topic's git buttons do. */
export type TopicGit = 'checkout' | 'master' | 'push'

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
  /** Every topic, the most recently active first: the topics mode's list (empty in the classic one). */
  topics: Entity[]
  /** The topics put down until something comes into what they wait on (`waiting`), by id. */
  waiting: string[]
  /** The topic on screen's waits, each with what has come into it since. */
  topicWaits: { wait: Wait; candidates: string[] }[]
  /** Where the topic on screen's box can send besides a note, in the order Tab goes through them. */
  topicTargets: SendTarget[]
  /** The items the chat on screen has opened out in its context pane, walked, by id. */
  contextViews: Record<string, View>
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
/** Whether two small derived values say the same, so one that didn't change keeps its identity. */
const sameJson = (a: unknown, b: unknown): boolean => a === b || JSON.stringify(a) === JSON.stringify(b)

/** The topics mode keeps its own stack, apart from the classic one's. */
const topicsStorageKey = 'detail-views.topics.state'
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

  /** Which UI this drives: the topics mode, or the classic one (the modules) behind its flag. */
  readonly topicsMode: boolean
  private readonly storageKey: string

  constructor(api: Api, env: Environment, options: { topics?: boolean } = {}) {
    this.api = api
    this.env = env
    this.topicsMode = options.topics ?? false
    this.storageKey = this.topicsMode ? topicsStorageKey : storageKey
    this.cache = new EntityCache({ scan: (ids) => api.scan(ids), load: (request) => api.load(request), foreign: foreignOf })
    const state = S.restore(env.load(this.storageKey), this.topicsMode ? topicsIds.root : 'slack')
    this.snapshot = { state, modules: moduleInfos, view: null, shown: noRows, peekViews: {}, sideView: null, topics: [], waiting: [], topicWaits: [], topicTargets: [], contextViews: {}, windowShown: {}, item: () => null, working: {}, now: Date.now(), findFocus: 0, centre: 0, toast: null }
    this.snapshot = this.derive(state)
  }

  async start(): Promise<() => void> {
    const stopCache = this.cache.subscribe(() => this.scheduleDerive())
    // Whatever arrived before this listened (the first loads can beat it at start) is shown now.
    this.scheduleDerive()
    const stopChanges = this.api.onChange((changed) => this.cache.invalidate(changed))
    const revisit = setInterval(() => this.cache.revisit(), revisitEvery)
    const clock = setInterval(() => {
      // A snooze that has just ended brings its topic back: the waits are worked out again.
      if (this.snoozeEnds !== null && Date.now() >= this.snoozeEnds) {
        this.snoozeEnded = true
        this.scheduleDerive()
      }
      this.publish({ ...this.snapshot, now: Date.now() })
    }, 1000)
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
    if (next.state !== this.snapshot.state) this.env.save(this.storageKey, S.persisted(next.state))
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
    const opened = state.chatContext[root] ?? []
    const contextViews: Record<string, View> = {}
    for (const id of opened) contextViews[id] = this.walk(id, state)
    const windowShown: Record<string, S.ShownView> = {}
    const mark = (window: string, root: string, view: View) => (windowShown[window] = S.markRows(view, state, root, previous.windowShown[window]))
    for (const peek of state.peeks) if (peek.target.kind === 'entity') mark(peek.key, peek.target.id, peekViews[peek.target.id])
    if (state.side && sideView) mark('side', state.side, sideView)
    for (const id of opened) mark(S.contextWindow(id), id, contextViews[id])
    // The list of topics is the walk of their root, one level down.
    const topicsView = this.topicsMode ? this.walk(topicsIds.root, state) : null
    const topics = topicsView ? topicsView.rows.filter((row) => row.depth === 1).map((row) => row.entity) : []
    const sameTopics = topics.length === previous.topics.length && topics.every((topic, at) => topic === previous.topics[at])
    // Walks for views no longer anywhere are dropped.
    const live = new Set([root, ...Object.keys(peekViews), ...(state.side ? [state.side] : []), ...opened, ...(this.topicsMode ? [topicsIds.root] : [])])
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
    // Waits and where a topic's box sends are read off the cache: only worked out again when it, or the list, changed.
    // A snooze ending changes them too, with no change to the cache.
    const sameCache = this.itemsFrom === cache && sameTopics && !this.snoozeEnded
    this.snoozeEnded = false
    const lens = sameCache ? null : lensOf(this.cache.source(cache))
    const now = Date.now()
    const waitingNow = lens ? topics.filter((topic) => waiting(topic.id, lens, now)).map((topic) => topic.id) : previous.waiting
    if (lens) {
      const ends = topics.flatMap((topic) => waitsOf(topic.data).flatMap((wait) => (wait.until !== undefined && wait.until > now ? [wait.until] : [])))
      this.snoozeEnds = ends.length ? Math.min(...ends) : null
    }
    const sameWaiting = waitingNow.length === previous.waiting.length && waitingNow.every((id, at) => id === previous.waiting[at])
    // Waits show on any topic; where a box sends, only on a task's.
    const onTopic = this.topicsMode && view.root?.type === 'topic'
    const sameTopic = sameCache && root === S.focused(previous.state)
    const topicWaits = !onTopic ? [] : sameTopic ? previous.topicWaits : waitsOf(view.root!.data).map((wait) => ({ wait, candidates: waitCandidates(root, wait, lens ?? lensOf(this.cache.source(cache))) }))
    const topicTargets = !onTopic || !isTaskTopic(view.root) ? [] : sameTopic ? previous.topicTargets : sendTargets(root, lens ?? lensOf(this.cache.source(cache)))
    this.itemsFrom = cache
    return {
      ...previous,
      state,
      view,
      shown,
      peekViews,
      sideView,
      topics: sameTopics ? previous.topics : topics,
      waiting: sameWaiting ? previous.waiting : waitingNow,
      topicWaits: sameJson(topicWaits, previous.topicWaits) ? previous.topicWaits : topicWaits,
      topicTargets: sameJson(topicTargets, previous.topicTargets) ? previous.topicTargets : topicTargets,
      contextViews,
      windowShown,
      item,
    }
  }

  private itemsFrom: CacheState | null = null
  /** When the next snooze of a topic ends, if one will; and whether one has, since the waits were last worked out. */
  private snoozeEnds: number | null = null
  private snoozeEnded = false
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

  /** The selected row of the window the keys act in, or null. A task topic's chat has none: it is a flat list, nothing in it selectable. Thoughts are a tree like any other. */
  selected(): ViewRow | null {
    if (this.onTopic()) return null
    return S.selectedRow(this.activeShown())
  }

  /** Whether the keys act in a task topic's chat (not thoughts: a tree, with nothing of Claude's). */
  private onTopic(): boolean {
    return this.state.window === null && isTaskTopic(this.root())
  }

  /** Whether the keys act in any topic, task or thoughts. */
  private onAnyTopic(): boolean {
    return this.state.window === null && this.root()?.type === 'topic'
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

  /** Opens an item in the context pane of the chat on screen out to its whole view, or folds it back. */
  toggleChatContext(id: string): void {
    this.update(S.toggleChatContext(this.state, S.focused(this.state), id))
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
    const opened = S.contextItemOf(window)
    if (opened !== null) this.toggleChatContext(opened)
    else if (window === 'side') this.closeSide()
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
    if (this.onTopic()) return
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

  setDialog(fields: S.DialogFields): void {
    const dialog = this.state.dialog
    if (dialog) this.update(S.setDialog(this.state, { ...dialog, ...fields } as S.Dialog))
  }

  /** Ctrl+.: the form naming a new topic, from anywhere in the topics mode. */
  canOpenTopicDialog(): boolean {
    return this.topicsMode && this.state.dialog === null
  }

  openTopicDialog(): void {
    if (this.canOpenTopicDialog()) this.update(S.setDialog(this.state, { kind: 'topic', name: '', display: 'task', path: '', worktree: false }))
  }

  /** `?` on a topic: the same form, for a topic that also goes into this one's chat. */
  canOpenChildTopicDialog(): boolean {
    return this.canOpenTopicDialog() && this.onTopic()
  }

  openChildTopicDialog(): void {
    if (this.canOpenChildTopicDialog()) this.update(S.setDialog(this.state, { kind: 'topic', name: '', display: 'task', path: '', worktree: false, parent: S.focused(this.state) }))
  }

  hasDialog(): boolean {
    return this.state.dialog !== null
  }

  cancelDialog(): void {
    this.update(S.setDialog(this.state, null))
  }

  /** Makes the session the form describes, under the Claude root, and pushes its view: its chat, ready for a prompt. A topic's form makes the topic, and opens it. */
  submitDialog(): void {
    const dialog = this.state.dialog
    if (!dialog) return
    if (dialog.kind === 'resolve') {
      this.update(S.setDialog(this.state, null))
      this.resolve(dialog.topic, true)
      return
    }
    if (dialog.kind === 'snooze') {
      const length = snoozeLength(dialog.length)
      if (length === null) return
      this.update(S.setDialog(this.state, null))
      void this.working(dialog.topic, 'snooze', () => this.api.topicSnooze(dialog.topic, Date.now() + length)).then((outcome) => {
        this.settle(dialog.topic, outcome)
        if (outcome.error) this.showToast(outcome.error)
      })
      return
    }
    if (dialog.kind === 'topic') {
      if (!dialog.name.trim()) return
      this.update(S.setDialog(this.state, null))
      // Thoughts have no path, whatever was typed before switching to them.
      const path = dialog.display === 'task' && dialog.path.trim() ? { cwd: dialog.path, worktree: dialog.worktree } : undefined
      this.makeTopic(dialog.name, path, dialog.parent, dialog.display)
      return
    }
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

  // --- Topics ----------------------------------------------------------------------

  /** A topic in the list was clicked: its chat fills the screen, its box hidden until Enter. */
  openTopic(id: string): void {
    this.update(S.setComposing(S.enter(this.state, id), false))
  }

  /** Typing the name of a new topic, at the head of the list. */
  setTopicName(text: string): void {
    this.update(S.setDraftFor(this.state, topicsIds.root, text))
  }

  /** Enter in that box: the topic is made, and opened. */
  createTopic(): void {
    const name = this.state.drafts[topicsIds.root] ?? ''
    if (!name.trim()) return
    this.update(S.setDraftFor(this.state, topicsIds.root, ''))
    this.makeTopic(name)
  }

  /** A topic named `name` is made (with its first path, and in another's chat, if given), and opened. */
  private makeTopic(name: string, path?: { cwd: string; worktree: boolean }, parent?: string, display?: TopicDisplay): void {
    void this.working(topicsIds.root, 'create', () => this.api.topicCreate(name, path, parent, display)).then((outcome) => {
      if (parent) this.settle(parent, outcome)
      this.settle(topicsIds.root, outcome)
      if (outcome.error) this.showToast(outcome.error)
      const made = outcome.events.find((event) => event.type === 'link' && event.sourceId === topicsIds.root)
      if (made?.type === 'link') this.openTopic(made.destinationId)
    })
  }

  /**
   * `.`: the next topic down the list (round from the open one, or from the
   * top) where Claude isn't working and that isn't waiting on anything still
   * to come (`waiting`), opened. Null when there is none.
   */
  private nextIdleTopic(): string | null {
    const lens = lensOf(this.cache.source(this.cache.get()))
    const topics = this.snapshot.topics
    const at = topics.findIndex((topic) => topic.id === S.focused(this.state))
    for (let step = 1; step <= topics.length; step++) {
      const topic = topics[(at + step) % topics.length]
      if (topic.id !== S.focused(this.state) && !answering(topic.id, lens) && !waiting(topic.id, lens)) return topic.id
    }
    return null
  }

  canNextIdleTopic(): boolean {
    return this.topicsMode && this.state.window === null && this.nextIdleTopic() !== null
  }

  nextIdle(): void {
    const id = this.nextIdleTopic()
    if (id) this.openTopic(id)
  }

  /** Whether a topic is open in the main view. */
  onTopicNow(): boolean {
    return this.onTopic()
  }

  /** Whether the topic on screen can be resolved now: not while Claude answers in it. */
  canResolveTopic(): boolean {
    return this.onAnyTopic() && !answering(S.focused(this.state), lensOf(this.cache.source(this.cache.get())))
  }

  /**
   * Resolves the topic on screen: out of the list, its worktree removed (only
   * one proved to be). If that would lose work, a form says what, to confirm
   * first.
   */
  resolveTopic(): void {
    if (!this.canResolveTopic()) return
    const id = S.focused(this.state)
    void this.working(id, 'resolve', () => this.api.topicResolveCheck(id)).then(({ warnings, error }) => {
      if (error) this.showToast(error)
      else if (warnings.length) this.update(S.setDialog(this.state, { kind: 'resolve', topic: id, warnings }))
      else this.resolve(id, false)
    })
  }

  /**
   * A topic's git buttons: `checkout` (its worktree's commit, in its repo),
   * `master` (the default branch there), `push` (its branch, to GitHub). The
   * core checks what it can only know from git, and says why it won't.
   */
  topicGit(action: TopicGit): void {
    if (!this.onTopic()) return
    const id = S.focused(this.state)
    const call = { checkout: () => this.api.topicCheckOut(id), master: () => this.api.topicCheckOutDefault(id), push: () => this.api.topicPush(id) }[action]
    void this.working(id, action, call).then((outcome) => {
      this.settle(id, outcome)
      this.showToast(outcome.error ?? { checkout: 'Checked out in the repo', master: 'Default branch checked out', push: 'Pushed' }[action])
    })
  }

  /** A form with nothing to type in (confirming a resolve): Enter confirms it and Escape gives up, from outside any text box. */
  hasConfirmDialog(): boolean {
    return this.state.dialog?.kind === 'resolve'
  }

  private resolve(id: string, force: boolean): void {
    void this.working(id, 'resolve', () => this.api.topicResolve(id, force)).then((outcome) => {
      this.settle(id, outcome)
      if (outcome.error) this.showToast(outcome.error)
      // Gone from the list: the window is left empty, as before a topic is chosen.
      else if (S.focused(this.state) === id) this.update(S.enter(this.state, topicsIds.root))
    })
  }

  /** Typing a directory in the open topic's path picker. */
  setPathDraft(text: string): void {
    if (this.onTopic()) this.update(S.setDraftFor(this.state, S.pathDraftKey(S.focused(this.state)), text))
  }

  /** Enter in the path picker's box: the directory typed, as a path. */
  addTypedPath(): void {
    const draft = this.onTopic() ? (this.state.drafts[S.pathDraftKey(S.focused(this.state))] ?? '') : ''
    if (draft.trim()) this.addPath(draft, false)
  }

  /** A path for the open topic (`cwd`, or a new worktree of it): an item in its chat; its next dispatch runs there. */
  addPath(cwd: string, worktree: boolean): void {
    if (!this.onTopic()) return
    const id = S.focused(this.state)
    void this.working(id, 'path', () => this.api.topicAddPath(id, cwd, worktree)).then((outcome) => {
      this.settle(id, outcome)
      if (outcome.error) this.showToast(outcome.error)
      else this.update(S.setDraftFor(this.state, S.pathDraftKey(id), ''))
    })
  }

  /** Whether a paste outside any text box goes into a topic: one is on screen, its box without the keyboard. */
  canPaste(): boolean {
    return this.onTopic() && !this.state.composing
  }

  /** Text pasted into the topic on screen: a link to something the app knows as that item, anything else as a message. */
  async paste(text: string): Promise<void> {
    if (!this.canPaste()) return
    const id = S.focused(this.state)
    const outcome = await this.api.topicPaste(id, text)
    this.settle(id, outcome)
    if (outcome.error) this.showToast(outcome.error)
  }

  /** A file pasted into the topic on screen: kept, as an item in its chat. */
  async attach(file: { name: string; mime: string; data: string }): Promise<void> {
    if (!this.onTopic()) return
    const id = S.focused(this.state)
    const outcome = await this.api.topicAttach(id, file)
    this.settle(id, outcome)
    if (outcome.error) this.showToast(outcome.error)
  }

  /** What is typed in the box of the topic on screen. */
  private topicDraft(): string {
    return this.onTopic() ? (this.state.drafts[S.focused(this.state)] ?? '') : ''
  }

  /**
   * Whether Ctrl+Enter sends to Claude: the topic on screen has messages
   * since its last dispatch, or one typed in its box, and Claude isn't still
   * answering the one before (never two prompts at once).
   */
  canDispatch(): boolean {
    if (!this.onTopic()) return false
    const id = S.focused(this.state)
    const lens = lensOf(this.cache.source(this.cache.get()))
    return !answering(id, lens) && (Boolean(this.topicDraft().trim()) || unsent(id, lens).length > 0)
  }

  /** Ctrl+Enter with something typed in a topic's box: it goes in as a message, even while Claude answers (to go with the next dispatch). */
  canSubmitAndDispatch(): boolean {
    if (this.topicTarget() && this.topicDraft().trim()) return false
    return this.canDispatch() || Boolean(this.topicDraft().trim())
  }

  /** Why Ctrl+Enter on a topic sends nothing, when it can't: said, rather than the key falling through to the side panel's Ctrl+Enter. */
  dispatchRefusal(): string | null {
    if (!this.onTopic() || this.canSubmitAndDispatch()) return null
    // What is typed is for Slack: Ctrl+Enter never sends it to Claude instead.
    if (this.topicTarget() && this.topicDraft().trim()) return 'The box is set to Slack: Enter sends it there, Tab goes back to a note'
    return answering(S.focused(this.state), lensOf(this.cache.source(this.cache.get()))) ? 'Claude is still answering' : 'Nothing new to send'
  }

  /**
   * Ctrl+Enter on a topic: what is in its box goes in as a message first
   * (the box closing, as Enter closes it), then every message since the last
   * dispatch goes to Claude at once, and its answer's bubble comes after
   * them; unless Claude is still answering, when the message waits.
   */
  async dispatch(): Promise<void> {
    if (!this.canSubmitAndDispatch()) return
    const id = S.focused(this.state)
    const text = this.topicDraft()
    const ready = this.canDispatch()
    if (text.trim()) {
      this.update(S.setComposing(S.setDraftFor(this.state, id, ''), false))
      this.settle(id, await this.api.submit(id, text))
    }
    if (!ready) {
      this.showToast('Claude is still answering: the message goes with the next Ctrl+Enter')
      return
    }
    const outcome = await this.working(id, 'dispatch', () => this.api.topicDispatch(id))
    this.settle(id, outcome)
    if (outcome.error) this.showToast(outcome.error)
  }

  // --- Where a topic's box sends, and what a topic waits on ---------------------------

  /** Where the box of the topic on screen sends: a Slack conversation or thread it offers, or null for a note. */
  topicTarget(): SendTarget | null {
    if (!this.onTopic()) return null
    const on = S.topicTargetOf(this.state, S.focused(this.state))
    return this.snapshot.topicTargets.find((target) => target.on === on) ?? null
  }

  /** Tab in a topic's box: it goes on to the next place it can send (a note, then each Slack one), when there is more than a note. */
  canCycleTopicTarget(): boolean {
    return this.onTopic() && this.snapshot.topicTargets.length > 0
  }

  cycleTopicTarget(): void {
    if (!this.canCycleTopicTarget()) return
    const targets = this.snapshot.topicTargets
    const at = targets.findIndex((target) => target.on === this.topicTarget()?.on)
    this.update(S.setTopicTarget(this.state, S.focused(this.state), at + 1 < targets.length ? targets[at + 1].on : null))
  }

  /**
   * Enter in a topic's box pointed at Slack: the text goes to Slack as me,
   * and into the chat as that message. The box goes back to a note, so a
   * second Enter can't send again by mistake.
   */
  private async sendToSlack(target: SendTarget, text: string): Promise<void> {
    const id = S.focused(this.state)
    this.update(S.setTopicTarget(S.setComposing(S.setDraftFor(this.state, id, ''), false), id, null))
    const outcome = await this.working(id, 'slack', () => this.api.topicSlackSend(id, target.on, text))
    this.settle(id, outcome)
    // Nothing went: what was typed comes back, to send again or change.
    if (outcome.error) {
      this.showToast(outcome.error)
      if (!(this.state.drafts[id] ?? '').trim()) this.update(S.setTopicTarget(S.setDraftFor(this.state, id, text), id, target.on))
    }
  }

  /**
   * Waits on a Slack message's thread, its conversation, or both, from now:
   * the topic on screen is put down until something comes into one.
   */
  waitOn(message: string, scope: 'thread' | 'channel' | 'both'): void {
    if (!this.onTopic()) return
    const data = this.snapshot.item(message)?.data ?? {}
    const channel = typeof data.channel === 'string' ? data.channel : message.startsWith('slack:conv:') ? message.slice('slack:conv:'.length) : null
    if (!channel) return
    const ts = typeof data.ts === 'string' ? data.ts : null
    const thread = ts ? `slack:msg:${channel}:${typeof data.threadTs === 'string' ? data.threadTs : ts}` : null
    const conversation = `slack:conv:${channel}`
    const on = scope === 'thread' ? [thread] : scope === 'channel' ? [conversation] : [thread, conversation]
    const id = S.focused(this.state)
    void this.working(id, 'wait', () => this.api.topicWait(id, on.filter((one): one is string => one !== null))).then((outcome) => {
      this.settle(id, outcome)
      if (outcome.error) this.showToast(outcome.error)
    })
  }

  /** Waits on another topic in the chat (until it is resolved) or a PR (until it is blocked by me, merged or closed): the topic on screen is put down until then. */
  waitFor(other: string): void {
    if (!this.onTopic()) return
    const id = S.focused(this.state)
    void this.working(id, 'wait', () => this.api.topicWait(id, [other])).then((outcome) => {
      this.settle(id, outcome)
      if (outcome.error) this.showToast(outcome.error)
    })
  }

  /** Whether the topic on screen can be snoozed from its box: the box empty, and set to a note (not Slack). */
  canSnooze(): boolean {
    return this.onAnyTopic() && this.state.dialog === null && !this.topicTarget() && !this.topicDraft().trim()
  }

  /** A form for how long to snooze the topic on screen for. */
  openSnoozeDialog(): void {
    if (this.canSnooze()) this.update(S.setDialog(this.state, { kind: 'snooze', topic: S.focused(this.state), length: '' }))
  }

  /** The × on a wait: the topic stops waiting on it. */
  unwait(on: string): void {
    const id = S.focused(this.state)
    void this.api.topicUnwait(id, on).then((outcome) => this.settle(id, outcome))
  }

  /** A message that came into a wait goes into the topic. */
  acceptCandidate(message: string): void {
    const id = S.focused(this.state)
    void this.api.link(id, message).then((outcome) => this.settle(id, outcome))
  }

  /** A message that came into a wait is passed over: no longer offered. */
  dismissCandidate(on: string, message: string): void {
    const id = S.focused(this.state)
    void this.api.topicDismiss(id, on, message).then((outcome) => this.settle(id, outcome))
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
    const opened = S.contextItemOf(window)
    if (opened !== null) this.toggleChatContext(opened)
    else if (window === 'side') this.closeSide()
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
    const view = window === 'side' ? this.snapshot.sideView : S.contextItemOf(window) !== null && id ? this.snapshot.contextViews[id] : id ? this.snapshot.peekViews[id] : null
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
    const target = this.topicTarget()
    if (target) {
      await this.sendToSlack(target, text)
      return
    }
    // A topic's box lets the keyboard go once its message is in: Enter gives it back.
    if (this.onTopic()) this.update(S.setComposing(this.state, false))
    // A chat's box in note mode writes a note in the chat, for the next prompt to bring in.
    if (this.root()?.type === 'claude.session' && S.chatModeOf(this.state, id) === 'note') {
      this.settle(id, await this.api.create(id, text))
      return
    }
    this.settle(id, await this.api.submit(id, text))
  }

  /** Moves the chat on screen to another directory, or a new worktree of its repo; its conversation goes with it. */
  setDirectory(cwd: string, worktree: boolean): void {
    const id = S.focused(this.state)
    void this.working(id, 'directory', () => this.api.claudeSetDirectory(id, cwd, worktree)).then((outcome) => {
      this.settle(id, outcome)
      if (outcome.error) this.showToast(outcome.error)
    })
  }

  /** Tab while a chat's box doesn't have the keyboard: it takes it again, and the chat hides its selection. */
  canReturnToPrompt(): boolean {
    const type = this.root()?.type
    return this.state.window === null && !this.state.composing && (type === 'claude.session' || isTaskTopic(this.root())) && this.state.edit === null
  }

  returnToPrompt(): void {
    this.update(S.setComposing(this.state, true))
  }

  /** Enter in a topic's box with nothing typed in it: the box lets the keyboard go (Enter again takes it back). */
  canLeaveTopicBox(): boolean {
    return this.onTopic() && this.state.composing && !(this.state.drafts[S.draftKey(this.state)] ?? '').trim()
  }

  /** Enter in a topic's chat while its box doesn't have the keyboard: the box takes it again. */
  canReturnToTopicBox(): boolean {
    return this.onTopic() && this.canReturnToPrompt()
  }

  /** What the chat on screen's box writes: a prompt, or a note. */
  chatMode(): S.ChatMode {
    return S.chatModeOf(this.state, S.focused(this.state))
  }

  setChatMode(mode: S.ChatMode): void {
    this.update(S.setChatMode(this.state, S.focused(this.state), mode))
  }

  /** ←/→ go through the modes of a chat's box while nothing is typed in it (else they move the caret). */
  canCycleChatMode(): boolean {
    return this.root()?.type === 'claude.session' && !(this.state.drafts[S.draftKey(this.state)] ?? '')
  }

  cycleChatMode(delta: 1 | -1): void {
    const at = S.chatModes.indexOf(this.chatMode())
    this.setChatMode(S.chatModes[(at + delta + S.chatModes.length) % S.chatModes.length])
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
