import { EntityCache, type CacheState } from '../../core/graph/cache.ts'
import { focusOf, foreignOf, itemOf, moduleInfos } from '../../core/present.ts'
import type { Entity, Focus, ModuleInfo, Outcome } from '../../core/types.ts'
import type { Api } from './api.ts'
import type { Environment } from './environment.ts'
import * as S from './state.ts'

export interface Snapshot {
  state: S.State
  modules: ModuleInfo[]
  focus: Focus | null
  /** What each open entity peek shows, by entity id. */
  peekFoci: Record<string, Focus>
  /** An item named on screen (a pill), presented. Reading one asks for it. */
  item(id: string): Entity | null
  /** What is under way for each entity: `older`, or an action's id. */
  working: Record<string, string[]>
  /** The time, to the second, for anything that says how long ago. */
  now: number
}

const storageKey = 'detail-views.state'
/** How often everything in the cache is looked at again, to load whatever has gone stale. */
const revisitEvery = 20_000

/**
 * The app without a screen: latent state, the entity cache, every effect.
 *
 * Everything shown is worked out from the cache (`focusOf`, `itemOf`), which
 * answers at once with whatever it has and fetches the rest behind. Nothing
 * here asks the core what to show.
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
  private derivedFrom: CacheState | null = null
  /** The last thing shown for each item, so an item that hasn't changed keeps its identity and its row doesn't redraw. */
  private readonly shown = new Map<string, { json: string; entity: Entity }>()

  constructor(api: Api, env: Environment) {
    this.api = api
    this.env = env
    this.cache = new EntityCache({ scan: (ids) => api.scan(ids), load: (request) => api.load(request), foreign: foreignOf })
    const state = S.restore(env.load(storageKey), 'slack')
    this.snapshot = { state, modules: moduleInfos, focus: null, peekFoci: {}, item: () => null, working: {}, now: Date.now() }
    this.snapshot = this.derive(state, this.cache.get())
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
    this.publish(this.derive(state, this.cache.get()))
  }

  /** One derivation per burst of cache changes, however many there were. */
  private scheduleDerive(): void {
    if (this.deriving) return
    this.deriving = true
    queueMicrotask(() => {
      this.deriving = false
      this.publish(this.derive(this.snapshot.state, this.cache.get()))
    })
  }

  /** An item as last shown, if nothing about it has changed. */
  private stable = (entity: Entity): Entity => {
    const json = JSON.stringify(entity)
    const known = this.shown.get(entity.id)
    if (known?.json === json) return known.entity
    this.shown.set(entity.id, { json, entity })
    return entity
  }

  private stableFocus(focus: Focus, previous: Focus | null | undefined): Focus {
    const next = { ...focus, entity: focus.entity && this.stable(focus.entity), children: focus.children.map(this.stable) }
    if (
      previous &&
      previous.entity === next.entity &&
      previous.loading === next.loading &&
      previous.error === next.error &&
      previous.compose === next.compose &&
      JSON.stringify(previous.actions) === JSON.stringify(next.actions) &&
      previous.children.length === next.children.length &&
      previous.children.every((child, index) => child === next.children[index])
    ) {
      return previous
    }
    return next
  }

  /** Everything shown, from latent state and the cache. Reading is what asks for what is missing. */
  private derive(state: S.State, cache: CacheState): Snapshot {
    const source = this.cache.source(cache)
    const previous = this.snapshot
    // A cursor move changes neither the focus nor the cache: nothing to work out again.
    const same = cache === this.derivedFrom
    this.derivedFrom = cache
    const focusFor = (id: string, before: Focus | null | undefined) =>
      same && before ? before : this.stableFocus(focusOf(id, source), before)
    const focused = S.focused(state)
    const focus = focusFor(focused, focused === S.focused(previous.state) ? previous.focus : null)
    const peekFoci: Record<string, Focus> = {}
    for (const peek of state.peeks) {
      if (peek.target.kind !== 'entity') continue
      const id = peek.target.id
      peekFoci[id] ??= focusFor(id, previous.peekFoci[id])
    }
    if (same) {
      return { state, modules: moduleInfos, focus, peekFoci, item: previous.item, working: previous.working, now: previous.now }
    }
    const items = new Map<string, Entity | null>()
    const item = (id: string): Entity | null => {
      if (!items.has(id)) {
        const found = itemOf(id, source)
        items.set(id, found && this.stable(found))
      }
      return items.get(id)!
    }
    return { state, modules: moduleInfos, focus, peekFoci, item, working: previous.working, now: previous.now }
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

  /** Shows what a write did straight away, and any failure on the entity it was for. */
  private settle(id: string, outcome: Outcome): void {
    this.cache.apply(outcome.events)
    this.cache.setError(id, outcome.error)
  }

  private get state(): S.State {
    return this.snapshot.state
  }

  private get focus(): Focus | null {
    return this.snapshot.focus
  }

  selected() {
    return S.selected(this.state, this.focus)
  }

  navigate(id: string): void {
    this.update(S.navigate(this.state, id))
  }

  openModule(index: number): void {
    const module = this.snapshot.modules[index]
    if (module) this.navigate(module.root)
  }

  back(): void {
    this.update(S.back(this.state))
  }

  forward(): void {
    this.update(S.forward(this.state))
  }

  move(delta: number): void {
    this.update(S.move(this.state, this.focus, delta))
  }

  select(id: string): void {
    this.update(S.select(this.state, id))
  }

  open(): void {
    const child = this.selected()
    if (child) this.navigate(child.id)
  }

  /** Loads the focus again from its service, fresh or not. */
  refresh(): void {
    this.cache.refresh(S.focused(this.state))
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

  compose(composing: boolean): void {
    this.update(S.setComposing(this.state, composing))
  }

  setDraft(text: string): void {
    this.update(S.setDraft(this.state, text))
  }

  hasDraft(): boolean {
    return Boolean(this.state.drafts[S.draftKey(this.state)]?.trim())
  }

  startAction(action: string): void {
    if (this.focus?.actions.some((offered) => offered.id === action)) this.update(S.startAction(this.state, action))
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

  async send(): Promise<void> {
    const id = S.focused(this.state)
    const text = this.state.drafts[id] ?? ''
    if (!text.trim()) return
    this.update(S.setDraft(this.state, ''))
    this.settle(id, await this.api.submit(id, text))
  }

  toggle(): void {
    const child = this.selected()
    if (child) void this.api.toggle(child.id).then((outcome) => this.settle(child.id, outcome))
  }

  /** Loads the focus further back: a conversation's history, a thread, or all of Slack. */
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
