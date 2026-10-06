import type { Entity, Focus, ModuleInfo } from '../../core/types.ts'
import type { Api } from './api.ts'
import type { Environment } from './environment.ts'
import * as S from './state.ts'

export interface Snapshot {
  state: S.State
  focus: Focus | null
  modules: ModuleInfo[]
  /** What each open entity peek shows, by entity id. */
  peekFoci: Record<string, Focus>
  /** Items mentioned on screen (as pills, say), by id. */
  summaries: Record<string, Entity | null>
}

const storageKey = 'detail-views.state'

/** The app without a screen: latent state, the focus cache, every effect. */
export class Session {
  private readonly api: Api
  private readonly env: Environment
  private readonly listeners = new Set<() => void>()
  private snapshot: Snapshot
  private request = 0
  private previousPeeks: S.Peek[] = []
  /** How many things on screen mention each item. */
  private readonly wanted = new Map<string, number>()
  private summariesPending = false
  private peekOpening: ReturnType<typeof setTimeout> | null = null
  private peekClosing: ReturnType<typeof setTimeout> | null = null

  constructor(api: Api, env: Environment) {
    this.api = api
    this.env = env
    this.snapshot = { state: S.restore(env.load(storageKey), 'slack'), focus: null, modules: [], peekFoci: {}, summaries: {} }
  }

  async start(): Promise<() => void> {
    const modules = await this.api.modules()
    this.set({ modules })
    const stop = this.api.onChange(() => {
      void this.load()
      void this.loadPeeks()
      this.loadSummaries()
    })
    await Promise.all([this.load(), this.loadPeeks()])
    return stop
  }

  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  get = (): Snapshot => this.snapshot

  private set(patch: Partial<Snapshot>): void {
    const previous = this.snapshot.state
    this.snapshot = { ...this.snapshot, ...patch }
    if (patch.state && patch.state !== previous) this.env.save(storageKey, S.persisted(patch.state))
    for (const listener of this.listeners) listener()
  }

  private update(next: S.State): void {
    const moved = S.focused(next) !== S.focused(this.snapshot.state)
    this.set({ state: next, ...(moved ? { focus: null } : {}) })
    if (moved) void this.load()
    if (next.peeks !== this.previousPeeks) {
      this.previousPeeks = next.peeks
      void this.loadPeeks()
    }
  }

  /** Reads the focused entity; a slower answer for an older focus is dropped. */
  private async load(): Promise<void> {
    const id = S.focused(this.snapshot.state)
    const request = ++this.request
    const focus = await this.api.focus(id)
    if (request === this.request) this.set({ focus })
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

  refresh(): void {
    void this.api.refresh(S.focused(this.state))
  }

  /** Hovering something peekable opens a peek after a moment, so passing over one doesn't. */
  hoverPeek(target: S.PeekTarget, anchor: S.Rect): void {
    this.holdPeek()
    if (this.peekOpening) clearTimeout(this.peekOpening)
    const current = S.transientPeek(this.state)
    if (current && S.sameTarget(current.target, target)) return
    this.peekOpening = setTimeout(() => {
      this.peekOpening = null
      const key = `${Date.now().toString(36)}${Math.random().toString(36).slice(2, 6)}`
      this.update(S.showPeek(this.state, { key, target, anchor, rect: null, pinned: false }))
    }, 350)
  }

  /** Leaving the trigger or the peek closes it, unless the pointer moves between them. */
  leavePeek(): void {
    if (this.peekOpening) clearTimeout(this.peekOpening)
    this.peekOpening = null
    if (this.peekClosing) clearTimeout(this.peekClosing)
    this.peekClosing = setTimeout(() => {
      this.peekClosing = null
      this.update(S.closePeek(this.state, null))
    }, 300)
  }

  holdPeek(): void {
    if (this.peekClosing) clearTimeout(this.peekClosing)
    this.peekClosing = null
  }

  /** `null` closes the transient peek only. */
  closePeek(key: string | null): void {
    if (key === null) this.holdPeek()
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

  /**
   * Something on screen mentions an item and wants it kept fresh. Returns
   * the release, for when it goes.
   */
  want = (id: string): (() => void) => {
    this.wanted.set(id, (this.wanted.get(id) ?? 0) + 1)
    if (!(id in this.snapshot.summaries)) this.loadSummaries()
    return () => {
      const count = (this.wanted.get(id) ?? 1) - 1
      if (count > 0) this.wanted.set(id, count)
      else this.wanted.delete(id)
    }
  }

  /** One request for everything wanted, however many asked this tick. */
  private loadSummaries(): void {
    if (this.summariesPending) return
    this.summariesPending = true
    queueMicrotask(async () => {
      this.summariesPending = false
      const ids = [...this.wanted.keys()]
      if (ids.length === 0) return
      const summaries = await this.api.summaries(ids)
      this.set({ summaries: { ...this.snapshot.summaries, ...summaries } })
    })
  }

  /** Loads what each entity peek shows; a reply for a peek since closed is dropped. */
  private async loadPeeks(): Promise<void> {
    const ids = [...new Set(this.state.peeks.flatMap((peek) => (peek.target.kind === 'entity' ? [peek.target.id] : [])))]
    const loaded = await Promise.all(ids.map(async (id) => [id, await this.api.focus(id)] as const))
    const open = new Set(
      this.state.peeks.flatMap((peek) => (peek.target.kind === 'entity' ? [peek.target.id] : [])),
    )
    this.set({ peekFoci: Object.fromEntries(loaded.filter(([id]) => open.has(id))) })
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
    await this.api.perform(id, action, text)
  }

  async send(): Promise<void> {
    const id = S.focused(this.state)
    const text = this.state.drafts[id] ?? ''
    if (!text.trim()) return
    this.update(S.setDraft(this.state, ''))
    await this.api.submit(id, text)
  }

  toggle(): void {
    const child = this.selected()
    if (child) void this.api.toggle(child.id)
  }

  markRead(): void {
    void this.api.markRead(S.focused(this.state))
  }
}
