import type { Focus, ModuleInfo } from '../../core/types.ts'
import type { Api } from './api.ts'
import type { Environment } from './environment.ts'
import * as S from './state.ts'

export interface Snapshot {
  state: S.State
  focus: Focus | null
  modules: ModuleInfo[]
}

const storageKey = 'detail-views.state'

/** The app without a screen: latent state, the focus cache, every effect. */
export class Session {
  private readonly api: Api
  private readonly env: Environment
  private readonly listeners = new Set<() => void>()
  private snapshot: Snapshot
  private request = 0

  constructor(api: Api, env: Environment) {
    this.api = api
    this.env = env
    this.snapshot = { state: S.restore(env.load(storageKey), 'slack'), focus: null, modules: [] }
  }

  async start(): Promise<() => void> {
    const modules = await this.api.modules()
    this.set({ modules })
    const stop = this.api.onChange(() => void this.load())
    await this.load()
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
    return Boolean(this.state.drafts[S.focused(this.state)]?.trim())
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
