import { openDatabase } from './db.ts'
import type { Module, ModuleContext } from './modules/module.ts'
import { Slack } from './modules/slack/slack.ts'
import { tasksModule } from './modules/tasks.ts'
import { Store } from './store.ts'
import type { Entity, Focus, ModuleInfo } from './types.ts'

const sweepEvery = 10 * 60_000

export interface CoreOptions {
  /** A file path, or `:memory:`. */
  path: string
  fetch?: typeof fetch
  now?: () => number
}

/**
 * The whole app without a screen. Everything it can do is in `actions`, which
 * is the only thing a UI reaches.
 */
export class Core {
  readonly store: Store
  readonly modules: Module[]
  readonly slack: Slack

  private readonly listeners = new Set<() => void>()
  private readonly errors = new Map<string, string>()
  private readonly refreshedAt = new Map<string, number>()
  private readonly inFlight = new Map<string, Promise<void>>()
  private readonly now: () => number
  private pending = false
  private sweeper: ReturnType<typeof setInterval> | null = null

  constructor(options: CoreOptions) {
    this.now = options.now ?? Date.now
    this.store = new Store(openDatabase(options.path), { now: this.now, onChange: () => this.changed() })
    const context: ModuleContext = {
      store: this.store,
      fetch: options.fetch ?? fetch,
      setError: (id, error) => this.setError(id, error),
    }
    this.slack = new Slack(context)
    this.modules = [this.slack, tasksModule(this.store)]
    for (const module of this.modules) {
      if (!this.store.get(module.root.id)) this.store.put(module.root.id, module.root.type, {})
    }
  }

  /** Starts the sweeper. Leave it off in tests and call `sweep` directly. */
  start(): void {
    this.store.sweep()
    this.sweeper ??= setInterval(() => this.store.sweep(), sweepEvery)
  }

  stop(): void {
    if (this.sweeper) clearInterval(this.sweeper)
    this.sweeper = null
  }

  /** Called at most once per tick, however many writes there were. */
  onChange(listener: () => void): () => void {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  private changed(): void {
    if (this.pending) return
    this.pending = true
    queueMicrotask(() => {
      this.pending = false
      for (const listener of this.listeners) listener()
    })
  }

  private setError(id: string, error: string | null): void {
    if (error === null) this.errors.delete(id)
    else this.errors.set(id, error)
    this.changed()
  }

  moduleOf(entity: Entity): Module | null {
    return this.modules.find((module) => module.owns(entity)) ?? null
  }

  /**
   * What the focus view of `id` shows, from the cache, straight away. If the
   * cache is stale a refresh starts behind it and a change follows.
   */
  focus(id: string): Focus {
    const entity = this.store.get(id)
    if (!entity) {
      return { entity: null, children: [], module: null, compose: null, loading: false, error: 'not found' }
    }
    const module = this.moduleOf(entity)
    const stale = this.now() - (this.refreshedAt.get(id) ?? 0) > (module?.staleAfter?.(id) ?? 60_000)
    if (module?.refresh && stale && !this.inFlight.has(id)) void this.refresh(id)
    const present = (child: Entity) => this.moduleOf(child)?.present?.(child) ?? child
    const children = this.store.children(id)
    return {
      entity: present(entity),
      children: (module?.order?.(entity, children) ?? children).map(present),
      module: module?.id ?? null,
      compose: module?.compose?.(entity) ?? null,
      loading: this.inFlight.has(id),
      error: this.errors.get(id) ?? null,
    }
  }

  refresh(id: string): Promise<void> {
    const existing = this.inFlight.get(id)
    if (existing) return existing
    const entity = this.store.get(id)
    const module = entity && this.moduleOf(entity)
    if (!module?.refresh) return Promise.resolve()
    const running = module
      .refresh(id)
      .then(() => {
        this.refreshedAt.set(id, this.now())
        this.errors.delete(id)
      })
      .catch((error: unknown) => {
        this.refreshedAt.set(id, this.now())
        this.errors.set(id, error instanceof Error ? error.message : String(error))
      })
      .finally(() => {
        this.inFlight.delete(id)
        this.changed()
      })
    this.inFlight.set(id, running)
    this.changed()
    return running
  }

  /** The composer under a focus view. Errors land on the focused entity. */
  async submit(id: string, text: string): Promise<void> {
    const entity = this.store.get(id)
    const module = entity && this.moduleOf(entity)
    if (!entity || !module?.submit || !text.trim()) return
    try {
      await module.submit(entity, text)
      this.setError(id, null)
    } catch (error) {
      this.setError(id, error instanceof Error ? error.message : String(error))
    }
  }

  /** The single registry of what a UI may ask for. */
  readonly actions = {
    modules: (): ModuleInfo[] => this.modules.map(({ id, name, root }) => ({ id, name, root: root.id })),
    focus: ({ id }: { id: string }): Focus => this.focus(id),
    refresh: ({ id }: { id: string }): Promise<void> => this.refresh(id),
    submit: ({ id, text }: { id: string; text: string }): Promise<void> => this.submit(id, text),
    toggle: ({ id }: { id: string }): void => {
      const entity = this.store.get(id)
      if (entity?.type === 'task') this.store.patch(id, { done: !entity.data.done })
    },
    markRead: async ({ id }: { id: string }): Promise<void> => {
      try {
        await this.slack.markRead(id)
      } catch (error) {
        this.setError(id, error instanceof Error ? error.message : String(error))
      }
    },
    /** Bytes of an image a message presented, by the ref it gave. */
    slackImage: ({ ref }: { ref: string }) => this.slack.image(ref),
    link: ({ parent, child }: { parent: string; child: string }): void => this.store.link(parent, child),
    unlink: ({ parent, child }: { parent: string; child: string }): void => this.store.unlink(parent, child),
  }
}

export type Actions = Core['actions']
