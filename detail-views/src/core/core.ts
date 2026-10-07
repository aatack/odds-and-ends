import { execFile } from 'node:child_process'
import { tmpdir } from 'node:os'
import type { Source } from './graph/cache.ts'
import { bucketEvents, rollupEntity, type GraphEntity } from './graph/entity.ts'
import { eventKey, link, value, type AppEvent, type Changed, type Scan } from './graph/events.ts'
import { cacheMigrations, openDatabase, ownedMigrations } from './db.ts'
import { importLegacy } from './legacy.ts'
import { GitHub } from './modules/github/github.ts'
import type { Module, ModuleContext } from './modules/module.ts'
import { Slack } from './modules/slack/slack.ts'
import { me, tasksModule } from './modules/tasks/tasks.ts'
import { focusOf, lensOf, moduleInfos, toItem, typeOf, type Lens } from './present.ts'
import { Blobs, EventStore, Settings } from './store.ts'
import { loadedKey } from './types.ts'
import type { Entity, Focus, LoadPart, LoadRequest, LoadResult, ModuleInfo, Outcome } from './types.ts'

const changeEvery = 150
const clearEvery = 7 * 24 * 60 * 60_000
const checkEvery = 10 * 60_000
/** How far a scan reads past what it was asked for, in layers and in entities per layer. */
const scanDepth = 1
const scanOverscan = 64

export interface CoreOptions {
  /** The owned event log: a file path, or `:memory:`. */
  owned: string
  /** The cache store, likewise. Safe to delete at any time. */
  cache: string
  /** The database before these two, imported once if it is there. */
  legacy?: string
  fetch?: typeof fetch
  gh?: ModuleContext['gh']
  now?: () => number
}

function runGh(args: string[]): Promise<string> {
  return new Promise((resolve, reject) => {
    // Away from any checkout, so `--delete-branch` can only touch the remote.
    execFile('gh', args, { cwd: tmpdir(), maxBuffer: 32 * 1024 * 1024 }, (error, stdout, stderr) => {
      if (error) reject(new Error(stderr.trim() || error.message))
      else resolve(stdout)
    })
  })
}

const message = (error: unknown): string => (error instanceof Error ? error.message : String(error))

/**
 * The whole app without a screen. Everything it can do is in `actions`, which
 * is the only thing a UI reaches.
 *
 * It holds two stores: `owned`, the log of what I made, and `cache`, what was
 * loaded from elsewhere. Every read is of both together, so a later event in
 * either wins: what I write now overrides what Slack said, and a cached value
 * written at timestamp 0 never overrides anything of mine.
 */
export class Core {
  readonly owned: EventStore
  readonly cache: EventStore
  readonly settings: Settings
  readonly blobs: Blobs
  readonly modules: Module[]
  readonly slack: Slack
  readonly github: GitHub

  private readonly listeners = new Set<(changed: Changed) => void>()
  private readonly errors = new Map<string, string>()
  private readonly inFlight = new Map<string, Promise<LoadResult>>()
  private readonly now: () => number
  private readonly cacheMeta: { get(): number | null; set(at: number): void }
  private pendingChange: Set<string> | null = null
  private everything = false
  private notifying = false
  private lastNotified = 0
  private clearer: ReturnType<typeof setInterval> | null = null
  private stopWatches: (() => void) | null = null

  constructor(options: CoreOptions) {
    this.now = options.now ?? Date.now
    const ownedDb = openDatabase(options.owned, ownedMigrations)
    const cacheDb = openDatabase(options.cache, cacheMigrations)
    this.owned = new EventStore(ownedDb, 'log', (ids) => this.changed(ids))
    this.cache = new EventStore(cacheDb, 'snapshot', (ids) => this.changed(ids))
    this.settings = new Settings(ownedDb)
    this.blobs = new Blobs(cacheDb)
    this.cacheMeta = {
      get: () => {
        const row = cacheDb.prepare(`SELECT value FROM meta WHERE key = 'created'`).get() as { value: string } | undefined
        return row ? Number(row.value) : null
      },
      set: (at) => {
        cacheDb.prepare(`INSERT INTO meta (key, value) VALUES ('created', ?) ON CONFLICT (key) DO UPDATE SET value = excluded.value`).run(String(at))
      },
    }
    if (this.cacheMeta.get() === null) this.cacheMeta.set(this.now())
    if (options.legacy) importLegacy(options.legacy, this.owned, this.settings)

    const context: ModuleContext = {
      owned: this.owned,
      cache: this.cache,
      settings: this.settings,
      blobs: this.blobs,
      lens: this.lens(),
      fetch: options.fetch ?? fetch,
      gh: options.gh ?? runGh,
      load: async (id, part, force) => {
        const { error } = await this.load({ id, part, force })
        if (error) throw new Error(error)
      },
      now: this.now,
    }
    this.slack = new Slack(context)
    this.github = new GitHub(context)
    this.modules = [this.slack, this.github, tasksModule(context)]
  }

  /**
   * Starts what runs while the app is open: the weekly clear-out of the cache,
   * and the watches that keep it current. Leave it off in tests and call
   * `clearCache` and `slack.poll` directly.
   */
  start(): void {
    this.clearCacheIfOld()
    this.clearer ??= setInterval(() => this.clearCacheIfOld(), checkEvery)
    this.stopWatches ??= this.slack.watch()
  }

  stop(): void {
    if (this.clearer) clearInterval(this.clearer)
    this.clearer = null
    this.stopWatches?.()
    this.stopWatches = null
  }

  private clearCacheIfOld(): void {
    if (this.now() - (this.cacheMeta.get() ?? 0) > clearEvery) this.clearCache()
  }

  /** Empties the cache store. Everything in it loads again as it is next looked at. */
  clearCache(): void {
    this.cache.clear()
    this.blobs.clear()
    this.cacheMeta.set(this.now())
    this.changed(null)
  }

  // --- Change notification -----------------------------------------------------

  /**
   * Hears which entities changed, at most once per `changeEvery` ms however
   * many writes there were. Null means anything may have.
   */
  onChange(listener: (changed: Changed) => void): () => void {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  private changed(ids: string[] | null): void {
    if (ids === null) this.everything = true
    else for (const id of ids) (this.pendingChange ??= new Set()).add(id)
    if (this.notifying) return
    this.notifying = true
    const wait = Math.max(0, this.lastNotified + changeEvery - Date.now())
    setTimeout(() => {
      const changed = this.everything ? null : [...(this.pendingChange ?? [])]
      this.notifying = false
      this.everything = false
      this.pendingChange = null
      this.lastNotified = Date.now()
      for (const listener of this.listeners) listener(changed)
    }, wait)
  }

  // --- Reading -----------------------------------------------------------------

  /** Every event touching `ids`, from both stores. Cached first, so on a tie mine win. */
  read(ids: readonly string[]): AppEvent[] {
    return [...this.cache.read(ids), ...this.owned.read(ids)]
  }

  entity(id: string): GraphEntity {
    return rollupEntity(id, this.read([id]))
  }

  /** The stores as a cache that already has everything: what a headless caller reads through. */
  source(): Source {
    return {
      get: (ids) => {
        const buckets = bucketEvents(ids, this.read(ids))
        return Object.fromEntries(ids.map((id) => [id, rollupEntity(id, buckets.get(id) ?? [])]))
      },
      pending: () => false,
      loading: (id) => [...this.inFlight.keys()].some((key) => key.startsWith(`${id} `)),
      error: (id) => this.errors.get(id) ?? null,
      expand: () => {},
    }
  }

  lens(): Lens {
    return lensOf(this.source())
  }

  item(id: string): Entity | null {
    return toItem(this.entity(id))
  }

  /** What the focus view of `id` shows, worked out exactly as the UI does. */
  focus(id: string): Focus {
    return focusOf(id, this.source())
  }

  /**
   * Complete events for `ids`, and for what they link out to a layer down: a
   * view almost always walks downwards, so reading ahead saves a round trip.
   * A layer clipped by the overscan isn't reported as covered.
   */
  scan(ids: readonly string[]): Scan {
    const covered = new Set<string>()
    const seen = new Set<string>()
    const events: AppEvent[] = []
    let frontier = [...new Set(ids)]
    for (const id of frontier) covered.add(id)
    for (let layer = 0; frontier.length; layer++) {
      const batch = this.read(frontier)
      for (const e of batch) {
        const key = eventKey(e)
        if (seen.has(key)) continue
        seen.add(key)
        events.push(e)
      }
      if (layer >= scanDepth) break
      const buckets = bucketEvents(frontier, batch)
      const next = new Set<string>()
      for (const id of frontier) {
        for (const child of rollupEntity(id, buckets.get(id) ?? []).outboundLinks) if (!covered.has(child)) next.add(child)
      }
      frontier = [...next].slice(0, scanOverscan)
      for (const id of frontier) covered.add(id)
    }
    return { entityIds: [...covered], events }
  }

  // --- Loading from other services ----------------------------------------------

  private moduleFor(id: string, values: Record<string, unknown> = {}): Module | null {
    const type = typeOf(id, values)
    return type ? (this.modules.find((module) => module.view.owns(type)) ?? null) : null
  }

  /**
   * Loads part of an entity from its service into the cache store, then marks
   * it loaded on the entity. Unless forced, a part still fresh is left alone,
   * and a load already running is joined rather than repeated.
   */
  load(request: LoadRequest): Promise<LoadResult> {
    const { id, part } = request
    const key = `${id} ${part}`
    const running = this.inFlight.get(key)
    if (running) return running
    const entity = this.entity(id)
    const type = typeOf(id, entity.values)
    const module = this.moduleFor(id, entity.values)
    const fresh = type && module?.view.foreign?.(id, type)?.[part]
    if (!type || !module?.load || fresh === undefined || fresh === null) return Promise.resolve({ error: null })
    const loadedAt = Number(entity.values[loadedKey(part)]) || 0
    if (!request.force && loadedAt && this.now() - loadedAt < fresh) return Promise.resolve({ error: null })

    const started = module
      .load(id, part, type)
      .then((): LoadResult => {
        this.cache.write([value(id, loadedKey(part), this.now(), 0, 'app')])
        this.errors.delete(id)
        return { error: null }
      })
      .catch((error: unknown): LoadResult => {
        this.errors.set(id, message(error))
        this.changed([id])
        return { error: message(error) }
      })
      .finally(() => this.inFlight.delete(key))
    this.inFlight.set(key, started)
    return started
  }

  /** Loads every part of an entity again, fresh or not. */
  async refresh(id: string): Promise<void> {
    const parts: LoadPart[] = ['self', 'children']
    await Promise.all(parts.map((part) => this.load({ id, part, force: true })))
  }

  // --- Doing things --------------------------------------------------------------

  private async attempt(id: string, body: () => Promise<AppEvent[]>): Promise<Outcome> {
    try {
      const events = await body()
      this.errors.delete(id)
      return { events, error: null }
    } catch (error) {
      this.errors.set(id, message(error))
      this.changed([id])
      return { events: [], error: message(error) }
    }
  }

  /** The composer under a focus view. */
  submit(id: string, text: string): Promise<Outcome> {
    const entity = this.item(id)
    const module = entity && this.moduleFor(id, { type: entity.type })
    if (!entity || !module?.submit || !text.trim()) return Promise.resolve({ events: [], error: null })
    return this.attempt(id, () => module.submit!(entity, text))
  }

  /** Does an action the focus offered, then loads the entity again to show what it did. */
  async perform(id: string, action: string, text: string): Promise<Outcome> {
    const lens = this.lens()
    const entity = this.item(id)
    const module = entity && this.moduleFor(id, { type: entity.type })
    if (!entity || !module?.perform) return { events: [], error: null }
    const offered = module.view.actions?.(entity, lens).find((candidate) => candidate.id === action)
    if (!offered) return this.attempt(id, () => Promise.reject(new Error(`${action} is not available here`)))
    if (offered.disabled) return this.attempt(id, () => Promise.reject(new Error(offered.disabled)))
    const outcome = await this.attempt(id, () => module.perform!(entity, action, text.trim()))
    await this.load({ id, part: 'children', force: true })
    return outcome
  }

  older(id: string): Promise<Outcome> {
    const entity = this.item(id)
    const module = entity && this.moduleFor(id, { type: entity.type })
    if (!entity || !module?.older || !module.view.older?.(entity)) return Promise.resolve({ events: [], error: null })
    return this.attempt(id, async () => {
      await module.older!(id)
      return []
    })
  }

  private write(events: AppEvent[]): Outcome {
    this.owned.write(events)
    return { events, error: null }
  }

  /** The single registry of what a UI may ask for. */
  readonly actions = {
    modules: (): ModuleInfo[] => moduleInfos,
    /** Complete events for entities: the only way the UI reads. */
    scan: ({ ids }: { ids: string[] }): Scan => this.scan(ids),
    load: (request: LoadRequest): Promise<LoadResult> => this.load(request),
    /** What a focus view shows, for a caller with no cache of its own. */
    focus: ({ id }: { id: string }): Focus => this.focus(id),
    perform: ({ id, action, text }: { id: string; action: string; text: string }): Promise<Outcome> =>
      this.perform(id, action, text),
    submit: ({ id, text }: { id: string; text: string }): Promise<Outcome> => this.submit(id, text),
    toggle: ({ id }: { id: string }): Outcome => {
      const entity = this.item(id)
      if (entity?.type !== 'task') return { events: [], error: null }
      return this.write([value(id, 'done', !entity.data.done, this.now(), me)])
    },
    markRead: ({ id }: { id: string }): Promise<Outcome> => this.attempt(id, () => this.slack.markRead(id)),
    /** Loads further back than anything loads on its own. */
    older: ({ id }: { id: string }): Promise<Outcome> => this.older(id),
    /** Bytes of an image a message presented, by the ref it gave. */
    slackImage: ({ ref }: { ref: string }) => this.slack.image(ref),
    link: ({ parent, child }: { parent: string; child: string }): Outcome => this.write([link(parent, child, this.now(), me)]),
    unlink: ({ parent, child }: { parent: string; child: string }): Outcome => {
      const module = this.moduleFor(parent, this.entity(parent).values)
      return this.write(module?.unlink?.(parent, child) ?? [link(parent, child, this.now(), me, 1)])
    },
  }
}

export type Actions = Core['actions']
