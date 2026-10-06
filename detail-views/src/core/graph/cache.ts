import { emptyEntity, rollupEntity, type GraphEntity } from './entity.ts'
import { byEntity, eventKey, type AppEvent, type Changed, type Scan } from './events.ts'
import type { GetEntities } from './walk.ts'
import { loadedKey, type Freshness, type LoadPart, type LoadRequest, type LoadResult } from '../types.ts'

// The entity cache: every event the UI has read, kept per entity, and what
// each entity rolls up to. Runtime only, never persisted. Shared with the
// renderer, and plain enough to run in node against a `Core` with no UI.
//
// It exists so that showing something never waits on anything. Reads come out
// of here synchronously and are always answered — with an empty entity if
// nothing is known yet — while the events are fetched behind and whatever
// reads them recomputes when they land. Asking is what fetches: there is no
// separate "load" to remember.
//
// It is also what reaches out to other services. Once an entity that was asked
// for has arrived, its module says which parts of it come from elsewhere
// (`Freshness`); a part never loaded, or loaded longer ago than it stays fresh,
// is loaded by the core into the cache store, which says so on the entity
// itself (`loaded.<part>`), and the change comes back here like any other.
// `children` is only loaded for an entity something has walked into
// (`expand`): a conversation shown as a row needs its unread count, not its
// history.
//
// Invalidation is by name and takes nothing away: an entry marked for reading
// again keeps its events (`stale`) so nothing on screen empties and refills.

export type LoadState = 'unloaded' | 'loading' | 'loaded' | 'stale' | 'error'

export interface CachedEntity {
  events: AppEvent[]
  loaded: LoadState
  error?: string
  /** The events rolled up; recomputed only when they change, so it keeps its identity otherwise. */
  entity: GraphEntity
}

export interface CacheState {
  entries: Record<string, CachedEntity>
  /** Ids with a load from another service in flight. */
  loading: Record<string, number>
  /** The last thing that went wrong for an id: a load, or something done to it. */
  errors: Record<string, string>
}

/** A read of the cache at one moment. Reading is asking. */
export interface Source {
  get: GetEntities
  /** True while an entity's events are on their way. */
  pending(id: string): boolean
  /** True while something about it is being loaded from another service. */
  loading(id: string): boolean
  error(id: string): string | null
  /** Says the walk went into this entity's children, so they are worth loading. */
  expand(id: string): void
}

export interface CacheDeps {
  scan(ids: string[]): Promise<Scan>
  load(request: LoadRequest): Promise<LoadResult>
  /** What of an entity comes from another service, by its module. */
  foreign(entity: GraphEntity): Freshness | null
  now?(): number
}

const waiting = (state: LoadState): boolean => state === 'unloaded' || state === 'loading'
const complete = (state: LoadState): boolean => state === 'loaded' || state === 'stale'
const unread = (state: LoadState): boolean => state === 'unloaded' || state === 'stale'
const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

const blank = (id: string): CachedEntity => ({ events: [], loaded: 'unloaded', entity: emptyEntity(id) })

export class EntityCache {
  private state: CacheState = { entries: {}, loading: {}, errors: {} }
  private readonly listeners = new Set<() => void>()
  private readonly deps: CacheDeps
  private readonly now: () => number

  /** Ids asked for since the last flush. */
  private readonly wanted = new Set<string>()
  /**
   * Every id anything has read, as opposed to every id that arrived: a scan
   * reads a layer past what was asked, and that head start is not a reason to
   * reach out to Slack for rows nobody has looked at.
   */
  private readonly asked = new Set<string>()
  /** Ids whose children something has walked into. */
  private readonly expanded = new Set<string>()
  /** `id part` → when a load was last started, so a reply racing its own change isn't loaded twice. */
  private readonly attempted = new Map<string, number>()
  private readonly inFlight = new Set<string>()
  private flushing = false
  /** A write counter, and the count each entity was last written at: a read issued before is not believed about it. */
  private writes = 0
  private readonly writtenAt = new Map<string, number>()
  private readonly outstanding = new Set<Promise<unknown>>()
  private readonly empties = new Map<string, GraphEntity>()

  constructor(deps: CacheDeps) {
    this.deps = deps
    this.now = deps.now ?? Date.now
  }

  get = (): CacheState => this.state

  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  private set(next: CacheState): void {
    if (next === this.state) return
    this.state = next
    for (const listener of this.listeners) listener()
  }

  private empty(id: string): GraphEntity {
    let made = this.empties.get(id)
    if (!made) this.empties.set(id, (made = emptyEntity(id)))
    return made
  }

  /** A read against the cache as it stands now. */
  source(state: CacheState = this.state): Source {
    return {
      get: (ids) => {
        this.request(ids)
        const out: Record<string, GraphEntity> = {}
        for (const id of ids) out[id] = state.entries[id]?.entity ?? this.empty(id)
        return out
      },
      pending: (id) => waiting(state.entries[id]?.loaded ?? 'unloaded'),
      loading: (id) => (state.loading[id] ?? 0) > 0,
      error: (id) => state.errors[id] ?? state.entries[id]?.error ?? null,
      expand: (id) => this.expand(id),
    }
  }

  // --- Reading ---------------------------------------------------------------

  /**
   * Ask for entities. Cheap enough to call on every read: anything loaded or
   * in flight is skipped, and a tick's worth of asking is one scan. Deferred,
   * so a read during a render never writes mid-render.
   */
  request(ids: readonly string[]): void {
    let fresh = false
    const newlyAsked: string[] = []
    for (const id of ids) {
      if (!this.asked.has(id)) {
        this.asked.add(id)
        newlyAsked.push(id)
      }
      if (!unread(this.state.entries[id]?.loaded ?? 'unloaded') || this.wanted.has(id)) continue
      this.wanted.add(id)
      fresh = true
    }
    if (newlyAsked.length) queueMicrotask(() => this.considerLoads(newlyAsked))
    if (!fresh || this.flushing) return
    this.flushing = true
    queueMicrotask(() => this.flush())
  }

  private flush(): void {
    this.flushing = false
    const ids = [...this.wanted].filter((id) => unread(this.state.entries[id]?.loaded ?? 'unloaded'))
    this.wanted.clear()
    if (!ids.length) return
    const issued = this.writes
    this.update((next) => {
      for (const id of ids) next[id] = { ...(next[id] ?? blank(id)), loaded: 'loading' }
    })
    this.track(
      this.deps.scan(ids).then(
        (scan) => this.receive(ids, scan, issued),
        (e) =>
          this.update((next) => {
            for (const id of ids) next[id] = { ...(next[id] ?? blank(id)), loaded: 'error', error: message(e) }
          }),
      ),
    )
  }

  /** A scan replaces what each covered entity held, unless it was written to since the scan went out. */
  private receive(requested: readonly string[], scan: Scan, issued: number): void {
    const buckets = new Map<string, AppEvent[]>()
    for (const id of [...scan.entityIds, ...requested]) buckets.set(id, [])
    for (const [id, events] of byEntity(scan.events)) buckets.get(id)?.push(...events)
    this.update((next) => {
      for (const [id, events] of buckets) {
        const entry = next[id] ?? blank(id)
        if ((this.writtenAt.get(id) ?? 0) > issued) {
          next[id] = { ...entry, loaded: entry.events.length ? 'stale' : 'unloaded' }
          continue
        }
        next[id] = { ...entry, events, loaded: 'loaded', error: undefined }
      }
    })
  }

  // --- Invalidating and writing through -------------------------------------

  private noteWrites(ids: Iterable<string>): void {
    this.writes++
    for (const id of ids) this.writtenAt.set(id, this.writes)
  }

  /**
   * Mark entities for reading again, or everything for `null` (the cache store
   * was cleared, say). They keep what they have meanwhile, and only what is
   * read again is fetched.
   */
  invalidate(changed: Changed): void {
    const ids = changed ?? Object.keys(this.state.entries)
    if (!ids.length) return
    this.noteWrites(ids)
    const entries = { ...this.state.entries }
    let any = false
    for (const id of ids) {
      const entry = entries[id]
      if (!entry) continue
      if (entry.loaded === 'loaded') entries[id] = { ...entry, loaded: 'stale' }
      else if (entry.loaded === 'error') entries[id] = { ...entry, loaded: 'unloaded', error: undefined }
      else continue
      any = true
    }
    if (changed === null) this.attempted.clear()
    // Nothing is fetched here: whatever still shows these reads them again
    // when it hears of the change, and only that is fetched.
    if (any) this.set({ ...this.state, entries })
  }

  /** Put events in as though read: a write the core has made, shown before the round trip. */
  apply(events: readonly AppEvent[]): void {
    if (!events.length) return
    const touched = byEntity(events)
    this.noteWrites(touched.keys())
    this.update((next) => {
      for (const [id, added] of touched) {
        const entry = next[id] ?? blank(id)
        next[id] = { ...entry, events: [...entry.events, ...added] }
      }
    })
  }

  /** Take events back out, matched by content. */
  remove(events: readonly AppEvent[]): void {
    if (!events.length) return
    const touched = byEntity(events)
    this.noteWrites(touched.keys())
    const dropped = new Set(events.map(eventKey))
    this.update((next) => {
      for (const [id] of touched) {
        const entry = next[id]
        if (entry) next[id] = { ...entry, events: entry.events.filter((e) => !dropped.has(eventKey(e))) }
      }
    })
  }

  /** Something done to an entity failed (or, with null, worked). */
  setError(id: string, error: string | null): void {
    const errors = { ...this.state.errors }
    if (error === null) {
      if (!(id in errors)) return
      delete errors[id]
    } else errors[id] = error
    this.set({ ...this.state, errors })
  }

  // --- Loading from other services -------------------------------------------

  /** The walk went into this entity's children: load them if they come from elsewhere. */
  expand(id: string): void {
    if (this.expanded.has(id)) return
    this.expanded.add(id)
    queueMicrotask(() => this.considerLoads([id]))
  }

  /**
   * Look again at everything read this session, or at `ids`, loading whatever
   * has gone stale since. Not only what is on screen: anything looked at stays
   * current while the app runs.
   */
  revisit(ids: readonly string[] = [...this.asked]): void {
    this.considerLoads(ids)
  }

  /** Load every part of an entity again now, fresh or not. */
  refresh(id: string): void {
    const entry = this.state.entries[id]
    const parts = this.deps.foreign(entry?.entity ?? this.empty(id)) ?? {}
    for (const part of Object.keys(parts) as LoadPart[]) {
      if (part === 'children' && !this.expanded.has(id)) continue
      this.startLoad(id, part, true)
    }
  }

  private considerLoads(ids: readonly string[]): void {
    const now = this.now()
    for (const id of ids) {
      const entry = this.state.entries[id]
      if (!entry || !complete(entry.loaded) || !this.asked.has(id)) continue
      const parts = this.deps.foreign(entry.entity)
      if (!parts) continue
      for (const [part, fresh] of Object.entries(parts) as [LoadPart, number][]) {
        if (part === 'children' && !this.expanded.has(id)) continue
        const key = `${id} ${part}`
        if (this.inFlight.has(key)) continue
        const loadedAt = Number(entry.entity.values[loadedKey(part)]) || 0
        if (now - Math.max(loadedAt, this.attempted.get(key) ?? 0) < fresh) continue
        this.startLoad(id, part, false)
      }
    }
  }

  private startLoad(id: string, part: LoadPart, force: boolean): void {
    const key = `${id} ${part}`
    if (this.inFlight.has(key)) return
    this.inFlight.add(key)
    this.attempted.set(key, this.now())
    this.set({ ...this.state, loading: { ...this.state.loading, [id]: (this.state.loading[id] ?? 0) + 1 } })
    const settle = (error: string | null) => {
      this.inFlight.delete(key)
      const loading = { ...this.state.loading }
      if ((loading[id] ?? 0) > 1) loading[id] -= 1
      else delete loading[id]
      const errors = { ...this.state.errors }
      if (error) errors[id] = error
      else delete errors[id]
      this.set({ ...this.state, loading, errors })
      // The core says what changed too, but not before this reply: read the
      // entity again now so its new `loaded.<part>` is seen before it is checked.
      this.invalidate([id])
    }
    this.track(
      this.deps.load({ id, part, force }).then(
        (result) => settle(result.error),
        (e) => settle(message(e)),
      ),
    )
  }

  // --- Rolling up -------------------------------------------------------------

  /** Every change to entries goes through here, so a rolled-up entity never lags its events. */
  private update(mutate: (draft: Record<string, CachedEntity>) => void): void {
    const before = this.state.entries
    const draft = { ...before }
    mutate(draft)
    const changed: string[] = []
    for (const [id, entry] of Object.entries(draft)) {
      const prior = before[id]
      if (prior === entry) continue
      if (prior && prior.events === entry.events) {
        draft[id] = { ...entry, entity: prior.entity }
        if (complete(entry.loaded) && !complete(prior.loaded)) changed.push(id)
        continue
      }
      draft[id] = { ...entry, entity: rollupEntity(id, entry.events) }
      changed.push(id)
    }
    this.set({ ...this.state, entries: draft })
    if (changed.length) queueMicrotask(() => this.considerLoads(changed))
  }

  // --- Waiting, for a caller with no screen ----------------------------------

  private track(promise: Promise<unknown>): void {
    this.outstanding.add(promise)
    void promise.finally(() => this.outstanding.delete(promise))
  }

  /** Resolves once nothing is being fetched or loaded. */
  async idle(): Promise<void> {
    for (;;) {
      // Let queued microtasks start whatever they are going to start.
      for (let i = 0; i < 5; i++) await Promise.resolve()
      if (!this.outstanding.size && !this.flushing) return
      await Promise.allSettled([...this.outstanding])
    }
  }
}
