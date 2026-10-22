import { atom } from './atom'
import { emptyEntity, rollupEntity, type Entity } from './entity'
import { lazyEvents } from './builtins'
import type { AppEvent } from './events'
import type { EntitySource } from './query'

// The entity cache: every event the app has read, kept per entity, and the
// entity each set of events rolls up to. Runtime only and never persisted.
//
// It exists so that showing something never waits on the network. Reads come out
// of here synchronously and are always answered — with an empty entity if
// nothing is known yet — while the events are fetched in the background and the
// rows recompute when they land. The same cache is what makes an edit in one tab
// show up in another: there is one copy of each entity, not one per frame.
//
// Nothing is ever evicted. A session's worth of entities is small, and dropping
// one would only mean fetching it again.
//
// Invalidation is by name, not wholesale. A write knows what it touched — the
// events it is making, or failing that the ids it changed — and that is all that
// is marked for re-reading, because everything else in here is as true as it was
// a moment ago. The wholesale version (`refreshEntities`) is kept for the one
// case that has nothing to go on: a change made somewhere this client cannot see.
//
// Nor does invalidation take anything away. An entry marked for re-reading keeps
// its events and goes to `stale`, so a row goes on showing what it has instead of
// emptying out and filling again. Doing it the other way — putting entries back
// to `unloaded` after every write — is what had every row on screen flash its
// loading state on every keystroke.

/**
 * How far along an entity's events are.
 *
 * `stale` is the one that isn't a step along that road: it means read in full
 * once and worth reading again — the store may have moved on. It is deliberately
 * *not* a kind of waiting, because an entity that is entirely here does not
 * become a row with nothing in it just because something might have changed
 * behind it. That distinction is the whole reason it exists: invalidating by
 * putting entries back to `unloaded` made every row on screen flash its loading
 * state on every keystroke.
 */
export type LoadState = 'unloaded' | 'loading' | 'loaded' | 'stale' | 'error'

export interface CachedEntity {
  /** Events read from the source. Complete once `loaded` says so. */
  events: AppEvent[]
  loaded: LoadState
  /** Why the last read failed, so a row can say so rather than sitting blank. */
  error?: string
  /**
   * Events an `onLoad` function returned rather than read from the source, and
   * never written back to it. Kept apart from the real ones so a refetch can
   * replace those without disturbing these. See `../builtins`.
   */
  lazy?: AppEvent[]
  /** Whether this entity's type has had its `onLoad` run for it. Absent: not yet. */
  lazyState?: LoadState
  /** Why that function failed, if it did. Apart from {@link error}: the read was fine. */
  lazyError?: string
  /**
   * The events above, rolled up — the entity everything else reads. Derived, but
   * cached and recomputed only when the events it came from change, so a row
   * keeps its identity across unrelated updates.
   */
  entity: Entity
}

export type EntityCache = Record<string, CachedEntity>

export const entitiesAtom = atom<EntityCache>({})

// Memoised, so that asking twice for an entity that isn't there hands back the
// same object and the rows built from it compare equal.
const empties = new Map<string, Entity>()
const empty = (id: string): Entity => {
  const known = empties.get(id)
  if (known) return known
  const made = emptyEntity(id)
  empties.set(id, made)
  return made
}

/** Whether a load state is one that something is still expected to come out of. */
const waiting = (state: LoadState): boolean => state === 'unloaded' || state === 'loading'

/** Whether the events are all here, whether or not they are known to be current. */
const complete = (state: LoadState): boolean => state === 'loaded' || state === 'stale'

/** Whether a read is owed: never done, or done and since invalidated. */
const unread = (state: LoadState): boolean => state === 'unloaded' || state === 'stale'

const blank = (id: string): CachedEntity => ({
  events: [],
  loaded: 'unloaded',
  entity: empty(id),
})

// --- Reading ----------------------------------------------------------------

/**
 * Read against a cache snapshot, asking for anything missing. The request is a
 * side effect of *reading*, which is what makes the whole thing work: a caller
 * says what it wants to show and is answered immediately, and the answer
 * improves. The fetch itself is deferred to a microtask, so reading during a
 * render never writes to the atom mid-render.
 */
export function entitiesFrom(cache: EntityCache): EntitySource {
  return {
    get: (ids) => {
      requestEntities(ids)
      const out: Record<string, Entity> = {}
      for (const id of ids) out[id] = cache[id]?.entity ?? empty(id)
      return out
    },
    pending: (id) => {
      const entry = cache[id]
      if (!entry) return true
      return waiting(entry.loaded)
    },
    error: (id) => cache[id]?.error ?? null,
  }
}

/** The live cache, for callers outside React — tools, and the call context. */
export const entities = (): EntitySource => entitiesFrom(entitiesAtom.get())

/** One entity as it currently stands, asking for it if it isn't here yet. */
export const getEntity = (id: string): Entity => entities().get([id])[id]

// --- Fetching ---------------------------------------------------------------

/** What a scan of the source hands back: complete events for a set of ids. */
export interface EventScan {
  entityIds: string[]
  events: AppEvent[]
}

export type EntityFetcher = (entityIds: string[]) => Promise<EventScan>

let fetcher: EntityFetcher | null = null
/**
 * Which source the cache belongs to. Bumped when that changes, since everything
 * held then belonged to the last one and no answer about it is worth having.
 */
let generation = 0
/**
 * A counter over writes, and the count each entity was last written at.
 *
 * A read is issued against the store as it stood, so an entity written to since
 * is one whose answer would put back what has just been changed — and it is only
 * *that* entity's answer that is wrong. Guarding per entity rather than throwing
 * the whole response away is what keeps one keystroke from discarding a read of
 * everything else on screen.
 */
let writes = 0
const writtenAt = new Map<string, number>()

/** Note a write, so no read issued before it is believed about these entities. */
function noteWrites(ids: Iterable<string>): void {
  writes++
  for (const id of ids) writtenAt.set(id, writes)
}

/** Whether a read issued at `at` can still be believed about this entity. */
const trustworthy = (id: string, at: number): boolean => (writtenAt.get(id) ?? 0) <= at

/** Ids asked for since the last flush. */
const wanted = new Set<string>()
let flushing = false

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

const stateOf = (cache: EntityCache, id: string): LoadState => cache[id]?.loaded ?? 'unloaded'

/**
 * Ask for entities. Idempotent and cheap enough to call on every render: one
 * already loaded, in flight, or known to have failed is not asked for again, and
 * everything asked for within a tick goes out as a single request.
 */
export function requestEntities(ids: readonly string[]): void {
  const cache = entitiesAtom.get()
  let added = false
  for (const id of ids) {
    if (!unread(stateOf(cache, id)) || wanted.has(id)) continue
    wanted.add(id)
    added = true
  }
  if (!added || flushing) return
  flushing = true
  // Deferred, and not only to batch: reading happens during render, and writing
  // to the atom there would re-enter React mid-render.
  queueMicrotask(flush)
}

function flush(): void {
  flushing = false
  const cache = entitiesAtom.get()
  const ids = [...wanted].filter((id) => unread(stateOf(cache, id)))
  wanted.clear()
  if (!ids.length) return
  const f = fetcher
  if (!f) return // Asked for before a source was open; the next request retries.

  const source = generation
  const issued = writes
  entitiesAtom.set((c) => {
    const next = { ...c }
    for (const id of ids) next[id] = { ...(next[id] ?? blank(id)), loaded: 'loading' }
    return next
  })

  void f(ids)
    .then((scan) => {
      if (source === generation) receive(ids, scan, issued)
      else abandon(ids)
    })
    .catch((e) => {
      if (source !== generation) return abandon(ids)
      const failed = message(e)
      update((next) => {
        for (const id of ids) {
          next[id] = { ...(next[id] ?? blank(id)), loaded: 'error', error: failed }
        }
      })
    })
}

/**
 * Give up on a read the cache has moved on from, so whoever still wants those
 * entities asks again. One that has its events keeps them and goes back to
 * `stale` — the answer is unwanted, not the events already in hand.
 */
function abandon(ids: readonly string[]): void {
  entitiesAtom.set((cache) => {
    const next = { ...cache }
    let any = false
    for (const id of ids) {
      const entry = next[id]
      if (entry?.loaded !== 'loading') continue
      next[id] = { ...entry, loaded: entry.events.length ? 'stale' : 'unloaded' }
      any = true
    }
    return any ? next : cache
  })
}

/**
 * Take a scan into the cache. An entity the scan covers has its events
 * *replaced*: the source hands back everything it holds for that id, so merging
 * would only be a way of keeping something that has since been undone.
 *
 * `issued` is the write count the read went out at, and an entity written to
 * since is left exactly as it stands: the answer predates the write, so taking it
 * would undo what the user has already been shown. That entity alone is put back
 * to needing a read; the rest of the scan is perfectly good.
 */
function receive(requested: readonly string[], scan: EventScan, issued: number): void {
  const buckets = new Map<string, AppEvent[]>()
  for (const id of scan.entityIds) buckets.set(id, [])
  // An id that was asked for but isn't in the scan has no events at all — an
  // entity nothing has been written to yet. That is a complete answer, not a
  // missing one.
  for (const id of requested) if (!buckets.has(id)) buckets.set(id, [])

  for (const e of scan.events) {
    if (e.type === 'value') buckets.get(e.entityId)?.push(e)
    else {
      buckets.get(e.sourceId)?.push(e)
      if (e.destinationId !== e.sourceId) buckets.get(e.destinationId)?.push(e)
    }
  }

  update((next) => {
    for (const [id, events] of buckets) {
      const entry = next[id] ?? blank(id)
      if (!trustworthy(id, issued)) {
        next[id] = { ...entry, loaded: entry.events.length ? 'stale' : 'unloaded' }
        continue
      }
      next[id] = { ...entry, events, loaded: 'loaded', error: undefined }
    }
  })
}

/** Point the cache at a source. What is cached belonged to the previous one. */
export function setEntityFetcher(next: EntityFetcher | null): void {
  fetcher = next
  generation++
  wanted.clear()
  writtenAt.clear()
  // Nothing has been asked of *this* source yet. Unlike `refreshEntities`, which
  // keeps what the rows still want, everything here belonged to the last one.
  entitiesAtom.set({})
}

/**
 * Mark named entities as needing reading again, keeping what is cached in the
 * meantime — the rows carry on showing the events they have while the fresh ones
 * are on their way.
 *
 * This is for a write the client cannot state: `createEntity` mints its id on the
 * server, so unlike an edit there are no events to apply here. Naming the
 * entities it touched is the next best thing, and is the whole difference between
 * re-reading three rows and re-reading the screen.
 */
export function invalidateEntities(ids: readonly string[]): void {
  if (!ids.length) return
  noteWrites(ids)
  entitiesAtom.set((cache) => {
    const next = { ...cache }
    let any = false
    for (const id of ids) {
      const entry = next[id]
      const marked = entry && invalidate(entry)
      if (!marked) continue
      next[id] = marked
      any = true
    }
    return any ? next : cache
  })
}

/**
 * An entry marked as owing a read, or null where it already owes one. What it has
 * it keeps: `stale` is complete-but-perhaps-old, which is what a row should go on
 * showing. An entry still `loading` is left alone — {@link receive} decides what
 * to make of its answer when it lands, since by then it knows whether the write
 * beat it.
 */
function invalidate(entry: CachedEntity): CachedEntity | null {
  if (complete(entry.loaded)) return entry.loaded === 'stale' ? null : { ...entry, loaded: 'stale' }
  // A read that failed is worth trying again: the store has changed under it.
  if (entry.loaded === 'error') return { ...entry, loaded: 'unloaded', error: undefined }
  return null
}

/**
 * Mark everything as needing reading again. Reserved for changes nothing here
 * saw at all — an agent writing notes over MCP, or the inspector writing
 * events of its own — since a write made through `source/entity` says what it
 * changed and only that needs re-reading.
 *
 * Entries keep their events and go to `stale` rather than `unloaded`, so no row
 * turns back into its loading state over this.
 */
export function refreshEntities(): void {
  const cache = entitiesAtom.get()
  noteWrites(Object.keys(cache))
  entitiesAtom.set(() => {
    const next: EntityCache = {}
    for (const [id, entry] of Object.entries(cache)) next[id] = invalidate(entry) ?? entry
    return next
  })
}

// --- Writing through --------------------------------------------------------

/**
 * Put events into the cache as though they had been read. This is how an edit
 * shows up before it has been persisted: the client knows exactly what it is
 * about to write — down to the timestamp and the author — so it can apply it
 * here and let the round trip happen behind the change the user already sees.
 */
export function applyEvents(events: readonly AppEvent[]): void {
  if (!events.length) return
  const touched = byEntity(events)
  // A read already in flight was issued against the store as it was, so its
  // answer would put back what has just been written — or, for a removal, what
  // has just been taken away. Only for these entities, though: the rest of that
  // answer is as good as it ever was.
  noteWrites(touched.keys())
  update((next) => {
    for (const [id, added] of touched) {
      // Including entities nothing has read yet, which is how a client that makes
      // up its own ids — the phone, adding a line — shows the new entity before
      // the write lands. The entry stays unloaded, so the first thing to ask for
      // it reads the rest of its history along with these.
      const entry = next[id] ?? blank(id)
      next[id] = { ...entry, events: [...entry.events, ...added] }
    }
  })
}

/**
 * Take events back out — what undo does, since undo deletes at the store rather
 * than compensating. Matched by content rather than by identity: the events come
 * back over the wire, so they are equal to the cached ones without being them.
 */
export function removeEvents(events: readonly AppEvent[]): void {
  if (!events.length) return
  const touched = byEntity(events)
  noteWrites(touched.keys())
  const dropped = new Set(events.map(eventKey))
  update((next) => {
    for (const [id] of touched) {
      const entry = next[id]
      if (!entry) continue
      const kept = entry.events.filter((e) => !dropped.has(eventKey(e)))
      if (kept.length !== entry.events.length) next[id] = { ...entry, events: kept }
    }
  })
}

/** Which entities an event belongs to: one for a value, both ends for a link. */
function byEntity(events: readonly AppEvent[]): Map<string, AppEvent[]> {
  const out = new Map<string, AppEvent[]>()
  const push = (id: string, e: AppEvent): void => {
    const list = out.get(id)
    if (list) list.push(e)
    else out.set(id, [e])
  }
  for (const e of events) {
    if (e.type === 'value') push(e.entityId, e)
    else {
      push(e.sourceId, e)
      if (e.destinationId !== e.sourceId) push(e.destinationId, e)
    }
  }
  return out
}

/** Events carry no id, so equality is what identifies one — as on the source. */
const eventKey = (e: AppEvent): string =>
  e.type === 'value'
    ? ['v', e.entityId, e.key, e.timestamp, e.author, JSON.stringify(e.value ?? null)].join(' ')
    : ['l', e.sourceId, e.destinationId, e.action, e.timestamp, e.author].join(' ')

// --- Rolling up -------------------------------------------------------------

/**
 * Apply a change and bring everything it affected up to date. Every write to the
 * cache goes through here, so a rolled-up entity can never be out of step with
 * the events it came from.
 */
function update(mutate: (draft: EntityCache) => void): void {
  entitiesAtom.set((cache) => {
    const draft = { ...cache }
    mutate(draft)
    return reconcile(cache, draft)
  })
}

/** Roll up every entry whose events changed; the rest keep their identity. */
function reconcile(before: EntityCache, draft: EntityCache): EntityCache {
  const next: EntityCache = {}

  for (const [id, entry] of Object.entries(draft)) {
    const prior = before[id]
    if (prior && prior.events === entry.events && prior.lazy === entry.lazy) {
      next[id] = entry
      continue
    }
    // Only the rolled-up entity is recomputed here; everything else about the
    // entry is whatever the change being reconciled left it as.
    next[id] = { ...entry, entity: rollupEntity(id, [...(entry.lazy ?? []), ...entry.events]) }
  }

  // Naming a type is what asks for it — nothing else reads one — and an entity
  // whose events are in is what may have an `onLoad` to run. Both write back
  // here, so neither may run inside the update.
  const types = new Set<string>()
  const ready: string[] = []
  for (const [id, entry] of Object.entries(next)) {
    const typeId = typeIdOf(entry.entity.values)
    if (typeId && unread(stateOf(next, typeId))) types.add(typeId)
    if (complete(entry.loaded) && !entry.lazyState) ready.push(id)
  }
  if (types.size) queueMicrotask(() => requestEntities([...types]))
  if (ready.length) queueMicrotask(() => startLazy(ready))

  return same(before, next) ? before : next
}

/** The id of the entity a set of values names as its type, if any. */
function typeIdOf(values: Record<string, unknown>): string | null {
  const typeId = values.type
  return typeof typeId === 'string' && typeId ? typeId : null
}

// --- Lazy events ------------------------------------------------------------

/**
 * Run a type's `onLoad` for one of its instances and hand back what it returned.
 * Injected rather than imported: running code needs a worker, and this layer has
 * no business knowing that.
 */
export type OnLoadRunner = (
  entityId: string,
  code: string,
  values: Record<string, unknown>,
) => Promise<unknown>

let runner: OnLoadRunner | null = null
export const setOnLoadRunner = (next: OnLoadRunner | null): void => {
  runner = next
}

/**
 * Decide what is left to run for entities whose events have arrived. The
 * function is the *type's*: an entity of no type, or of a type with no `onLoad`,
 * is settled on the spot. One waiting on its type is left until the type lands,
 * and the type is asked for as soon as its id is known, so the wait ends.
 */
function startLazy(ids: readonly string[]): void {
  if (!runner) return
  const cache = entitiesAtom.get()
  const run: { id: string; code: string }[] = []
  const settled: string[] = []
  for (const id of ids) {
    const entry = cache[id]
    if (!entry || !complete(entry.loaded) || entry.lazyState) continue
    const typeId = typeIdOf(entry.entity.values)
    if (!typeId) {
      settled.push(id)
      continue
    }
    if (!complete(stateOf(cache, typeId))) continue
    const code = cache[typeId]?.entity.values.onLoad
    if (typeof code === 'string' && code.trim()) run.push({ id, code })
    else settled.push(id)
  }
  if (!settled.length && !run.length) return
  update((next) => {
    for (const id of settled) next[id] = { ...next[id], lazyState: 'loaded' }
    for (const { id } of run) next[id] = { ...next[id], lazyState: 'loading' }
  })
  for (const { id, code } of run) void runLazy(id, code)
}

/** One at a time: a page of entities all reaching out at once is not kind to anything. */
let lazyQueue: Promise<unknown> = Promise.resolve()

function runLazy(id: string, code: string): Promise<void> {
  const at = generation
  const run = lazyQueue.then(async () => {
    const go = runner
    const entry = entitiesAtom.get()[id]
    if (!go || !entry || at !== generation) return
    try {
      const events = lazyEvents(id, await go(id, code, entry.entity.values))
      if (at !== generation) return
      update((next) => {
        // A function may speak for entities other than its own, so each event is
        // filed under whichever entity it is about.
        for (const [target, added] of byEntity(events)) {
          const held = next[target] ?? blank(target)
          next[target] = { ...held, lazy: [...(held.lazy ?? []), ...added] }
        }
        next[id] = { ...next[id], lazyState: 'loaded' }
      })
    } catch (e) {
      if (at !== generation) return
      update((next) => {
        next[id] = { ...next[id], lazyState: 'error', lazyError: message(e) }
      })
    }
  })
  lazyQueue = run
  return run
}

/** Whether nothing changed after all, so subscribers needn't hear about it. */
function same(before: EntityCache, next: EntityCache): boolean {
  const keys = Object.keys(next)
  if (keys.length !== Object.keys(before).length) return false
  return keys.every((id) => before[id] === next[id])
}
