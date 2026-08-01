import { atom } from './atom'
import { emptyEntity, rollupEntity, type Entity } from './entity'
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

/** How far along an entity's events (or its derived events) are. */
export type LoadState = 'unloaded' | 'loading' | 'loaded' | 'error'

export interface CachedEntity {
  /** Events read from the source. Complete once `loaded` says so. */
  events: AppEvent[]
  loaded: LoadState
  /** Why the last read failed, so a row can say so rather than sitting blank. */
  error?: string
  /**
   * Events produced by an `events` script rather than read from the source, and
   * never written back to it — the text of a Slack message, the branches on a
   * repo. Kept apart from the real ones so a refetch can replace those without
   * disturbing these.
   */
  derived: AppEvent[]
  /** Whether this entity's own `events` script has been run. */
  derivedState: LoadState
  /**
   * Why that script failed, if it did. Kept apart from {@link error} because the
   * two mean opposite things: a read that failed is a row that can't be trusted,
   * while a script that threw is a row that is simply missing the extra its
   * author hoped for. Conflating them made a bad script look like a bad store.
   */
  derivedError?: string
  /** The events above, rolled up. Derived, but cached — see {@link reconcile}. */
  base: Entity
  /**
   * The entity everything else reads: {@link base} with its type's values laid
   * in behind. Also derived and cached, and recomputed only when one of those
   * two changes, so a row keeps its identity across unrelated updates.
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

const blank = (id: string): CachedEntity => ({
  events: [],
  loaded: 'unloaded',
  derived: [],
  derivedState: 'unloaded',
  base: empty(id),
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
      const state = cache[id]?.loaded ?? 'unloaded'
      return state === 'unloaded' || state === 'loading'
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
 * Bumped whenever everything is invalidated. A response issued before the bump
 * describes the store as it was, so it is dropped rather than written over the
 * newer picture — the entities it covered are marked unloaded, and asked for
 * again by whoever still wants them.
 */
let generation = 0
/** Ids asked for since the last flush. */
const wanted = new Set<string>()
/**
 * Every id anything has ever asked for, as opposed to every id that has arrived.
 * The two differ because a read fetches a couple of layers past what it was asked
 * for, and the overscan is a head start on scrolling rather than a request: an
 * entity that came back that way is cached but unasked-for, and running its
 * `events` script would mean a page reaching out to Slack or GitHub on behalf of
 * rows nobody has looked at. Kept apart from {@link wanted}, which empties on
 * every flush and only ever holds what is still outstanding.
 */
const asked = new Set<string>()
let flushing = false

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

const stateOf = (cache: EntityCache, id: string): LoadState => cache[id]?.loaded ?? 'unloaded'

/**
 * Ask for entities. Idempotent and cheap enough to call on every render: one
 * already loaded, in flight, or known to have failed is not asked for again, and
 * everything asked for within a tick goes out as a single request.
 *
 * Asking is also what lets an entity's `events` script run, so an id already
 * cached is still recorded here even though there is nothing to fetch — that is
 * how an overscanned entity's script starts when a row finally shows it.
 */
export function requestEntities(ids: readonly string[]): void {
  const cache = entitiesAtom.get()
  let added = false
  let newlyAsked = false
  for (const id of ids) {
    if (!asked.has(id)) {
      asked.add(id)
      newlyAsked = true
    }
    if (stateOf(cache, id) !== 'unloaded' || wanted.has(id)) continue
    wanted.add(id)
    added = true
  }
  // Deferred for the same reason the fetch is: this reads during render.
  if (newlyAsked) queueMicrotask(() => startDerivations([...ids]))
  if (!added || flushing) return
  flushing = true
  // Deferred, and not only to batch: reading happens during render, and writing
  // to the atom there would re-enter React mid-render.
  queueMicrotask(flush)
}

function flush(): void {
  flushing = false
  const cache = entitiesAtom.get()
  const ids = [...wanted].filter((id) => stateOf(cache, id) === 'unloaded')
  wanted.clear()
  if (!ids.length) return
  const f = fetcher
  if (!f) return // Asked for before a source was open; the next request retries.

  const issued = generation
  entitiesAtom.set((c) => {
    const next = { ...c }
    for (const id of ids) next[id] = { ...(next[id] ?? blank(id)), loaded: 'loading' }
    return next
  })

  void f(ids)
    .then((scan) => {
      if (issued === generation) receive(ids, scan)
      else abandon(ids)
    })
    .catch((e) => {
      if (issued !== generation) return abandon(ids)
      const failed = message(e)
      update((next) => {
        for (const id of ids) {
          next[id] = { ...(next[id] ?? blank(id)), loaded: 'error', error: failed }
        }
      })
    })
}

/**
 * Give up on a read that the store has moved on from. The entities go back to
 * unloaded rather than staying in flight forever, so whoever still wants them
 * asks again — which, since the thing that invalidated them changed the cache,
 * is about to happen anyway.
 */
function abandon(ids: readonly string[]): void {
  entitiesAtom.set((cache) => {
    const next = { ...cache }
    let any = false
    for (const id of ids) {
      if (next[id]?.loaded !== 'loading') continue
      next[id] = { ...next[id], loaded: 'unloaded' }
      any = true
    }
    return any ? next : cache
  })
}

/**
 * Take a scan into the cache. An entity the scan covers has its events
 * *replaced*: the source hands back everything it holds for that id, so merging
 * would only be a way of keeping something that has since been undone.
 */
function receive(requested: readonly string[], scan: EventScan): void {
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
      next[id] = { ...(next[id] ?? blank(id)), events, loaded: 'loaded', error: undefined }
    }
  })
}

/** Point the cache at a source. What is cached belonged to the previous one. */
export function setEntityFetcher(next: EntityFetcher | null): void {
  fetcher = next
  generation++
  wanted.clear()
  // Nothing has been asked of *this* source yet. Unlike `refreshEntities`, which
  // keeps what the rows still want, everything here belonged to the last one.
  asked.clear()
  entitiesAtom.set({})
}

/**
 * Mark everything as needing reading again, keeping what is cached in the
 * meantime. Called after any write: rows carry on showing the entities they
 * have while the fresh events are on their way, so nothing flickers and nothing
 * has to be worked out about which entities a write could have touched.
 *
 * Derived events are deliberately left alone. They are computed once a session,
 * and re-running a script that reaches out to GitHub on every keystroke would
 * not do.
 */
export function refreshEntities(): void {
  generation++
  wanted.clear()
  entitiesAtom.set((cache) => {
    const next: EntityCache = {}
    for (const [id, entry] of Object.entries(cache)) {
      next[id] = entry.loaded === 'unloaded' ? entry : { ...entry, loaded: 'unloaded' }
    }
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
  // A read already in flight was issued against the store as it was, so its
  // answer would put back what has just been written — or, for a removal, what
  // has just been taken away.
  generation++
  update((next) => {
    for (const [id, added] of byEntity(events)) {
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
  generation++
  const dropped = new Set(events.map(eventKey))
  update((next) => {
    for (const [id] of byEntity(events)) {
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

/** The id of the entity a set of values takes its defaults from, if any. */
function typeIdOf(values: Record<string, unknown>): string | null {
  const typeId = values.type
  return typeof typeId === 'string' && typeId ? typeId : null
}

/**
 * Recompute the derived half of every entry whose inputs changed, and start
 * whatever that leaves worth starting.
 *
 * Two passes, because an entity's values depend on its *type's*: everything is
 * rolled up first, and only then are the defaults laid in. That way the order
 * entries happen to sit in can't decide whether one sees its type's new values
 * or its old ones. Defaults are drawn from the type's own roll-up rather than
 * from its defaulted values, which keeps the dependency exactly one deep — a
 * type that is its own type is then a curiosity rather than a hang.
 *
 * The second pass covers the whole cache rather than only what changed, since a
 * type arriving has to reach every entity that names it. That is one walk of a
 * session's worth of entities per batch of events, not per event.
 */
function reconcile(before: EntityCache, draft: EntityCache): EntityCache {
  const next: EntityCache = {}
  const rerolled = new Set<string>()

  for (const [id, entry] of Object.entries(draft)) {
    const prior = before[id]
    if (prior && prior.events === entry.events && prior.derived === entry.derived) {
      next[id] = entry
      continue
    }
    rerolled.add(id)
    next[id] = { ...entry, base: rollupEntity(id, [...entry.events, ...entry.derived]) }
  }

  /** Types nothing has read, or has read but since invalidated. */
  const staleTypes = new Set<string>()
  /** Entities whose events are in and whose derived events have yet to be. */
  const candidates: string[] = []

  for (const [id, entry] of Object.entries(next)) {
    const typeId = typeIdOf(entry.base.values)
    const defaults = typeId ? next[typeId]?.base.values : undefined
    // Naming a type is what asks for it — nothing else reads one, so without
    // this a type would never load at all, and never reload after a write.
    if (typeId && stateOf(next, typeId) === 'unloaded') staleTypes.add(typeId)

    const prior = before[id]
    const priorTypeId = prior ? typeIdOf(prior.base.values) : null
    const priorDefaults = priorTypeId ? before[priorTypeId]?.base.values : undefined
    // Only the rolled-up entity is recomputed here; everything else about the
    // entry is whatever the change being reconciled left it as.
    if (rerolled.has(id) || !prior || defaults !== priorDefaults) {
      next[id] = { ...entry, entity: withDefaults(entry.base, defaults) }
    }

    // Only what was asked for: see `asked`. An overscanned entity is left alone
    // until something reads it, and picked up by `requestEntities` when it does.
    if (asked.has(id) && next[id].loaded === 'loaded' && next[id].derivedState === 'unloaded') {
      candidates.push(id)
    }
  }

  // Both of these write back here, so neither may run inside the update.
  if (staleTypes.size) queueMicrotask(() => requestEntities([...staleTypes]))
  if (candidates.length) queueMicrotask(() => startDerivations(candidates))

  return same(before, next) ? before : next
}

/** Whether nothing changed after all, so subscribers needn't hear about it. */
function same(before: EntityCache, next: EntityCache): boolean {
  const keys = Object.keys(next)
  if (keys.length !== Object.keys(before).length) return false
  return keys.every((id) => before[id] === next[id])
}

/**
 * An entity with its type's values behind its own. A key the type defines and
 * the entity doesn't is taken from the type — and "doesn't" covers null as well
 * as absent, because the store is append-only and null is the only way to take a
 * value off: an event saying "no longer this" cannot be an event saying "and
 * nothing else either", or a key could never be given back to its default once
 * overridden.
 *
 * So the two ways of having no value of your own mean the same thing, which is
 * what makes clearing a key in the inspector and never writing it indisinguishable
 * from the outside — as they should be.
 */
function withDefaults(base: Entity, defaults: Record<string, unknown> | undefined): Entity {
  if (!defaults) return base
  const values = { ...base.values }
  let changed = false
  for (const [key, value] of Object.entries(defaults)) {
    // A type whose own key is null defines no default, so there is nothing here
    // to lay behind anything — and writing one in would hand back a new object
    // on every reconcile for no change anybody could see.
    if (value == null) continue
    if (values[key] != null) continue
    values[key] = value
    changed = true
  }
  return changed ? { ...base, values } : base
}

// --- Derived events ---------------------------------------------------------

/**
 * Run an entity's `events` script and hand back what it returned. Injected
 * rather than imported: running code needs a worker, and this layer has no
 * business knowing that.
 */
export type CodeEvaluator = (
  entityId: string,
  code: string,
  values: Record<string, unknown>,
) => Promise<unknown>

let evaluator: CodeEvaluator | null = null
export const setCodeEvaluator = (next: CodeEvaluator | null): void => {
  evaluator = next
}

/** The author on an event a script made up, rather than a person writing one. */
const DERIVED_AUTHOR = 'derived'

/**
 * Decide what, if anything, is left to compute for entities whose events have
 * arrived. An entity with no `events` value has nothing to compute and is
 * settled on the spot, which is what keeps this from reconsidering the whole
 * cache every time anything changes.
 *
 * A script may only run once the entity's *type* has loaded, since the script
 * itself can come from the type — so an entity waiting on its type is left for
 * now and picked up when the type lands. The type is asked for as soon as its id
 * is known, so the wait always ends.
 */
function startDerivations(ids: readonly string[]): void {
  const cache = entitiesAtom.get()
  const ready: { id: string; code: string }[] = []
  const settled: string[] = []

  for (const id of ids) {
    const entry = cache[id]
    if (!entry || entry.loaded !== 'loaded' || entry.derivedState !== 'unloaded') continue

    const typeId = typeIdOf(entry.base.values)
    if (typeId && stateOf(cache, typeId) !== 'loaded') {
      requestEntities([typeId])
      continue
    }

    const code = entry.entity.values.events
    if (typeof code === 'string' && code.trim()) ready.push({ id, code })
    else settled.push(id)
  }

  if (settled.length) {
    update((next) => {
      for (const id of settled) next[id] = { ...next[id], derivedState: 'loaded' }
    })
  }
  if (!ready.length) return
  update((next) => {
    for (const { id } of ready) next[id] = { ...next[id], derivedState: 'loading' }
  })
  for (const { id, code } of ready) void derive(id, code)
}

/**
 * Scripts run one at a time. There is a single sandbox behind the evaluator, and
 * a page's worth of entities all reaching out at once is not something to
 * inflict on whatever they are reaching.
 */
let queue: Promise<unknown> = Promise.resolve()

function derive(id: string, code: string): Promise<void> {
  const run = queue.then(async () => {
    const evaluate = evaluator
    const entry = entitiesAtom.get()[id]
    if (!evaluate || !entry) return
    try {
      const events = derivedEvents(id, await evaluate(id, code, entry.entity.values))
      update((next) => {
        // A script may speak for entities other than its own — a repo giving its
        // branches their text — so each event is filed under whichever entity it
        // is about.
        for (const [target, added] of byEntity(events)) {
          const held = next[target] ?? blank(target)
          next[target] = { ...held, derived: [...held.derived, ...added] }
        }
        next[id] = { ...next[id], derivedState: 'loaded' }
      })
    } catch (e) {
      update((next) => {
        next[id] = { ...next[id], derivedState: 'error', derivedError: message(e) }
      })
    }
  })
  queue = run
  return run
}

/**
 * Run every `events` script again. The one affordance for iterating on one:
 * scripts are otherwise computed once a session, so without this the only way to
 * see a change is to reload the app.
 *
 * All of them, not one — a script may write events onto entities other than its
 * own, and nothing records which script put what where, so there is no honest
 * way to undo just one. Clearing the lot and recomputing is both simpler and
 * what you want while you are working on one.
 */
export function refreshDerived(): void {
  entitiesAtom.set((cache) => {
    const draft: EntityCache = {}
    for (const [id, entry] of Object.entries(cache)) {
      draft[id] =
        entry.derived.length === 0 && entry.derivedState === 'unloaded'
          ? entry
          : { ...entry, derived: [], derivedState: 'unloaded', derivedError: undefined }
    }
    return reconcile(cache, draft)
  })
}

/**
 * What a script returned, as events. A bare object counts as one event, and a
 * value event that names no entity is about the entity that produced it — which
 * is the common case by far. The timestamp defaults to 0, so a derived value
 * sorts behind every real edit and can never overwrite one.
 *
 * Anything unrecognisable is dropped rather than thrown over: a script that logs
 * and returns nothing has still done its job.
 */
export function derivedEvents(id: string, returned: unknown): AppEvent[] {
  const list = Array.isArray(returned) ? returned : returned == null ? [] : [returned]
  const out: AppEvent[] = []
  for (const raw of list) {
    if (!raw || typeof raw !== 'object') continue
    const e = raw as Record<string, unknown>
    const base = { timestamp: Number(e.timestamp ?? 0) || 0, author: String(e.author ?? DERIVED_AUTHOR) }
    if (e.type === 'link' || e.sourceId != null || e.destinationId != null) {
      if (e.sourceId == null || e.destinationId == null) continue
      out.push({
        ...base,
        type: 'link',
        sourceId: String(e.sourceId),
        destinationId: String(e.destinationId),
        action: ((Number(e.action ?? 0) || 0) as 0 | 1 | 2 | 3),
      })
      continue
    }
    if (typeof e.key !== 'string') continue
    out.push({
      ...base,
      type: 'value',
      entityId: e.entityId == null ? id : String(e.entityId),
      key: e.key,
      value: e.value ?? null,
    })
  }
  return out
}
