// The entity cache as plain data: what the browser knows of each entity, and
// the pure functions that change it. `Egdb` (store.ts) keeps one of these in a
// Jotai atom.
//
// The cache has two parts:
//
// - `base`: what the server said, entity by entity.
// - `pending`: writes sent to the server and not yet confirmed.
//
// Reads go through a view: `base` with the pending writes applied on top. A
// write therefore shows at once, and a write that the server refuses goes away
// again with no extra work: its entry leaves `pending`.

import { matches } from './filter.js'
import type {
  Direction,
  EntityId,
  EventRef,
  Filter,
  Follow,
  FollowRule,
  LinkGroup,
  Op,
  Values,
  WalkCache,
} from './types.js'

export const TYPE_KEY = 'type'

/** A link group's key in `EntityRecord.links`: link type and direction. */
export type GroupKey = string

export function groupKey(linkType: string, direction: Direction): GroupKey {
  return `${linkType}\u0000${direction}`
}

/** A link as the cache keeps it. `event` is null while a local write is unconfirmed. */
export interface CachedLink {
  source: EntityId
  destination: EntityId
  link_type: string
  sort: number
  event: EventRef | null
}

export interface EntityRecord {
  readonly id: EntityId
  /** All non-null values; `undefined` while they are not loaded. */
  readonly values?: Values
  /** The link groups that are loaded, sorted. An absent group is not loaded. */
  readonly links: Readonly<Record<GroupKey, readonly CachedLink[]>>
  /**
   * Made by a local write to an entity that the cache did not know: a new
   * entity, as far as the browser can tell. Its absent link groups count as
   * empty rather than not loaded.
   */
  readonly local?: boolean
}

export interface PendingWrite {
  readonly id: number
  readonly ops: readonly Op[]
  /** The local clock when the write was made: the sort of `timestamp` links. */
  readonly timestamp: number
}

export interface CacheState {
  readonly base: ReadonlyMap<EntityId, EntityRecord>
  readonly pending: readonly PendingWrite[]
}

export const emptyCache: CacheState = { base: new Map(), pending: [] }

/** A read-only view of the cache: `base` with the pending writes on top. */
export interface EntityView {
  get(id: EntityId): EntityRecord | undefined
}

export function viewOf(state: CacheState): EntityView {
  if (state.pending.length === 0) return state.base
  const overlay = new Map<EntityId, EntityRecord>()
  const read = (id: EntityId) => overlay.get(id) ?? state.base.get(id)
  for (const write of state.pending) {
    for (const op of write.ops) applyOp(read, (r) => overlay.set(r.id, r), op, write.timestamp)
  }
  return { get: read }
}

/** Whether a record's entity is visible through links: it has a type. */
export function isTyped(values: Values): boolean {
  return values[TYPE_KEY] !== undefined && values[TYPE_KEY] !== null
}

/** The entities that a batch of ops writes to. */
export function entitiesOf(ops: readonly Op[]): EntityId[] {
  const ids = new Set<EntityId>()
  for (const op of ops) {
    if ('set' in op) ids.add(op.set.entity)
    else {
      ids.add(op.link.source)
      ids.add(op.link.destination)
    }
  }
  return [...ids]
}

/** The link groups that a batch of ops writes to, per entity. */
export function groupsOf(ops: readonly Op[]): Map<EntityId, Set<GroupKey>> {
  const out = new Map<EntityId, Set<GroupKey>>()
  const add = (id: EntityId, key: GroupKey) => {
    if (!out.has(id)) out.set(id, new Set())
    out.get(id)!.add(key)
  }
  for (const op of ops) {
    if ('link' in op) {
      add(op.link.source, groupKey(op.link.link_type, 'outbound'))
      add(op.link.destination, groupKey(op.link.link_type, 'inbound'))
    }
  }
  return out
}

function blank(id: EntityId): EntityRecord {
  return { id, values: {}, links: {}, local: true }
}

/**
 * Apply one op to the records that `read` gives, passing each changed record
 * to `write`. This is the optimistic half of a write: it does what the server
 * will do, as far as the cache can know it.
 */
export function applyOp(
  read: (id: EntityId) => EntityRecord | undefined,
  write: (record: EntityRecord) => void,
  op: Op,
  now: number,
): void {
  if ('set' in op) {
    const { entity, key, value } = op.set
    const record = read(entity) ?? blank(entity)
    // Values that are not loaded stay so: one key is not all of them.
    if (!record.values) return
    const values = { ...record.values }
    if (value === null) delete values[key]
    else values[key] = value
    write({ ...record, values })
    return
  }
  const { source, destination, link_type, sort = 'timestamp' } = op.link
  const key = sort === 'removed' ? null : sort === 'timestamp' ? now : sort.explicit
  const link: CachedLink = { source, destination, link_type, sort: key ?? 0, event: null }
  for (const [id, direction, other] of [
    [source, 'outbound', destination],
    [destination, 'inbound', source],
  ] as const) {
    // An entity the cache does not know, or a group it has not loaded: a
    // later load brings the server's version.
    const record = read(id)
    if (!record) continue
    const group = groupKey(link_type, direction)
    const links = record.links[group] ?? (record.local ? [] : undefined)
    if (!links) continue
    const rest = links.filter((l) => otherEnd(l, direction) !== other)
    const next = key === null ? rest : sortLinks([...rest, link], direction)
    write({ ...record, links: { ...record.links, [group]: next } })
  }
}

export function otherEnd(link: CachedLink, direction: Direction): EntityId {
  return direction === 'outbound' ? link.destination : link.source
}

/** The server's link order: ascending sort key, then the other entity's ID. */
export function sortLinks(links: CachedLink[], direction: Direction): CachedLink[] {
  return links.sort((a, b) => {
    if (a.sort !== b.sort) return a.sort - b.sort
    const x = otherEnd(a, direction)
    const y = otherEnd(b, direction)
    return x < y ? -1 : x > y ? 1 : 0
  })
}

/** Apply a batch of ops to `base` directly (used when a write is confirmed but cannot be read back). */
export function applyOps(base: ReadonlyMap<EntityId, EntityRecord>, ops: readonly Op[], now: number) {
  const next = new Map(base)
  for (const op of ops) applyOp((id) => next.get(id), (r) => next.set(r.id, r), op, now)
  return next
}

// --- merging what the server says ------------------------------------------

/** What a load brings: values and link groups per entity, either part optional. */
export interface Loaded {
  values?: Record<EntityId, Values>
  links?: Record<EntityId, readonly LinkGroup[]>
}

export function loadedFromWalk(cache: WalkCache): Loaded {
  const values: Record<EntityId, Values> = {}
  for (const [id, node] of Object.entries(cache.nodes)) values[id] = node.values
  return { values, links: cache.links }
}

/**
 * Merge a load into `base`. `skip(id)` leaves an entity out: one that a write
 * changed after the load started, so that an old answer cannot hide it.
 */
export function mergeLoaded(
  base: ReadonlyMap<EntityId, EntityRecord>,
  loaded: Loaded,
  skip: (id: EntityId) => boolean = () => false,
): Map<EntityId, EntityRecord> {
  const next = new Map(base)
  for (const [id, values] of Object.entries(loaded.values ?? {})) {
    if (skip(id)) continue
    const record = next.get(id)
    next.set(id, { id, values, links: record?.links ?? {} })
  }
  for (const [id, groups] of Object.entries(loaded.links ?? {})) {
    if (skip(id)) continue
    const record = next.get(id) ?? { id, links: {} }
    const links = { ...record.links }
    for (const group of groups) {
      links[groupKey(group.link_type, group.direction)] = group.links
    }
    next.set(id, { ...record, links, local: undefined })
  }
  return next
}

// --- reading -----------------------------------------------------------------

/** The follow rules of an entity, from its type. */
export function rulesOf(follow: Follow, values: Values): readonly FollowRule[] {
  const type = values[TYPE_KEY]
  return typeof type === 'string' ? (follow[type] ?? []) : []
}

/** Whether a node passes an optional filter. */
export function passes(filter: Filter | undefined, id: EntityId, values: Values): boolean {
  return !filter || matches(filter, id, values)
}
