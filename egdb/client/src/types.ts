// The JSON shapes of the egdb HTTP API (see API.md in the crate). Names follow
// the wire format, so they are snake_case where the server's are.

/** A stored value. `null` in a write clears the key; reads never return it. */
export type Value = null | boolean | number | string | Json[] | { [key: string]: Json }
export type Json = null | boolean | number | string | Json[] | { [key: string]: Json }

/** All the (non-null) values of one entity. */
export type Values = Record<string, Value>

export type BranchId = string
export type EntityId = string

/** A path through the graph: the first ID is the start, the last is the node. */
export type Path = readonly EntityId[]

export type Direction = 'outbound' | 'inbound'

/** `[link_type, direction]`. */
export type LinkPair = readonly [linkType: string, direction: Direction]

/**
 * `[link_type, direction, child_type]`: follow these links to entities of
 * `child_type` only. A `null` child type is the wildcard: any type.
 */
export type FollowRule = readonly [linkType: string, direction: Direction, childType: string | null]

/** Which links a walk follows out of an entity, keyed by that entity's `type`. */
export type Follow = Readonly<Record<string, readonly FollowRule[]>>

/** How a link sorts: by the time it was written (default), by a number, or removed. */
export type Sort = 'timestamp' | { explicit: number } | 'removed'

export interface EventRef {
  branch: BranchId
  seq: number
  timestamp: number
  author: string
}

/** What a log read keeps: each filter left out keeps everything. `keys` are value keys and link types. */
export interface LogFilter {
  entities?: EntityId[]
  authors?: string[]
  keys?: string[]
  /** At or after, in ms. */
  since?: number
  /** Before, in ms. */
  until?: number
  /** At most this many: the newest kept, or with `after` the oldest. */
  limit?: number
  /** Only value events, or only link events; both when left out. */
  kind?: 'values' | 'links'
  /** Only clears and link removals. */
  removed?: boolean
  /** Only the branch's own events after this seq, oldest first: the change feed. */
  after?: number
  /** With `after`, and nothing after it: hold the request this many ms (at most 60000) for a write. */
  wait?: number
}

/** One value event; `value` null is a clear. */
export interface ValueChange {
  kind: 'value'
  entity: EntityId
  key: string
  value: Value
  event: EventRef
}

/** One link event; `sort` null is a removal. */
export interface LinkChange {
  kind: 'link'
  source: EntityId
  destination: EntityId
  link_type: string
  sort: number | null
  event: EventRef
}

export type Change = ValueChange | LinkChange

/**
 * A log read. `head` is the seq to pass as `after` next time (the last event's when `limit` cut it short, else the
 * branch's head); `reset` says the cursor was past the head (from another store): read what is held again.
 */
export interface Log {
  events: Change[]
  head: number
  reset: boolean
}

export interface Link {
  source: EntityId
  destination: EntityId
  link_type: string
  sort: number
  event: EventRef
}

export interface LinkGroup {
  link_type: string
  direction: Direction
  links: Link[]
}

// --- writes -----------------------------------------------------------------

export type Op =
  | { set: { entity: EntityId; key: string; value: Value } }
  | { link: { source: EntityId; destination: EntityId; link_type: string; sort?: Sort } }

export interface WriteBatch {
  branch: BranchId
  author: string
  timestamp?: number
  ops: Op[]
}

export interface WriteResult {
  branch: BranchId
  first_seq: number
  last_seq: number
  timestamp: number
}

/** How long the server lets a batch be undone (`UNDO_WINDOW_MS` on the server). */
export const UNDO_WINDOW_MS = 5 * 60 * 1000

export interface UndoOutcome {
  removed: [number, number]
  events: number
  restored: number
}

// --- find -------------------------------------------------------------------

export type Filter =
  | { eq: { key: string; value: Value } }
  | { ne: { key: string; value: Value } }
  | { lt: { key: string; value: number | string } }
  | { le: { key: string; value: number | string } }
  | { gt: { key: string; value: number | string } }
  | { ge: { key: string; value: number | string } }
  | { contains: { key: string; value: string } }
  | { contains_ignore_case: { key: string; value: string } }
  | { prefix: { key: string; value: string } }
  | { in: { key: string; values: Value[] } }
  | { is_null: { key: string } }
  | { not_null: { key: string } }
  | { and: Filter[] }
  | { or: Filter[] }
  | { not: Filter }

export type FindOrder = 'entity_id' | 'insertion' | { value_asc: string } | { value_desc: string }

export interface FindOptions {
  limit?: number
  cursor?: string | null
  order?: FindOrder
  values?: boolean
  include_archived?: boolean
  scan_limit?: number
}

export interface FindRow {
  id: EntityId
  values: Values
}

export interface FindPage {
  rows: FindRow[]
  next: string | null
  scanned: number
}

// --- walk -------------------------------------------------------------------

export type WalkDirection = 'forward' | 'backward'

export interface WalkOptions {
  steps?: number
  read_ahead?: { depth?: number; width?: number }
  visited?: EntityId[]
  direction?: WalkDirection
}

export interface WalkRequest {
  start: Path
  follow: Follow
  /** The walk goes below a node only when this matches it. */
  descend?: Filter
  /** A node is a row only when this matches it. */
  include?: Filter
  /** Paths that the walk does not go below. */
  collapsed?: readonly Path[]
  options?: WalkOptions
}

export interface WalkNode {
  id: EntityId
  values: Values
}

/** The values and followed links of the entities on a walk's paths. */
export interface WalkCache {
  nodes: Record<EntityId, WalkNode>
  links: Record<EntityId, LinkGroup[]>
}

export interface WalkPage {
  /** The paths of the nodes that `include` accepted, in visiting order. */
  paths: EntityId[][]
  cache: WalkCache
  next: EntityId[] | null
  steps: number
  loads: number
  visited?: EntityId[]
}

// --- prove ------------------------------------------------------------------

export interface ProveOptions {
  /** The most links below the start that a proof may have. */
  max_depth?: number
  /** The most entities the search may expand; past it the outcome is `unknown`. */
  max_nodes?: number
}

/** A walk query (as in `WalkRequest`, without paging options) and the entity it should emit. */
export interface ProveRequest {
  start: Path
  follow: Follow
  descend?: Filter
  include?: Filter
  collapsed?: readonly Path[]
  /** The entity to prove that the walk emits. */
  item: EntityId
  options?: ProveOptions
}

export type ProveOutcome = { proved: { path: EntityId[]; links: Link[] } } | 'no_path' | 'unknown'

export interface Prove {
  outcome: ProveOutcome
  stats: {
    nodes_visited: number
    link_reads: number
    value_reads: number
    sql_queries: number
    depth_reached: number
  }
}

// --- branches ---------------------------------------------------------------

export interface SeqRange {
  start: number
  end?: number | null
}

export interface ParentRef {
  branch: BranchId
  range: SeqRange
}

export interface BranchInfo {
  id: BranchId
  name: string
  archived: boolean
  head: number
  parents: ParentRef[]
  created_at: number
}

export type ErrorKind =
  | 'invalid'
  | 'rejected'
  | 'unauthorized'
  | 'unknown_branch'
  | 'branch_archived'
  | 'undo_refused'
  | 'schema'
  | 'corrupt'
  | 'internal'
  | 'not_implemented'
