import { emptyEntity, summaryOf, type Entity, type LinkDirection } from './entity'

// The query, as a stepper over paths. One function knows how to get from a path
// to the next one in a depth-first reading of the graph; a query is that
// function run until it runs out or the caller has enough.
//
// Its only way of reaching the outside world is `getEntities`, which is
// deliberately synchronous: on the frontend it reads the entity cache, so the
// tree recomputes the instant an entity changes rather than after a refetch, and
// an entity that hasn't loaded simply looks childless until it has. A caller
// with a real store behind it wraps one around a batch loader.

/** Entities by id. Ids with nothing behind them come back as empty entities. */
export type GetEntities = (entityIds: string[]) => Record<string, Entity>

/**
 * A read of a cache at one moment, plus what each entity is doing. Everything
 * that turns state into rows takes one of these rather than a cache itself, so
 * the derivations never have to know how the cache is shaped.
 */
export interface EntitySource {
  get: GetEntities
  /** True while an entity's events are still on their way. */
  pending: (id: string) => boolean
  /** Why an entity couldn't be read, if it couldn't. */
  error: (id: string) => string | null
}

export interface Traversal {
  /** Which links to follow: `out` for children, `in` for whatever points here. */
  direction: LinkDirection
  /** Entity ids whose children are not walked. */
  collapsed: readonly string[]
  /**
   * entity id → how many levels may be walked below it (null = no limit). The
   * cap is relative to the entity that set it, and the nearest ancestor with an
   * entry wins — so a limit set deep in the tree overrides one set above it,
   * including by lifting it.
   */
  maxDepth: Record<string, number | null>
}

export const NO_TRAVERSAL: Traversal = { direction: 'out', collapsed: [], maxDepth: {} }

const last = (path: readonly string[]): string => path[path.length - 1]

const linksOf = (entity: Entity, direction: LinkDirection): string[] =>
  direction === 'in' ? entity.inboundLinks : entity.outboundLinks

/**
 * How many more levels may be walked below the end of `path`. The nearest
 * ancestor that sets a cap decides, so an entity given its own limit is not also
 * subject to its parent's; `null` there means "no limit", which is how a deep
 * entity lifts a cap set above it.
 */
function budget(path: readonly string[], maxDepth: Record<string, number | null>): number {
  for (let i = path.length - 1; i >= 0; i--) {
    if (!(path[i] in maxDepth)) continue
    const cap = maxDepth[path[i]]
    return cap == null ? Infinity : cap - (path.length - 1 - i)
  }
  return Infinity
}

/**
 * The children of the entity at the end of `path`, as the traversal sees them:
 * nothing at all when it is folded or out of depth, and never an entity already
 * on the path — the same entity may appear in several branches, but it can't be
 * its own ancestor.
 */
export function childrenOf(path: readonly string[], get: GetEntities, t: Traversal): string[] {
  const id = last(path)
  if (id === undefined) return []
  if (t.collapsed.includes(id) || budget(path, t.maxDepth) <= 0) return []
  const entity = get([id])[id]
  if (!entity) return []
  return linksOf(entity, t.direction).filter((child) => !path.includes(child))
}

/**
 * The path after this one in a depth-first reading: the first child if there is
 * one, otherwise the next sibling of the deepest ancestor that has one. Null
 * when the traversal is exhausted.
 *
 * Siblings are recomputed from the parent rather than remembered, which is what
 * makes the walk safe to interleave with edits: a path whose entity has since
 * been unlinked simply carries on from the next ancestor that still has
 * somewhere to go.
 */
export function stepPath(
  path: readonly string[],
  get: GetEntities,
  t: Traversal,
): string[] | null {
  const children = childrenOf(path, get, t)
  if (children.length) return [...path, children[0]]

  for (let i = path.length - 1; i > 0; i--) {
    const parentPath = path.slice(0, i)
    const siblings = childrenOf(parentPath, get, t)
    const at = siblings.indexOf(path[i])
    if (at >= 0 && at + 1 < siblings.length) return [...parentPath, siblings[at + 1]]
  }
  return null
}

export interface ResolvedQuery {
  /** Every path the traversal reached, in reading order. The first is `start`. */
  paths: string[][]
  /** False when the limit cut the traversal short — there is more below. */
  complete: boolean
  /** Where to start again to get the rest; null when there is no rest. */
  next: string[] | null
}

/**
 * Walk from `start` until the traversal is exhausted or `limit` paths have been
 * collected. The starting path is itself a result: a query rooted at an entity
 * includes that entity.
 */
export function resolveQuery(
  start: readonly string[],
  get: GetEntities,
  t: Traversal,
  limit: number,
): ResolvedQuery {
  const paths: string[][] = []
  let path: string[] | null = [...start]
  while (path && paths.length < limit) {
    paths.push(path)
    path = stepPath(path, get, t)
  }
  return { paths, complete: path == null, next: path }
}

// --- Filters ----------------------------------------------------------------

/**
 * What to keep of what the traversal reached. These are filters over the rows
 * rather than part of the walk: the traversal decides where to go, and this
 * decides what is worth showing, so a limit means the same thing either way.
 */
export interface QueryFilters {
  /** Keep rows whose text contains this, plus their ancestors. */
  find?: string | null
  /** Keep only section rows, plus the row the query started from. */
  sections?: boolean
}

const key = (path: readonly string[]): string => path.join('\0')

/**
 * Apply the filters to a set of resolved paths.
 *
 * Find keeps a matching row's ancestors, so the tree still reads. Sections
 * pointedly does not — the point of it is to see the sections and nothing else —
 * and rows keep their real depth either way, so a section nested inside an
 * ordinary entity still reads as nested. Find is applied first, so the two
 * compose.
 *
 * Sections keeps the row the query started from whether or not it is one, since
 * it is the thing that was asked about. That is the *path* it started from, not
 * every row at that depth: a walk resumed in the middle of the tree would
 * otherwise let every sibling of the resume point through.
 */
export function filterPaths(
  start: readonly string[],
  paths: string[][],
  get: GetEntities,
  filters: QueryFilters,
): string[][] {
  const find = filters.find?.trim().toLowerCase()
  const entities = get(paths.map((path) => last(path)))
  const values = (path: readonly string[]): Record<string, unknown> =>
    entities[last(path)]?.values ?? {}

  let kept = paths
  if (find) {
    const keep = new Set<string>()
    for (const path of kept) {
      const text = summaryOf(values(path)).text ?? ''
      if (!text.toLowerCase().includes(find)) continue
      for (let i = 1; i <= path.length; i++) keep.add(key(path.slice(0, i)))
    }
    kept = kept.filter((path) => keep.has(key(path)))
  }
  if (filters.sections) {
    kept = kept.filter((path) => key(path) === key(start) || values(path).section === true)
  }
  return kept
}

// --- Running one against a store --------------------------------------------

/** Read entities in a batch. The async counterpart of {@link GetEntities}. */
export type LoadEntities = (entityIds: string[]) => Promise<Record<string, Entity>>

export interface QueryRow {
  /** Ids from the query's starting entity to this one — its identity, since ids repeat. */
  path: string[]
  entity: Entity
}

export interface QueryPage {
  rows: QueryRow[]
  /**
   * Where to resume when the limit cut the walk short: pass it back as `path` to
   * carry on from exactly there. Null once the traversal has run out.
   */
  continuation: string[] | null
  /** How many entities the traversal visited, before the filters were applied. */
  scanned: number
}

/**
 * Resolve a query against a store, one page at a time.
 *
 * The traversal itself is synchronous and knows nothing about loading, so this
 * runs it against a cache that starts empty and fills up: every pass records
 * what it wanted and couldn't have, reads that batch, and runs again. A pass
 * therefore reaches one level deeper than the last, and the whole thing settles
 * in as many round trips as the page is deep — not one per entity.
 *
 * A pass is the *whole* page — the walk, the filters, and the rows — rather
 * than only the walk, because the three want different entities. The walk stops
 * reading at a row it will not descend through (one that is folded, or at the
 * depth cap), so a page built off the walk alone comes back with its deepest
 * rows blank: read but never asked for. Building the page each time and looping
 * until nothing is outstanding is what makes "the answer" and "what was loaded"
 * the same set. This is the difference from the frontend, which renders what it
 * has and fills in as events arrive; here there is nothing to redraw, so the
 * page is not returned until it is whole.
 *
 * The limit is on the walk rather than on what survives the filters, so a
 * narrow filter over a wide tree comes back with few rows and a continuation
 * rather than with a long silence.
 */
export async function runQuery(
  start: readonly string[],
  load: LoadEntities,
  t: Traversal,
  limit: number,
  filters: QueryFilters = {},
): Promise<QueryPage> {
  const known = new Map<string, Entity>()
  let missing = new Set<string>()

  const get: GetEntities = (ids) => {
    const out: Record<string, Entity> = {}
    for (const id of ids) {
      const entity = known.get(id)
      if (entity) out[id] = entity
      else {
        missing.add(id)
        out[id] = emptyEntity(id)
      }
    }
    return out
  }

  /** The page as it stands, recording everything it wanted and didn't have. */
  const pass = (): QueryPage => {
    const resolved = resolveQuery(start, get, t, limit)
    const kept = filterPaths(start, resolved.paths, get, filters)
    return {
      rows: kept.map((path) => ({ path, entity: get([last(path)])[last(path)] })),
      continuation: resolved.next,
      scanned: resolved.paths.length,
    }
  }

  let page = pass()
  while (missing.size) {
    const batch = [...missing]
    missing = new Set()
    for (const [id, entity] of Object.entries(await load(batch))) known.set(id, entity)
    // An id that came back with nothing is still an answer; recording it stops
    // the next pass asking for it again, which is what makes this terminate.
    for (const id of batch) if (!known.has(id)) known.set(id, emptyEntity(id))
    page = pass()
  }
  return page
}
