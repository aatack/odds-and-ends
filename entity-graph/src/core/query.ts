import type { Entity, LinkDirection } from './wrapper'

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
  return { paths, complete: path == null }
}
