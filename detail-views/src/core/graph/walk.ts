import { emptyEntity, type GraphEntity } from './entity.ts'

/**
 * The graph walk: a stepper from one path to the next in a depth-first
 * reading, bounded by a depth and a row limit so a view never draws more than
 * it can. Its only reach outside is `get`, which is synchronous: in the
 * renderer it reads the entity cache, so an entity not loaded yet looks
 * childless and the walk fills in as events arrive. Shared with the renderer.
 */

/** Entities by id. Ids with nothing behind them come back empty. */
export type GetEntities = (ids: string[]) => Record<string, GraphEntity>

export interface Traversal {
  /** entity id → how many levels may be walked below it (null: no limit). The nearest ancestor with an entry wins. */
  maxDepth: Record<string, number | null>
  /** Entities whose children are read last first: chat, which wants its newest end. */
  newestFirst?: readonly string[]
}

const last = (path: readonly string[]): string => path[path.length - 1]

function budget(path: readonly string[], maxDepth: Traversal['maxDepth']): number {
  for (let i = path.length - 1; i >= 0; i--) {
    if (!(path[i] in maxDepth)) continue
    const cap = maxDepth[path[i]]
    return cap == null ? Infinity : cap - (path.length - 1 - i)
  }
  return Infinity
}

/** The children the walk goes into below the end of `path`; never an ancestor. */
export function childrenOf(path: readonly string[], get: GetEntities, t: Traversal): string[] {
  const id = last(path)
  if (id === undefined || budget(path, t.maxDepth) <= 0) return []
  const entity = get([id])[id] ?? emptyEntity(id)
  const children = entity.outboundLinks.filter((child) => !path.includes(child))
  return t.newestFirst?.includes(id) ? children.reverse() : children
}

/** The path after this one, depth first; null when the walk is done. */
export function stepPath(path: readonly string[], get: GetEntities, t: Traversal): string[] | null {
  const below = childrenOf(path, get, t)
  if (below.length) return [...path, below[0]]
  for (let i = path.length - 1; i > 0; i--) {
    const parent = path.slice(0, i)
    const siblings = childrenOf(parent, get, t)
    const at = siblings.indexOf(path[i])
    if (at >= 0 && at + 1 < siblings.length) return [...parent, siblings[at + 1]]
  }
  return null
}

export interface Walked {
  /** Every path reached, in reading order; the first is the start. */
  paths: string[][]
  /** False when the limit cut the walk short. */
  complete: boolean
}

/** Walk from `start` until done or `limit` paths are collected. */
export function walk(start: readonly string[], get: GetEntities, t: Traversal, limit: number): Walked {
  const paths: string[][] = []
  let path: string[] | null = [...start]
  while (path && paths.length < limit) {
    paths.push(path)
    path = stepPath(path, get, t)
  }
  return { paths, complete: path === null }
}
