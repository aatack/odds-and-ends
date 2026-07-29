import { summaryOf, type Entity, type EntitySummary, type LinkDirection } from './entity'
import {
  filterPaths,
  resolveQuery,
  type EntitySource,
  type QueryFilters,
  type QueryRow,
  type Traversal,
} from './query'

// A query, as rows. The traversal in ./query says which entities are in view and
// in what order; this says what each of them looks like as a line in an outline.
//
// Every client shares this, because it is the same outline on every screen: what
// differs between them is the shell around it — a desktop frame with a selection
// and an in-place editor, a phone level with one edit at a time — and each adds
// that to what comes out of here.

/**
 * One line of an outline: what the entity says about itself, plus where it sits.
 * A row is identified by its *path*, not its id, since the graph isn't a tree
 * and the same entity can appear in several places at once.
 */
export interface TreeRow extends EntitySummary {
  id: string
  /** Levels below the entity the query started at. */
  depth: number
  path: string[]
  /** The row above it in the tree, or null at the top of the query. */
  parentId: string | null
  /** When true the text renders as a section heading. */
  section?: boolean
  /** Checkbox state: `true` = open box, `false` = ticked, undefined = plain bullet. */
  open?: boolean
  hasChildren: boolean
  collapsed: boolean
}

export interface Tree {
  rows: TreeRow[]
  /** True when the traversal ran out rather than hitting the row limit. */
  complete: boolean
  /** True while any row's entity is still being read. */
  loading: boolean
  error: string | null
}

export const EMPTY_TREE: Tree = { rows: [], complete: true, loading: false, error: null }

/**
 * One row: what the entity at the end of a path says about itself, plus what the
 * caller knows about where it sits. Depth is passed in rather than read off the
 * path because how far in a row reads depends on what the query started from,
 * and the two callers below start from different places.
 */
function rowOf(
  path: readonly string[],
  entity: Entity,
  depth: number,
  direction: LinkDirection,
  collapsed: boolean,
): TreeRow {
  const open = entity.values.open
  return {
    id: path[path.length - 1],
    depth,
    path: [...path],
    parentId: path.length > 1 ? path[path.length - 2] : null,
    ...summaryOf(entity.values),
    section: entity.values.section === true,
    open: open === true ? true : open === false ? false : undefined,
    // Which links count is the direction the query reads in, so a chevron
    // means "there is more under this here" rather than always meaning
    // outbound links.
    hasChildren: (direction === 'in' ? entity.inboundLinks : entity.outboundLinks).length > 0,
    collapsed,
  }
}

const isStrictPrefix = (a: readonly string[], b: readonly string[]): boolean =>
  a.length < b.length && a.every((id, i) => b[i] === id)

/**
 * How deep each of a filtered set of paths *reads*, which is not how deep it
 * sits: a row whose parent the filters removed moves up to take its place, so
 * one row is never more than one level further in than the row above it. Without
 * this a sections-only outline of a deeply buried heading indents it past
 * anything it could be read as sitting under.
 *
 * Depth is therefore "how many of my ancestors are still here", counted by
 * walking the kept rows in reading order over a stack of the ones above. The
 * stack is seeded with the first row's own ancestors so that a page resuming
 * mid-tree still starts at the depth it really sits at — only gaps *between kept
 * rows* are closed, never the gap above the first of them.
 */
function keptDepths(
  kept: readonly (readonly string[])[],
  depthOf: (path: readonly string[]) => number,
): number[] {
  if (!kept.length) return []
  const above: (readonly string[])[] = []
  for (let i = 1; i < kept[0].length; i++) above.push(kept[0].slice(0, i))
  const base = depthOf(kept[0]) - above.length
  return kept.map((path) => {
    while (above.length && !isStrictPrefix(above[above.length - 1], path)) above.pop()
    const depth = base + above.length
    above.push(path)
    return depth
  })
}

/**
 * A page of a query as rows — the counterpart of {@link buildTree} for a caller
 * that was handed the answer rather than walking a cache itself, which is what
 * the `query` tool's rows are.
 *
 * Depth counts from the *root of the walk* rather than from the page's first row,
 * because a page after the first resumes in the middle of the tree: its rows can
 * be shallower than the one it resumed at, and the only thing every page shares
 * is the root all of its paths begin with. Nothing is folded — a caller reading
 * pages has no folded set, and the page is what it is.
 */
export const rowsOfPage = (
  rows: readonly QueryRow[],
  direction: LinkDirection = 'out',
): TreeRow[] => {
  const depths = keptDepths(
    rows.map((r) => r.path),
    (path) => path.length - 1,
  )
  return rows.map(({ path, entity }, i) => rowOf(path, entity, depths[i], direction, false))
}

/**
 * Run a query and describe what it reached.
 *
 * The limit is on the *traversal*, not on what survives the filters, which is
 * what makes find a filter over the rows rather than a different query. An
 * entity that hasn't loaded looks childless, so the tree fills in as events
 * arrive — and `loading` says whether that is still happening, which is the
 * difference between "there is nothing here" and "there is nothing here yet".
 */
export function buildTree(
  start: readonly string[],
  source: EntitySource,
  traversal: Traversal,
  limit: number,
  filters: QueryFilters = {},
): Tree {
  const { paths, complete } = resolveQuery(start, source.get, traversal, limit)
  const kept = filterPaths(start, paths, source.get, filters)

  const ids = paths.map((path) => path[path.length - 1])
  const entities = source.get(ids)
  const folded = new Set(traversal.collapsed)
  const depths = keptDepths(kept, (path) => path.length - start.length)

  const rows = kept.map((path, i): TreeRow => {
    const id = path[path.length - 1]
    return rowOf(path, entities[id], depths[i], traversal.direction, folded.has(id))
  })

  return {
    rows,
    complete,
    // Over everything walked, not only what survived: a row filtered out was
    // still read, and a filter that matches nothing yet is still waiting.
    loading: ids.some((id) => source.pending(id)),
    error: ids.map((id) => source.error(id)).find((e) => e != null) ?? null,
  }
}

/** The traversal a folded set and a depth map describe. */
export const traversalOf = (
  direction: Traversal['direction'],
  collapsed: readonly string[],
  maxDepth: Record<string, number | null> = {},
): Traversal => ({ direction, collapsed, maxDepth })
