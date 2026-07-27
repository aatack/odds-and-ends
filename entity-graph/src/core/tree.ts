import { summaryOf, type EntitySummary } from './entity'
import {
  filterPaths,
  resolveQuery,
  type EntitySource,
  type QueryFilters,
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

  const rows = kept.map((path): TreeRow => {
    const id = path[path.length - 1]
    const entity = entities[id]
    const open = entity.values.open
    return {
      id,
      depth: path.length - start.length,
      path,
      parentId: path.length > 1 ? path[path.length - 2] : null,
      ...summaryOf(entity.values),
      section: entity.values.section === true,
      open: open === true ? true : open === false ? false : undefined,
      // Which links count is the direction the query reads in, so a chevron
      // means "there is more under this here" rather than always meaning
      // outbound links.
      hasChildren:
        (traversal.direction === 'in' ? entity.inboundLinks : entity.outboundLinks).length > 0,
      collapsed: folded.has(id),
    }
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
