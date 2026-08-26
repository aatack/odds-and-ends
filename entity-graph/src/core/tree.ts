import { str, summaryOf, type Entity, type EntitySummary, type LinkDirection } from './entity'
import { actionsOf } from './schema'
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
  /**
   * True when the row has children the outline isn't showing — folded, past a
   * depth cap, cut by a filter, ticked, or simply below where the walk stopped.
   * A row saying so is what greying one out means: there is more here than is on
   * the screen, whatever the reason. Folding is only the most common of them.
   */
  hidesChildren: boolean
  /**
   * True while this row's own entity is still arriving — its events, or the
   * derived ones its type's script makes. A row says so in place of its bullet,
   * which is the difference between an entity with nothing on it and one that
   * hasn't been read yet.
   */
  loading: boolean
  /**
   * The tools the row's *type* puts on it, by id and in the order the type
   * listed them — a button each, drawn on the end of the row's text. Absent
   * where there is no source to look the type up in, which is the caller
   * reading a page it was handed ({@link rowsOfPage}) rather than walking a
   * cache itself.
   */
  actions?: string[]
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

/** What the caller knows about a row that the entity itself cannot say. */
interface RowFacts {
  collapsed: boolean
  loading: boolean
  hidesChildren: boolean
  actions?: string[]
}

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
  { collapsed, loading, hidesChildren, actions }: RowFacts,
): TreeRow {
  const open = entity.values.open
  return {
    ...(actions?.length ? { actions } : {}),
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
    hidesChildren,
    loading,
  }
}

/** A path as one string, so a set of them can be asked about. */
const keyOf = (path: readonly string[]): string => path.join('\0')

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
  // A page was handed over rather than walked, so there is nothing to compare a
  // row's children against: it says it hides none of them.
  return rows.map(({ path, entity }, i) =>
    rowOf(path, entity, depths[i], direction, {
      collapsed: false,
      loading: false,
      hidesChildren: false,
    }),
  )
}

/**
 * Run a query and describe what it reached.
 *
 * The limit is on the *traversal*, not on what survives the filters, which is
 * what makes find a filter over the rows rather than a different query — though
 * the filters do reach the walk, since `open` also says where not to go. An
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
  const { paths, complete } = resolveQuery(start, source.get, traversal, limit, filters)
  const kept = filterPaths(start, paths, source.get, filters)

  const ids = paths.map((path) => path[path.length - 1])
  const entities = source.get(ids)
  const folded = new Set(traversal.collapsed)
  const depths = keptDepths(kept, (path) => path.length - start.length)

  // What each row's type has to say about it. Reading a type is also what asks
  // for it, so a row's buttons appear as soon as its type lands — and a type only
  // rows reference is fetched on their behalf.
  const typeIds = [
    ...new Set(kept.map((path) => str(entities[path[path.length - 1]]?.values.type))),
  ].filter((id): id is string => !!id)
  const types = typeIds.length ? source.get(typeIds) : {}

  // Which rows are on screen, so each of them can be asked whether its own
  // children are among them. Every reason a child might not be — folded, capped,
  // filtered, ticked, or past where the walk stopped — has already been decided
  // by the time this set exists, which is why it is one question here rather than
  // five conditions spread over the walk.
  const shown = new Set(kept.map(keyOf))

  const rows = kept.map((path, i): TreeRow => {
    const id = path[path.length - 1]
    const entity = entities[id]
    const typeId = str(entity?.values.type)
    const links = traversal.direction === 'in' ? entity.inboundLinks : entity.outboundLinks
    return rowOf(path, entity, depths[i], traversal.direction, {
      collapsed: folded.has(id),
      loading: source.pending(id),
      // A link back to something already on the path is not a child anybody could
      // have shown — the walk refuses it so a row can't be its own ancestor — so
      // it doesn't count as hidden.
      hidesChildren: links.some(
        (child) => !path.includes(child) && !shown.has(keyOf([...path, child])),
      ),
      actions: typeId ? actionsOf(types[typeId]?.values) : undefined,
    })
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
