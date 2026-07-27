import type { LinkDirection, QueryResult } from '../core/types'
import { NO_PAGE, pagesAtom, str, summariesAtom, summaryOf, type EntitySummary, type Page } from './query'
import { getView } from './store'
import {
  ROOT_ID,
  collapsedBelow,
  levelKey,
  samePath,
  topLevel,
  type Level,
  type ViewState,
} from './types'

// Everything derived from latent state plus the query cache: the flat row list the
// outline renders, the selection actually in effect, and the names things go by.
// All pure functions, and the same ones the views and the tools read — so what is
// on screen and what a tool acts on cannot disagree.

/** A rendered bullet backed by a real entity: what it says about itself, plus where
 * it sits on screen. */
export interface EntityRow extends EntitySummary {
  kind: 'entity'
  id: string
  /** Depth within the level (0 = its root). */
  depth: number
  /** Ids from the root to this row — its identity, since ids can repeat. */
  path: string[]
  /** The row's parent id, or null at the root of the level. */
  parentId: string | null
  /** True when the text renders as a section heading. */
  section?: boolean
  /** Checkbox state: `true` = open box, `false` = ticked, undefined = plain bullet. */
  open?: boolean
  hasChildren: boolean
  collapsed: boolean
  selected: boolean
  /** True while this row's text is being edited in place. */
  editing: boolean
  /** The persisted draft, while editing. */
  draft?: string
}

/** The transient row shown while a new entity is being typed. */
export interface InputRow {
  kind: 'input'
  depth: number
  draft: string
  type?: string
  section?: boolean
  open?: boolean
}

export type Row = EntityRow | InputRow

export interface ViewRows {
  rows: Row[]
  /** The selection in effect — resolved against the visible rows. Never stored. */
  selectedPath: string[]
  loading: boolean
  error: string | null
  /** True when the whole tree has been fetched (no further pages). */
  complete: boolean
  /** True when another page is waiting to be asked for. */
  more: boolean
}

const key = (path: readonly string[]): string => path.join('\0')

/**
 * Walk the query results into a flat list, tracking each row's full path and
 * dropping everything under a folded row.
 *
 * The folding is done here rather than by the query (see the note in ./query), so
 * this has to skip subtrees itself: the results arrive whole, and a row inside a
 * folded ancestor is simply not rendered.
 */
function walk(results: QueryResult[], collapsed: Set<string>, direction: LinkDirection): EntityRow[] {
  const out: EntityRow[] = []
  const stack: string[] = []
  // Depth of the shallowest folded row we are currently inside, if any.
  let hiddenBelow = Infinity
  for (const { entity, depth } of results) {
    stack.length = depth
    stack.push(entity.id)
    if (depth > hiddenBelow) continue
    hiddenBelow = Infinity
    const folded = collapsed.has(entity.id)
    if (folded) hiddenBelow = depth
    const open = entity.values.open
    out.push({
      kind: 'entity',
      id: entity.id,
      depth,
      path: stack.slice(),
      parentId: depth > 0 ? (stack[depth - 1] ?? null) : null,
      ...summaryOf(entity.values),
      section: entity.values.section === true,
      open: open === true ? true : open === false ? false : undefined,
      hasChildren: (direction === 'in' ? entity.inboundLinks : entity.outboundLinks).length > 0,
      collapsed: folded,
      selected: false,
      editing: false,
    })
  }
  return out
}

/** Keep rows whose text matches, plus their ancestors so the tree still reads. */
function applyFind(rows: EntityRow[], find: string): EntityRow[] {
  const q = find.trim().toLowerCase()
  if (!q) return rows
  const keep = new Set<string>()
  for (const row of rows) {
    if (!(row.text ?? '').toLowerCase().includes(q)) continue
    for (let i = 1; i <= row.path.length; i++) keep.add(key(row.path.slice(0, i)))
  }
  return rows.filter((r) => keep.has(key(r.path)))
}

/**
 * Keep the section rows plus the level's root — the tree as a contents page.
 * Unlike find, non-matching ancestors are dropped: the point is to see the
 * sections and nothing else. Rows keep their real depth, so a section nested
 * inside an ordinary entity still reads as nested.
 */
const onlySections = (rows: EntityRow[]): EntityRow[] =>
  rows.filter((r) => r.depth === 0 || r.section)

/**
 * The selection actually in effect. Strips trailing ids until the path exists,
 * falling back to the level's root. While pages are still outstanding an unfound
 * path is left alone rather than snapped, since the row it names may yet arrive.
 */
export function resolveSelectedPath(
  latent: string[],
  rows: EntityRow[],
  rootId: string,
  complete: boolean,
): string[] {
  const paths = new Set(rows.map((r) => key(r.path)))
  if (paths.has(key(latent))) return latent
  if (!complete) return latent
  let path = latent
  while (path.length > 0 && !paths.has(key(path))) path = path.slice(0, -1)
  return path.length > 0 ? path : [rootId]
}

/** Build the rows on screen from the latent state and whatever the cache holds. */
export function buildRows(s: ViewState, level: Level, page: Page): ViewRows {
  const complete = !page.loading && page.continuation == null
  let rows = walk(page.results, new Set(collapsedBelow(s.collapsed, level.rootId)), level.direction)
  if (s.find != null) rows = applyFind(rows, s.find)
  if (s.sectionsOnly) rows = onlySections(rows)

  const selectedPath = resolveSelectedPath(level.selectedPath, rows, level.rootId, complete)
  const edit = s.edit
  const marked: Row[] = rows.map((row) => {
    const editing = edit?.mode === 'edit' && samePath(row.path, edit.path)
    return {
      ...row,
      selected: samePath(row.path, selectedPath),
      editing,
      draft: editing ? edit?.draft : undefined,
    }
  })

  // Splice the "new entity" input in after the row it will follow — or, when it
  // will be the last child, after the parent's whole visible subtree. A folded
  // parent has none on screen, so the box lands directly beneath it.
  if (edit?.mode === 'create') {
    const at = marked.findIndex((r) => r.kind === 'entity' && samePath(r.path, edit.path))
    if (at >= 0) {
      const parentDepth = marked[at].depth
      let insert = at + 1
      if (edit.after) {
        // Directly after that child's own subtree, so it appears where it will be.
        const afterAt = marked.findIndex(
          (r, i) => i > at && r.kind === 'entity' && r.id === edit.after && r.depth === parentDepth + 1,
        )
        if (afterAt >= 0) {
          insert = afterAt + 1
          while (insert < marked.length && marked[insert].depth > parentDepth + 1) insert++
        }
      } else {
        while (insert < marked.length && marked[insert].depth > parentDepth) insert++
      }
      const open = edit.values.open
      marked.splice(insert, 0, {
        kind: 'input',
        depth: parentDepth + 1,
        draft: edit.draft,
        type: str(edit.values.type),
        section: edit.values.section === true,
        open: open === true ? true : open === false ? false : undefined,
      })
    }
  }

  return {
    rows: marked,
    selectedPath,
    loading: page.loading,
    error: page.error,
    complete,
    more: page.continuation != null,
  }
}

/** The rows on screen, from the current state and cache. */
export function viewRows(s: ViewState = getView()): ViewRows {
  const level = topLevel(s)
  return buildRows(s, level, pagesAtom.get()[levelKey(level)] ?? NO_PAGE)
}

/** Only the entity rows, in order — what selection movement steps through. */
export const entityRows = (rows: Row[]): EntityRow[] =>
  rows.filter((r): r is EntityRow => r.kind === 'entity')

// --- Labels -----------------------------------------------------------------

/** What to call a file with no caption of its own: `image/png` → "PNG image". */
function fileLabel(mimeType?: string): string {
  const subtype = mimeType?.split('/')[1]?.split('+')[0]?.replace(/[^a-z0-9]/gi, '')
  if (!subtype) return 'File'
  return `${subtype.toUpperCase()} ${mimeType?.startsWith('image/') ? 'image' : 'file'}`
}

/**
 * An entity's display name, from the summaries the query engine has harvested: its
 * text, or what it is when it has none, and failing that its raw id — which is
 * what a crumb shows for a moment after a cold start.
 *
 * Trimmed to one line: a crumb or a sheet title has one line to spend, and an
 * entity's text can be a paragraph.
 */
export function entityLabel(summaries: Record<string, EntitySummary>, id: string): string {
  const summary = summaries[id]
  if (summary?.text) return summary.text.split('\n')[0].slice(0, 80)
  if (summary?.type === 'file') return fileLabel(summary.mimeType)
  return id === ROOT_ID ? 'Index' : id
}

export const labelOf = (id: string): string => entityLabel(summariesAtom.get(), id)

/** One step of the navigation stack, outermost first — the header's crumb trail. */
export interface Crumb {
  /** Index in the stack, which is what following a crumb pops to. */
  index: number
  rootId: string
  label: string
}

export const crumbs = (s: ViewState, summaries: Record<string, EntitySummary>): Crumb[] =>
  s.stack.map((level, index) => ({
    index,
    rootId: level.rootId,
    label: entityLabel(summaries, level.rootId),
  }))
