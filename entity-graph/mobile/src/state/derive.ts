import { str, summaryOf, type EntitySummary } from '../../../src/core/entity'
import type { EntitySource } from '../../../src/core/query'
import { buildTree, type TreeRow } from '../../../src/core/tree'
import {
  ROOT_ID,
  collapsedBelow,
  samePath,
  type Level,
  type ViewState,
} from './types'

// Everything derived from latent state plus the entity cache: the flat row list
// the outline renders, the selection actually in effect, and the names things go
// by. All pure functions, and the same ones the views and the tools read — so
// what is on screen and what a tool acts on cannot disagree.
//
// The rows themselves come from `core/tree`, shared with the desktop app: it is
// the same outline over the same cache. What this adds is what a *level* knows
// and a query doesn't — which row is selected, which is being typed into, and
// where the box for a new line goes.

export type { EntitySummary }
export { str, summaryOf }

/**
 * A rendered bullet backed by a real entity: what the query says about it, plus
 * what the level says about it.
 */
export interface EntityRow extends TreeRow {
  kind: 'entity'
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
  /** True while any row's entity is still being read. */
  loading: boolean
  error: string | null
  /** True when the traversal ran out rather than hitting the row limit. */
  complete: boolean
}

const key = (path: readonly string[]): string => path.join('\0')

/**
 * The selection actually in effect. Strips trailing ids until the path exists,
 * falling back to the level's root. While entities are still outstanding an
 * unfound path is left alone rather than snapped, since the row it names may yet
 * arrive.
 */
export function resolveSelectedPath(
  latent: string[],
  rows: readonly TreeRow[],
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

/**
 * Build the rows on screen: the shared tree, marked up with what this level
 * knows — which row is selected, which is being typed into, and where the box
 * for a new line goes.
 */
export function buildRows(
  s: ViewState,
  level: Level,
  source: EntitySource,
  limit: number,
): ViewRows {
  const { rows, complete, loading, error } = buildTree(
    [level.rootId],
    source,
    {
      direction: level.direction,
      collapsed: collapsedBelow(s.collapsed, level.rootId),
      maxDepth: {},
    },
    limit,
    { find: s.find, sections: s.sectionsOnly },
  )

  // A path that isn't among the rows may simply not have arrived yet, so the
  // selection is only snapped once the level has everything it is going to get.
  const settled = complete && !loading
  const selectedPath = resolveSelectedPath(level.selectedPath, rows, level.rootId, settled)
  const edit = s.edit
  const marked: Row[] = rows.map((row): Row => {
    const editing = edit?.mode === 'edit' && samePath(row.path, edit.path)
    return {
      kind: 'entity',
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

  return { rows: marked, selectedPath, loading, error, complete }
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
 * An entity's display name: its text, or what it is when it has none, and
 * failing that its raw id — which is what a crumb shows for a moment after a
 * cold start, until the entity loads.
 *
 * Reading the cache is also what asks for it, so naming an entity is enough to
 * make it load. Trimmed to one line: a crumb or a sheet title has one line to
 * spend, and an entity's text can be a paragraph.
 */
export function entityLabel(source: EntitySource, id: string): string {
  const summary = summaryOf(source.get([id])[id].values)
  if (summary.text) return summary.text.split('\n')[0].slice(0, 80)
  if (summary.type === 'file') return fileLabel(summary.mimeType)
  return id === ROOT_ID ? 'Index' : id
}

/** One step of the navigation stack, outermost first — the header's crumb trail. */
export interface Crumb {
  /** Index in the stack, which is what following a crumb pops to. */
  index: number
  rootId: string
  label: string
}

export const crumbs = (s: ViewState, source: EntitySource): Crumb[] =>
  s.stack.map((level, index) => ({
    index,
    rootId: level.rootId,
    label: entityLabel(source, level.rootId),
  }))
