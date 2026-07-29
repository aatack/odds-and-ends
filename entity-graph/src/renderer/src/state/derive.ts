import { str, summaryOf, type EntitySummary } from '../../../core/entity'
import type { EntitySource } from '../../../core/query'
import { buildTree, type TreeRow } from '../../../core/tree'
import { focusOf } from './store'
import {
  ROOT_ID,
  collapsedBelow,
  directionOf,
  last,
  samePath,
  type CallContext,
  type FrameState,
  type LayoutState,
} from './types'

// Everything derived from latent state plus the entity cache: the flat row list
// a frame renders, the selection actually in effect, and the context a call is
// born with. All pure functions — the views and the tools call the same ones, so
// what the user sees and what a tool acts on can't disagree.
//
// A frame's rows are the query, run here rather than fetched: `core/tree` steps
// the traversal over whatever the cache holds, so a row appears the moment its
// entity does and an edit redraws the tree without a round trip. What this adds
// to those rows is everything the *frame* knows and the query doesn't — which
// row is selected, which is being typed into, and where the box for a new child
// goes.

export type { EntitySummary }
export { str, summaryOf }

// --- Rows -------------------------------------------------------------------

/**
 * A rendered bullet backed by a real entity: what the query says about it (its
 * {@link TreeRow} — text, type, where it sits, and for a file its mime type,
 * which is on the entity as well as the resource so the row knows what it is
 * about to show before the bytes load) plus what the frame says about it.
 */
export interface EntityRow extends TreeRow {
  kind: 'entity'
  selected: boolean
  /** True while this row's text is being edited in place. */
  editing: boolean
  /** The persisted draft, while editing. */
  draft?: string
}

/**
 * The transient row shown while creating a child entity. It carries the values
 * the create was started with, so the row it is about to become — a section, a
 * checkbox, a code block — looks like itself while it is being typed.
 */
export interface InputRow {
  kind: 'input'
  depth: number
  draft: string
  type?: string
  section?: boolean
  open?: boolean
}

export type Row = EntityRow | InputRow

export interface FrameRows {
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
 * falling back to the frame's root. While pages are still outstanding an unfound
 * path is left alone rather than snapped, since the row it names may yet arrive.
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
 * A frame's rows before the frame's own cursor is on them: the traversal, the
 * filters, and one row object per line.
 *
 * Split from {@link markRows} because the selection lives on the frame, so moving
 * the cursor changes the state every row is derived from. Rebuilding all of it
 * then meant re-walking the graph and re-reading every entity to draw the same
 * rows with a different one highlighted — which is what made holding a movement
 * key down slow once a parent had a couple of hundred children.
 */
export interface FrameTree {
  /** Selected and editing are false on all of them; that is `markRows`'s to say. */
  rows: EntityRow[]
  loading: boolean
  error: string | null
  complete: boolean
}

export const EMPTY_FRAME_TREE: FrameTree = {
  rows: [],
  loading: false,
  error: null,
  complete: true,
}

/** Everything about a frame's rows that its selection cannot change. */
export function frameTree(
  frame: FrameState,
  collapsed: readonly string[],
  source: EntitySource,
  limit: number,
): FrameTree {
  const { rows, complete, loading, error } = buildTree(
    [frame.rootId],
    source,
    {
      direction: directionOf(frame),
      collapsed: collapsedBelow(collapsed, frame.rootId),
      maxDepth: frame.maxDepth,
    },
    limit,
    { find: frame.find, sections: frame.sectionsOnly },
  )
  return {
    rows: rows.map((row): EntityRow => ({ kind: 'entity', ...row, selected: false, editing: false })),
    loading,
    error,
    complete,
  }
}

/**
 * A frame's tree with what the frame knows laid over it: which row is selected,
 * which is being typed into, and where the box for a new child goes.
 *
 * Rows the cursor doesn't touch are passed through *by identity* rather than
 * copied, so a keystroke that moves the selection re-renders the two rows that
 * changed instead of every row on screen.
 */
export function markRows(tree: FrameTree, frame: FrameState): FrameRows {
  const { rows, complete, loading, error } = tree

  // A path that isn't among the rows may simply not have arrived yet, so the
  // selection is only snapped once the frame has everything it is going to get.
  const settled = complete && !loading
  const selectedPath = resolveSelectedPath(frame.selectedPath, rows, frame.rootId, settled)
  const edit = frame.edit
  const marked: Row[] = rows.map((row): Row => {
    const editing = edit?.mode === 'edit' && samePath(row.path, edit.path)
    const selected = samePath(row.path, selectedPath)
    if (!editing && !selected) return row
    return { ...row, selected, editing, draft: editing ? edit?.draft : undefined }
  })

  // Splice the "new child" input in after the parent's whole subtree — which for
  // a folded parent is nothing, so it lands directly beneath it.
  if (edit?.mode === 'create') {
    const at = marked.findIndex((r) => r.kind === 'entity' && samePath(r.path, edit.path))
    if (at >= 0) {
      const parentDepth = marked[at].depth
      let insert = at + 1
      while (insert < marked.length && marked[insert].depth > parentDepth) insert++
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

/**
 * Build a frame's rows in one go. What a caller outside React wants — a tool, the
 * call context — since it has nothing to memoise the halves against and is
 * reading once rather than on every keystroke.
 */
export const buildRows = (
  frame: FrameState,
  collapsed: readonly string[],
  source: EntitySource,
  limit: number,
): FrameRows => markRows(frameTree(frame, collapsed, source, limit), frame)

export const EMPTY_FRAME_ROWS: FrameRows = {
  rows: [],
  selectedPath: [],
  loading: false,
  error: null,
  complete: true,
}

/** A frame's rows, given the whole latent state and a read of the cache. */
export function frameRows(
  s: LayoutState,
  source: EntitySource,
  frameId: string | null,
  limit: number,
): FrameRows {
  const frame = frameId ? s.frames[frameId] : null
  if (!frame) return EMPTY_FRAME_ROWS
  return buildRows(frame, s.tabs[frame.tabId]?.collapsed ?? [], source, limit)
}

/** Only the entity rows, in order — what selection movement steps through. */
export const entityRows = (rows: Row[]): EntityRow[] =>
  rows.filter((r): r is EntityRow => r.kind === 'entity')

// --- Call context -----------------------------------------------------------

/**
 * The context a call is born with. Two layers:
 *
 * 1. Entity values, folded along the path the user is looking at — every frame
 *    root in the tab's stack (outermost first), then the selection path inside
 *    the top frame. Later entries win, so the selected entity's values take
 *    precedence; `null` values are skipped rather than folded.
 * 2. The positional keys arguments actually reference (`entityId`, `parentId`,
 *    …), which override any same-named entity value, and then `extra` — what a
 *    right-click supplies, which need not be the current selection.
 *
 * `autofill` says whether the result applies itself to a tool's arguments or is
 * merely offered to them; see {@link CallContext}.
 *
 * `within` names a path inside the top frame other than the selection — the row
 * a gesture landed on rather than the row the keyboard is on. The frame is still
 * the focused one: every such gesture starts with a mousedown, which selects the
 * group it is in before the click is handled.
 *
 * `rows` is the focused frame's, which is where the resolved selection comes
 * from; it is passed in rather than rebuilt because the caller has just built it.
 */
export function buildCallContext(
  s: LayoutState,
  source: EntitySource,
  rows: FrameRows,
  opts: { extra?: Record<string, unknown>; autofill?: boolean; within?: string[] } = {},
): CallContext {
  const { groupId, tabId, frameId } = focusOf(s)
  const tab = tabId ? s.tabs[tabId] : null
  const frame = frameId ? s.frames[frameId] : null
  const selectedPath = opts.within ?? rows.selectedPath

  const stackRoots = (tab?.frameIds ?? []).map((id) => s.frames[id]?.rootId).filter(Boolean) as string[]
  const path = [...stackRoots, ...selectedPath]
  const values: Record<string, unknown> = {}
  // Reading these is also what asks for them, so an outer frame that isn't
  // mounted contributes nothing the first time and its own values the next.
  const folded = source.get(path)
  for (const id of path) {
    for (const [k, v] of Object.entries(folded[id]?.values ?? {})) {
      if (v !== null) values[k] = v
    }
  }

  const selectedId = last(selectedPath)
  const parentId = selectedPath.length > 1 ? selectedPath[selectedPath.length - 2] : undefined
  const positional: Record<string, unknown> = {
    ...(selectedId ? { entityId: selectedId } : {}),
    ...(parentId ? { parentId } : {}),
    ...(frame ? { rootId: frame.rootId } : {}),
    ...(frameId ? { frameId } : {}),
    ...(tabId ? { tabId } : {}),
    ...(groupId ? { groupId } : {}),
  }

  return {
    values: { ...values, ...positional, ...(opts.extra ?? {}) },
    ...(opts.autofill === false ? { autofill: false } : {}),
    path,
    groupId,
    tabId,
    frameId,
    startedAt: Date.now(),
  }
}

// --- Labels -----------------------------------------------------------------

/** What to call a file that has no caption of its own: `image/png` → "PNG image". */
function fileLabel(mimeType?: string): string {
  const subtype = mimeType?.split('/')[1]?.split('+')[0]?.replace(/[^a-z0-9]/gi, '')
  if (!subtype) return 'File'
  return `${subtype.toUpperCase()} ${mimeType?.startsWith('image/') ? 'image' : 'file'}`
}

/**
 * An entity's display name: its text, or what it is when it has none — a file's
 * kind, "Index" at the root — and failing all of that the raw id, until it has
 * loaded. A file's real name lives with its bytes rather than on the entity, so
 * a caller holding the resource can do better than this.
 *
 * Reading the cache is also what asks for it, so naming an entity is enough to
 * make it load: a tab whose entity has never been on screen shows its id for one
 * frame and its text thereafter.
 */
export function entityLabel(source: EntitySource, id: string): string {
  const summary = summaryOf(source.get([id])[id].values)
  if (summary.text) return summary.text
  if (summary.type === 'file') return fileLabel(summary.mimeType)
  return id === ROOT_ID ? 'Index' : id
}

/** The entity a tab is showing: the root of its top frame. */
export function tabRootId(s: LayoutState, tabId: string): string | undefined {
  const topId = last(s.tabs[tabId]?.frameIds ?? [])
  return topId ? s.frames[topId]?.rootId : undefined
}

/** One step of a tab's frame stack, outermost first — the tab's breadcrumb. */
export interface Crumb {
  frameId: string
  /** The entity the frame is rooted at, which is what the crumb stands for. */
  rootId: string
  label: string
}

/**
 * The tab's frame stack as a trail of names. It is the route the user took to
 * get here (each `d` pushed a frame), not the entity's ancestry in the graph —
 * which is the same thing when you drilled in, and the honest answer when you
 * didn't.
 */
export function frameCrumbs(s: LayoutState, source: EntitySource, tabId: string | null): Crumb[] {
  const tab = tabId ? s.tabs[tabId] : null
  if (!tab) return []
  return tab.frameIds
    .map((frameId) => ({ frameId, rootId: s.frames[frameId]?.rootId }))
    .filter((f): f is { frameId: string; rootId: string } => !!f.rootId)
    .map(({ frameId, rootId }) => ({ frameId, rootId, label: entityLabel(source, rootId) }))
}
