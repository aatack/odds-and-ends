import { str, summaryOf, type EntitySummary } from '../../../core/entity'
import type { EntitySource, QueryFilters, Traversal } from '../../../core/query'
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
  /**
   * Each row's key, in row order. Index-aligned with {@link rows}, and stable
   * while the tree is: a view can measure and memoise against these without
   * being disturbed by the cursor moving.
   */
  keys: string[]
  /** The selection in effect — resolved against the visible rows. Never stored. */
  selectedPath: string[]
  /** Where that selection sits in {@link rows}, or -1 when it isn't among them. */
  selectedIndex: number
  /** The row being typed into — an edited row, or the box for a new child. */
  editIndex: number
  /** True while any row's entity is still being read. */
  loading: boolean
  error: string | null
  /** True when the traversal ran out rather than hitting the row limit. */
  complete: boolean
}

/**
 * A row's identity as one string. The path rather than the id, since the same
 * entity can appear in several places; joined on a NUL, which no id can contain,
 * so two different paths can never come out the same.
 *
 * This is what makes finding a row a lookup rather than a search. Everything that
 * wants to know *where* a path is — the selection, the edit, a view's measured
 * heights — goes through these.
 */
export const rowKey = (path: readonly string[]): string => path.join('\0')

/** The key of the box shown while creating a child of `parentKey`. */
const inputKey = (parentKey: string): string => `\0input\0${parentKey}`

/**
 * The selection actually in effect. Strips trailing ids until the path is one of
 * the rows, falling back to the frame's root. While pages are still outstanding
 * an unfound path is left alone rather than snapped, since the row it names may
 * yet arrive.
 *
 * Takes the tree's key → index map rather than the rows: this runs on every
 * keystroke, and building a set of every row's path to answer it — which is what
 * it used to do — is the sort of per-row work that made a long frame lag.
 */
export function resolveSelectedPath(
  latent: string[],
  at: ReadonlyMap<string, number>,
  rootId: string,
  complete: boolean,
): string[] {
  if (at.has(rowKey(latent))) return latent
  if (!complete) return latent
  let path = latent
  while (path.length > 0 && !at.has(rowKey(path))) path = path.slice(0, -1)
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
  /** Row keys in row order, computed once with the rows. */
  keys: string[]
  /** key → index in {@link rows}. Computed once, so a lookup costs nothing later. */
  at: Map<string, number>
  loading: boolean
  error: string | null
  complete: boolean
}

export const EMPTY_FRAME_TREE: FrameTree = {
  rows: [],
  keys: [],
  at: new Map(),
  loading: false,
  error: null,
  complete: true,
}

// Every traversal a frame runs comes through `frameTree`, so one hook there sees
// all of them: the render path, and every live read a tool or a call context
// makes. It exists for the tests, which count them — that is how "moving the
// cursor does not re-resolve the query" is asserted rather than hoped for. Nothing
// in the app sets it: a console call here runs on every traversal, and
// `console.trace` in particular is dear enough to be the lag it was looking for.
let observer: ((frame: FrameState, limit: number) => void) | null = null

export const setQueryObserver = (
  fn: ((frame: FrameState, limit: number) => void) | null,
): void => {
  observer = fn
}

/**
 * The walk a frame asks for: where it goes, and what it keeps. Named because two
 * callers need it — the rows, and anything that wants to know where the walk
 * *ends* without running it — and a second copy of this is a jump that lands
 * somewhere the rows don't go.
 */
export const frameQuery = (
  frame: FrameState,
  collapsed: readonly string[],
): { traversal: Traversal; filters: QueryFilters } => ({
  traversal: {
    direction: directionOf(frame),
    collapsed: collapsedBelow(collapsed, frame.rootId),
    // The frame's own limit is already in here, under its root: the sections
    // filter puts one there rather than implying one of its own.
    maxDepth: frame.maxDepth,
  },
  filters: { find: frame.find, sections: frame.sectionsOnly, open: frame.openOnly },
})

/** Everything about a frame's rows that its selection cannot change. */
export function frameTree(
  frame: FrameState,
  collapsed: readonly string[],
  source: EntitySource,
  limit: number,
): FrameTree {
  observer?.(frame, limit)
  const { traversal, filters } = frameQuery(frame, collapsed)
  const { rows, complete, loading, error } = buildTree(
    [frame.rootId],
    source,
    traversal,
    limit,
    filters,
  )

  // Keys and their index, once, here — where the rows are built and not again
  // until they are. Everything downstream looks a row up rather than hunting it.
  const keys = new Array<string>(rows.length)
  const at = new Map<string, number>()
  const marked = new Array<EntityRow>(rows.length)
  for (let i = 0; i < rows.length; i++) {
    const key = rowKey(rows[i].path)
    keys[i] = key
    at.set(key, i)
    marked[i] = { kind: 'entity', ...rows[i], selected: false, editing: false }
  }
  return { rows: marked, keys, at, loading, error, complete }
}

/**
 * A frame's tree with what the frame knows laid over it: which row is selected,
 * which is being typed into, and where the box for a new child goes.
 *
 * This runs on every keystroke, so it does no per-row work at all. Both the
 * selection and the edit are found by looking their key up in the tree's index,
 * and the rows are the tree's own array with *at most two entries replaced* — so
 * a memoised row component re-renders only where something actually changed.
 */
export function markRows(tree: FrameTree, frame: FrameState): FrameRows {
  const { rows, complete, loading, error } = tree

  // A path that isn't among the rows may simply not have arrived yet, so the
  // selection is only snapped once the frame has everything it is going to get.
  const settled = complete && !loading
  const selectedPath = resolveSelectedPath(frame.selectedPath, tree.at, frame.rootId, settled)
  const edit = frame.edit

  let selectedIndex = tree.at.get(rowKey(selectedPath)) ?? -1
  let editIndex = edit?.mode === 'edit' ? (tree.at.get(rowKey(edit.path)) ?? -1) : -1

  const marked: Row[] = rows.slice()
  let keys = tree.keys
  if (selectedIndex >= 0) marked[selectedIndex] = { ...rows[selectedIndex], selected: true }
  if (editIndex >= 0) {
    marked[editIndex] = { ...marked[editIndex], editing: true, draft: edit?.draft } as EntityRow
  }

  // Splice the "new child" input in after the parent's whole subtree — which for
  // a folded parent is nothing, so it lands directly beneath it. The keys are
  // copied too, since a view reads them index for index with the rows.
  if (edit?.mode === 'create') {
    const parentKey = rowKey(edit.path)
    const parent = tree.at.get(parentKey) ?? -1
    if (parent >= 0) {
      const parentDepth = marked[parent].depth
      let insert = parent + 1
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
      keys = tree.keys.slice()
      keys.splice(insert, 0, inputKey(parentKey))
      // Everything at or after the insert has shifted down one.
      if (selectedIndex >= insert) selectedIndex++
      editIndex = insert
    }
  }

  return { rows: marked, keys, selectedPath, selectedIndex, editIndex, loading, error, complete }
}

export const EMPTY_FRAME_ROWS: FrameRows = {
  rows: [],
  keys: [],
  selectedPath: [],
  selectedIndex: -1,
  editIndex: -1,
  loading: false,
  error: null,
  complete: true,
}

// Note what is not here: a function that resolves a frame and marks it in one go.
// It read well and was the wrong shape — every caller of it resolved the tree
// afresh, which is exactly what a cursor move must not do. `state/query` puts the
// two halves together over a memo of the first, and is the only way in.

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
