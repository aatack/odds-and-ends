import type { LinkDirection, QueryResult } from '../../../core/wrapper'
import {
  NO_PAGE,
  cachedValues,
  str,
  summaryOf,
  type EntitySummary,
  type FramePage,
  type QueryCache,
} from './query'
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

// Everything derived from latent state plus the query cache: the flat row list a
// frame renders, the selection actually in effect, and the context a call is
// born with. All pure functions — the views and the tools call the same ones, so
// what the user sees and what a tool acts on can't disagree.

// --- Rows -------------------------------------------------------------------

/**
 * A rendered bullet backed by a real entity: what the entity says about itself
 * (its {@link EntitySummary} — text, type, and for a file its mime type, which is
 * on the entity as well as the resource so the row knows what it is about to show
 * before the bytes load) plus where it sits in the frame.
 */
export interface EntityRow extends EntitySummary {
  kind: 'entity'
  id: string
  /** Depth within the query (0 = root). */
  depth: number
  /** Ids from the root to this row — its identity, since ids repeat. */
  path: string[]
  /** When true the text renders as a section heading. */
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
  loading: boolean
  error: string | null
  /** True when the whole tree has been fetched (no further pages). */
  complete: boolean
}

const key = (path: readonly string[]): string => path.join('\0')

/**
 * Walk the query results into a flat list, tracking each row's full path.
 * `direction` is the one the query ran in, so "has children" means "has more
 * rows under it here" rather than always meaning outbound links.
 */
function walk(
  results: QueryResult[],
  collapsed: Set<string>,
  direction: LinkDirection,
): EntityRow[] {
  const out: EntityRow[] = []
  const stack: string[] = []
  for (const { entity, depth } of results) {
    stack.length = depth
    stack.push(entity.id)
    const open = entity.values.open
    out.push({
      kind: 'entity',
      id: entity.id,
      depth,
      path: stack.slice(),
      ...summaryOf(entity.values),
      section: entity.values.section === true,
      open: open === true ? true : open === false ? false : undefined,
      hasChildren:
        (direction === 'in' ? entity.inboundLinks : entity.outboundLinks).length > 0,
      collapsed: collapsed.has(entity.id),
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
 * Keep the section rows, plus the frame's root — the tree read as a table of
 * contents. Unlike find, non-matching ancestors are dropped rather than kept:
 * the point is to see the sections and nothing else. Rows keep their real depth,
 * so a section nested inside an ordinary entity still reads as nested. The root
 * stays whatever it is, so the frame keeps its anchor and the selection has
 * somewhere to fall back to.
 */
const onlySections = (rows: EntityRow[]): EntityRow[] =>
  rows.filter((r) => r.depth === 0 || r.section)

/**
 * The selection actually in effect. Strips trailing ids until the path exists,
 * falling back to the frame's root. While pages are still outstanding an unfound
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

/** Build a frame's rows from its latent state and whatever the cache holds. */
export function buildRows(
  frame: FrameState,
  collapsed: readonly string[],
  page: FramePage,
): FrameRows {
  const complete = !page.loading && page.continuation == null
  let rows = walk(page.results, new Set(collapsedBelow(collapsed, frame.rootId)), directionOf(frame))
  if (frame.find != null) rows = applyFind(rows, frame.find)
  if (frame.sectionsOnly) rows = onlySections(rows)

  const selectedPath = resolveSelectedPath(frame.selectedPath, rows, frame.rootId, complete)
  const edit = frame.edit
  const marked: Row[] = rows.map((row) => {
    const editing = edit?.mode === 'edit' && samePath(row.path, edit.path)
    return {
      ...row,
      selected: samePath(row.path, selectedPath),
      editing,
      draft: editing ? edit?.draft : undefined,
    }
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

  return { rows: marked, selectedPath, loading: page.loading, error: page.error, complete }
}

/** A frame's rows, given the whole latent state and cache. */
export function frameRows(s: LayoutState, cache: QueryCache, frameId: string | null): FrameRows {
  const frame = frameId ? s.frames[frameId] : null
  if (!frame) return { rows: [], selectedPath: [], loading: false, error: null, complete: true }
  return buildRows(frame, s.tabs[frame.tabId]?.collapsed ?? [], cache[frame.id] ?? NO_PAGE)
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
 */
export function buildCallContext(
  s: LayoutState,
  cache: QueryCache,
  opts: { extra?: Record<string, unknown>; autofill?: boolean; within?: string[] } = {},
): CallContext {
  const { groupId, tabId, frameId } = focusOf(s)
  const tab = tabId ? s.tabs[tabId] : null
  const frame = frameId ? s.frames[frameId] : null
  const { selectedPath: resolved } = frameRows(s, cache, frameId)
  const selectedPath = opts.within ?? resolved

  const stackRoots = (tab?.frameIds ?? []).map((id) => s.frames[id]?.rootId).filter(Boolean) as string[]
  const path = [...stackRoots, ...selectedPath]
  const values: Record<string, unknown> = {}
  for (const id of path) {
    const entity = cachedValues(cache, id)
    if (!entity) continue
    for (const [k, v] of Object.entries(entity)) {
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
 * An entity's display name, from the summaries harvested by the query engine: its
 * text, or what it is when it has none — a file's kind, "Index" at the root — and
 * failing all of that the raw id, until something has loaded it. A file's real
 * name lives with its bytes rather than on the entity, so a caller holding the
 * resource can do better than this.
 */
export function entityLabel(summaries: Record<string, EntitySummary>, id: string): string {
  const summary = summaries[id]
  if (summary?.text) return summary.text
  if (summary?.type === 'file') return fileLabel(summary.mimeType)
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
export function frameCrumbs(
  s: LayoutState,
  summaries: Record<string, EntitySummary>,
  tabId: string | null,
): Crumb[] {
  const tab = tabId ? s.tabs[tabId] : null
  if (!tab) return []
  return tab.frameIds
    .map((frameId) => ({ frameId, rootId: s.frames[frameId]?.rootId }))
    .filter((f): f is { frameId: string; rootId: string } => !!f.rootId)
    .map(({ frameId, rootId }) => ({ frameId, rootId, label: entityLabel(summaries, rootId) }))
}
