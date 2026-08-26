import { atom } from './atom'
import {
  EMPTY_FRAME_ROWS,
  EMPTY_FRAME_TREE,
  buildCallContext,
  frameTree,
  markRows,
  type FrameRows,
  type FrameTree,
} from './derive'
import { entities, entitiesAtom, entitiesFrom, type EntityCache } from '../../../core/cache'
import { focusOf, getLayout, layoutAtom } from './store'
import type { CallContext, FrameState, LayoutState } from './types'

// A frame's query, which is no longer a fetch: the traversal in core/query runs
// over the entity cache, so a frame's rows are a *derivation* like any other and
// what is left to keep here is how much of the tree has been unrolled, and the
// last answer for each frame.
//
// That is what pagination has become. Nothing is fetched a page at a time any
// more — the cache asks for whatever the rows mention — so a page is simply a
// ceiling on how far the traversal walks before it stops, raised when the view
// scrolls near the bottom.

/** Rows unrolled per page. The first is the frame's whole budget until it scrolls. */
export const PAGE_SIZE = 200

/**
 * What a frame's budget was raised against — everything that decides which
 * entities the walk visits. A budget belongs to *that* query and not to the
 * frame: filling a screen off a narrow filter can raise the ceiling a long way,
 * and inheriting that when the filter is cleared would leave the frame resolving
 * every row it had to walk past to find three.
 */
const shapeOf = (frame: FrameState): string =>
  JSON.stringify([
    frame.rootId,
    frame.direction,
    frame.find,
    frame.sectionsOnly,
    frame.openOnly,
    frame.maxDepth,
  ])

/** How far one frame's traversal may walk, and the query that was raised for. */
export interface RowBudget {
  shape: string
  limit: number
}

/**
 * frame id → its budget. Runtime only: an entry left behind by a closed frame
 * costs one number.
 */
export const rowLimitsAtom = atom<Record<string, RowBudget>>({})

/** A frame's budget: what was raised for this query, or a fresh page for a new one. */
const limitOf = (frame: FrameState, limits: Record<string, RowBudget>): number => {
  const held = limits[frame.id]
  return held && held.shape === shapeOf(frame) ? held.limit : PAGE_SIZE
}

/**
 * Unroll more of a frame's tree, if the traversal stopped at the limit rather
 * than because there was nothing more. A frame still waiting on entities looks
 * finished — an entity nobody has read yet has no children — so it doesn't grow
 * here; it grows when they arrive, and this is asked again on the next scroll.
 *
 * The ceiling *doubles* rather than going up by a page. The limit is on the walk
 * and not on the rows it keeps, so a filtered frame can ask for another page and
 * get no more rows at all — and it is then asked again, since the screen is still
 * not full. Growing by a fixed page there means re-walking the tree once per two
 * hundred entities until the filter finally yields something, which for a narrow
 * filter over a wide tree is most of the store walked over and over. Doubling
 * makes that a handful of walks. Scrolling pays the same way round: a frame the
 * user has already read a thousand rows of asks for a thousand more.
 */
export function loadMore(frameId: string): void {
  const frame = getLayout().frames[frameId]
  if (!frame || rowsOf(frameId).complete) return
  rowLimitsAtom.set((limits) => {
    const at = limitOf(frame, limits)
    return { ...limits, [frameId]: { shape: shapeOf(frame), limit: at + Math.max(PAGE_SIZE, at) } }
  })
}

/** One array, so a tab with nothing folded doesn't look like a change. */
const EMPTY_COLLAPSED: readonly string[] = []

/**
 * The last traversal each frame resolved, with the values it was resolved
 * against. Runtime only, one entry per open frame, dropped when the frame goes.
 *
 * Here rather than in a `useMemo` because React is not the only caller: a tool
 * reads the rows to find out which one comes after the selection, and it reads
 * them through `rowsOf`, outside any render. So a keypress that moved the cursor
 * resolved the whole query again even once the render path stopped doing so —
 * the memo has to be somewhere both of them can see, and then they share one
 * answer rather than each having their own.
 */
const trees = new Map<string, { against: readonly unknown[]; tree: FrameTree }>()

const unchanged = (a: readonly unknown[], b: readonly unknown[]): boolean =>
  a.length === b.length && a.every((v, i) => v === b[i])

/**
 * A frame's traversal, resolved or remembered. What it is resolved against is
 * everything that can change the *shape* of the tree — and pointedly not the
 * selection or the edit, which are laid over the result by `markRows` and are why
 * this exists: moving the cursor changes the frame, and the frame is the thing a
 * naive memo would key on.
 */
export function treeOf(
  s: LayoutState,
  cache: EntityCache,
  limits: Record<string, RowBudget>,
  frameId: string | null,
): FrameTree {
  const frame = frameId ? s.frames[frameId] : null
  if (!frame) return EMPTY_FRAME_TREE
  const collapsed = s.tabs[frame.tabId]?.collapsed ?? EMPTY_COLLAPSED
  const limit = limitOf(frame, limits)
  const against = [
    frame.rootId,
    frame.direction,
    frame.find,
    frame.sectionsOnly,
    frame.openOnly,
    frame.maxDepth,
    collapsed,
    // The cache rather than a source wrapping it: `entities()` hands back a fresh
    // object every call, while the cache only changes identity when it changes.
    cache,
    limit,
  ]
  const held = trees.get(frame.id)
  if (held && unchanged(held.against, against)) return held.tree

  const tree = frameTree(frame, collapsed, entitiesFrom(cache), limit)
  trees.set(frame.id, { against, tree })
  return tree
}

/** A frame's rows, against a read of the cache and the limits already in hand. */
export const rowsFrom = (
  s: LayoutState,
  cache: EntityCache,
  limits: Record<string, RowBudget>,
  frameId: string | null,
): FrameRows => {
  const frame = frameId ? s.frames[frameId] : null
  const tree = treeOf(s, cache, limits, frameId)
  return frame ? markRows(tree, frame) : EMPTY_FRAME_ROWS
}

/** A frame's rows as they stand — the live counterpart to `useFrameRows`. */
export const rowsOf = (frameId: string | null, s: LayoutState = getLayout()): FrameRows =>
  rowsFrom(s, entitiesAtom.get(), rowLimitsAtom.get(), frameId)

/**
 * The context a call would be born in right now. Lives here rather than in
 * `derive` because it needs the focused frame's rows, and how far those are
 * unrolled is this module's business.
 */
export function liveContext(
  opts: { extra?: Record<string, unknown>; autofill?: boolean; within?: string[] } = {},
): CallContext {
  const s = getLayout()
  const cache = entitiesAtom.get()
  const { frameId } = focusOf(s)
  // The rows come from the memo; the source is for folding values along the path
  // the context is built from, which is a handful of entities rather than a walk.
  return buildCallContext(s, entities(), rowsFrom(s, cache, rowLimitsAtom.get(), frameId), opts)
}

// A frame whose root or filters changed is not a different query to be refetched
// any more — it is the same derivation over the same cache. Its budget does start
// over, but that needs nothing here: a budget carries the query it was raised for
// and is simply not the frame's any more once it asks a different one. The one
// thing worth noticing is a frame going away, since neither its unroll budget nor
// its last answer should be inherited by whatever id React hands out next.
layoutAtom.subscribe(() => {
  const frames = getLayout().frames
  for (const id of trees.keys()) if (!frames[id]) trees.delete(id)

  const limits = rowLimitsAtom.get()
  const ids = Object.keys(limits)
  if (!ids.length) return
  if (ids.every((id) => frames[id])) return
  rowLimitsAtom.set(Object.fromEntries(ids.filter((id) => frames[id]).map((id) => [id, limits[id]])))
})
