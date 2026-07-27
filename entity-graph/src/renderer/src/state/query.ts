import { atom } from './atom'
import { buildCallContext, frameRows, type FrameRows } from './derive'
import { entities } from '../../../core/cache'
import type { EntitySource } from '../../../core/query'
import { focusOf, getLayout, layoutAtom } from './store'
import type { CallContext, LayoutState } from './types'

// A frame's query, which is no longer a fetch: the traversal in core/query runs
// over the entity cache, so a frame's rows are a *derivation* like any other and
// the only thing left to keep here is how much of the tree has been unrolled.
//
// That is what pagination has become. Nothing is fetched a page at a time any
// more — the cache asks for whatever the rows mention — so a page is simply a
// ceiling on how far the traversal walks before it stops, raised when the view
// scrolls near the bottom.

/** Rows unrolled per page. The first is the frame's whole budget until it scrolls. */
export const PAGE_SIZE = 200

/**
 * frame id → how many rows its traversal may produce. Runtime only, and only
 * ever raised: an entry left behind by a closed frame costs one number.
 */
export const rowLimitsAtom = atom<Record<string, number>>({})

export const rowLimit = (frameId: string | null): number =>
  (frameId ? rowLimitsAtom.get()[frameId] : undefined) ?? PAGE_SIZE

/**
 * Unroll another page of a frame's tree, if the traversal stopped at the limit
 * rather than because there was nothing more. A frame still waiting on entities
 * looks finished — an entity nobody has read yet has no children — so it doesn't
 * grow here; it grows when they arrive, and this is asked again on the next
 * scroll.
 */
export function loadMore(frameId: string): void {
  if (rowsOf(frameId).complete) return
  rowLimitsAtom.set((limits) => ({
    ...limits,
    [frameId]: (limits[frameId] ?? PAGE_SIZE) + PAGE_SIZE,
  }))
}

/** A frame's rows, against a read of the cache and the limits already in hand. */
export const rowsFrom = (
  s: LayoutState,
  source: EntitySource,
  limits: Record<string, number>,
  frameId: string | null,
): FrameRows =>
  frameRows(s, source, frameId, (frameId ? limits[frameId] : undefined) ?? PAGE_SIZE)

/** A frame's rows as they stand — the live counterpart to `useFrameRows`. */
export const rowsOf = (frameId: string | null, s: LayoutState = getLayout()): FrameRows =>
  rowsFrom(s, entities(), rowLimitsAtom.get(), frameId)

/**
 * The context a call would be born in right now. Lives here rather than in
 * `derive` because it needs the focused frame's rows, and how far those are
 * unrolled is this module's business.
 */
export function liveContext(
  opts: { extra?: Record<string, unknown>; autofill?: boolean; within?: string[] } = {},
): CallContext {
  const s = getLayout()
  const source = entities()
  const { frameId } = focusOf(s)
  return buildCallContext(s, source, rowsFrom(s, source, rowLimitsAtom.get(), frameId), opts)
}

// A frame whose root or filters changed is not a different query to be refetched
// any more — it is the same derivation over the same cache. The one thing worth
// noticing is a frame going away, since its unroll budget should not be inherited
// by whatever id React hands out next.
layoutAtom.subscribe(() => {
  const limits = rowLimitsAtom.get()
  const ids = Object.keys(limits)
  if (!ids.length) return
  const frames = getLayout().frames
  if (ids.every((id) => frames[id])) return
  rowLimitsAtom.set(Object.fromEntries(ids.filter((id) => frames[id]).map((id) => [id, limits[id]])))
})
