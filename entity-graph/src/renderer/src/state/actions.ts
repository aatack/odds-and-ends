import * as P from './peeks'
import { transientPeek } from './peeks'
import * as R from './reducers'
import { getLayout, updateLayout } from './store'
import type { EditState, FrameState, FrameType, LayoutState, LinkDirection, Rect } from './types'

// Named mutators over the latent state. Two callers:
//
//  - tools, which are the invocable, loggable, key-bound surface;
//  - views, for direct manipulation — clicking a row, dragging a tab, typing in
//    the in-place editor — where routing a mouse gesture through the call
//    machine would put noise in the log for no gain.
//
// Either way the reducers stay the only thing that knows the shape of the state.

export const selectPath = (frameId: string, path: string[]): void =>
  updateLayout((s) => R.setSelectedPath(s, frameId, path))

export const toggleCollapse = (tabId: string, entityId: string): void =>
  updateLayout((s) => R.toggleCollapsed(s, tabId, entityId))

export const setCollapsed = (tabId: string, entityId: string, collapsed: boolean): void =>
  updateLayout((s) => R.setCollapsed(s, tabId, entityId, collapsed))


export const setFind = (frameId: string, find: string | null): void =>
  updateLayout((s) => R.setFind(s, frameId, find))

export const setSectionsOnly = (frameId: string, on: boolean): void =>
  updateLayout((s) => R.setSectionsOnly(s, frameId, on))

export const setOpenOnly = (frameId: string, on: boolean): void =>
  updateLayout((s) => R.setOpenOnly(s, frameId, on))

export const setDirection = (frameId: string, direction: LinkDirection): void =>
  updateLayout((s) => R.setDirection(s, frameId, direction))

export const setMaxDepth = (frameId: string, entityId: string, depth: number | null): void =>
  updateLayout((s) => R.setMaxDepth(s, frameId, entityId, depth))

/** The frame's own depth limit, below its root. Null takes it off. */
export const setFrameDepth = (frameId: string, depth: number | null): void =>
  updateLayout((s) => R.setFrameDepth(s, frameId, depth))

// --- Editing ----------------------------------------------------------------

export const setEdit = (frameId: string, edit: EditState | null): void =>
  updateLayout((s) => R.setEdit(s, frameId, edit))

/** Update the persisted draft of whatever edit is in progress. */
export const setDraft = (frameId: string, draft: string): void =>
  updateLayout((s) =>
    R.updateFocus(s, frameId, (f) => (f.edit ? { ...f, edit: { ...f.edit, draft } } : f)),
  )

export const startEdit = (frameId: string, path: string[], text: string): void =>
  setEdit(frameId, { mode: 'edit', path, draft: text })

/**
 * Begin creating a child; `values` are written alongside the text on commit.
 *
 * A folded parent stays folded: the input row is spliced in after whatever of
 * the parent's subtree is on screen, which for a folded one is nothing at all,
 * so it lands directly beneath the parent and is visible either way. Unfolding
 * would drag the rest of the subtree into view purely as a side effect of
 * adding to it.
 */
export function startCreate(
  frameId: string,
  path: string[],
  values: Record<string, unknown> = {},
): void {
  if (!path.length) return
  setEdit(frameId, { mode: 'create', path, draft: '', values })
}

// --- Structure --------------------------------------------------------------

export const selectTab = (groupId: string, tabId: string): void =>
  updateLayout((s) => R.selectTab(s, groupId, tabId))

export const selectGroup = (groupId: string): void => updateLayout((s) => R.selectGroup(s, groupId))

export const closeTab = (groupId: string, tabId: string): void =>
  updateLayout((s) => R.closeTab(s, groupId, tabId))

export const addTab = (groupId: string, rootId?: string): void =>
  updateLayout((s) => R.addTab(s, groupId, rootId))

export const pushFrame = (tabId: string, rootId: string, type: FrameType = 'focus'): void =>
  updateLayout((s) => R.pushFrame(s, tabId, rootId, type))

export const setDetailDraft = (frameId: string, draft: Record<string, unknown> | null): void =>
  updateLayout((s) => R.setDetailDraft(s, frameId, draft))

export const popToFrame = (tabId: string, frameId: string): void =>
  updateLayout((s) => R.popToFrame(s, tabId, frameId))

/** The frame a tool is acting on, resolved from the current focus. */
export const frameOf = (s: LayoutState, frameId: string | null): FrameState | null =>
  frameId ? (s.frames[frameId] ?? null) : null

// --- Peeks ------------------------------------------------------------------

// Hovering is direct manipulation with a clock in it: a peek opens a moment after
// the pointer settles on something, so passing over a pill on the way somewhere
// else opens nothing, and closes a moment after the pointer leaves, so crossing
// the gap between a pill and its peek doesn't close it. The timers are runtime
// only; what they decide is a reducer like any other.

const OPEN_AFTER = 350
const CLOSE_AFTER = 300

/** The peek the pointer is in, or null for the main view. */
let hovering: string | null = null
let opening: ReturnType<typeof setTimeout> | null = null
let closing: ReturnType<typeof setTimeout> | null = null

const holdPeeks = (): void => {
  if (closing) clearTimeout(closing)
  closing = null
}

/**
 * The pointer settled on something peekable. `origin` is the peek it is drawn
 * in (null: the main view), which the new one stacks on.
 */
export function hoverPeek(entityId: string, anchor: Rect, origin: string | null): void {
  hovering = origin
  holdPeeks()
  if (opening) clearTimeout(opening)
  opening = null
  const top = transientPeek(getLayout())
  if (top && top.parent === origin && top.entityId === entityId) return
  opening = setTimeout(() => {
    opening = null
    updateLayout((s) => P.showPeek(s, entityId, anchor, origin))
  }, OPEN_AFTER)
}

/** Open a pinned peek on an entity, with the keys. */
export const openPeek = (entityId: string, rect: Rect): void =>
  updateLayout((s) => P.openPeek(s, entityId, rect))

/** The pointer went into a peek's window. */
export function enterPeek(key: string): void {
  // A close already scheduled still runs: it keeps this peek and those under it,
  // and closes whatever was stacked above.
  hovering = key
}

/**
 * The pointer left something peekable (`window` null) or a peek's window. After
 * a moment, the transient peeks it is no longer in or under close.
 */
export function leavePeek(window: string | null): void {
  if (opening) clearTimeout(opening)
  opening = null
  // Leaving a pill leaves the pointer in the same window; leaving a window leaves
  // it on the main view until it enters another.
  if (window !== null && hovering === window) hovering = null
  holdPeeks()
  closing = setTimeout(() => {
    closing = null
    updateLayout((s) => P.keepHovered(s, hovering))
  }, CLOSE_AFTER)
}

/** Close one peek (and what is stacked on it), or the top transient one. */
export function closePeek(key: string | null): void {
  holdPeeks()
  updateLayout((s) => P.closePeek(s, key))
}

/** A peek was moved or resized, which pins it. */
export const placePeek = (key: string, rect: Rect): void =>
  updateLayout((s) => P.placePeek(s, key, rect))

export const raisePeek = (key: string): void => updateLayout((s) => P.raisePeek(s, key))

/**
 * Give the keys to a peek, or back to the main view. Taking them out of a
 * transient peek the pointer isn't in closes it: nothing is keeping it any more.
 */
export function focusPeek(key: string | null): void {
  updateLayout((s) => {
    const focused = P.focusPeek(s, key)
    return key === null && focused !== s ? P.keepHovered(focused, hovering) : focused
  })
}
