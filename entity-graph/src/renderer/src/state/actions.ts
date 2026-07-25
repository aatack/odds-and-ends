import * as R from './reducers'
import { updateLayout } from './store'
import type { EditState, FrameState, LayoutState } from './types'

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

export const setMaxDepth = (frameId: string, entityId: string, depth: number | null): void =>
  updateLayout((s) => R.setMaxDepth(s, frameId, entityId, depth))

// --- Editing ----------------------------------------------------------------

export const setEdit = (frameId: string, edit: EditState | null): void =>
  updateLayout((s) => R.setEdit(s, frameId, edit))

/** Update the persisted draft of whatever edit is in progress. */
export const setDraft = (frameId: string, draft: string): void =>
  updateLayout((s) =>
    R.updateFrame(s, frameId, (f) => (f.edit ? { ...f, edit: { ...f.edit, draft } } : f)),
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

export const pushFrame = (tabId: string, rootId: string): void =>
  updateLayout((s) => R.pushFrame(s, tabId, rootId))

/** The frame a tool is acting on, resolved from the current focus. */
export const frameOf = (s: LayoutState, frameId: string | null): FrameState | null =>
  frameId ? (s.frames[frameId] ?? null) : null
