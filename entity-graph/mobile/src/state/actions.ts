import type { LinkDirection } from '../core/types'
import * as R from './reducers'
import { updateView } from './store'
import type { EditState } from './types'

// Named mutators over the latent state. Two callers, exactly as on the desktop:
//
//  - tools, which are the invocable, listable surface;
//  - views, for direct manipulation — tapping a row, tapping a chevron, typing in
//    the in-place editor — where routing a touch through the tool machinery would
//    add noise for no gain.
//
// Either way the reducers stay the only thing that knows the shape of the state.

export const pushLevel = (rootId: string): void => updateView((s) => R.pushLevel(s, rootId))
export const popLevel = (): void => updateView(R.popLevel)
export const popToLevel = (index: number): void => updateView((s) => R.popToLevel(s, index))
export const resetTo = (rootId: string): void => updateView((s) => R.resetTo(s, rootId))

export const setDirection = (direction: LinkDirection): void =>
  updateView((s) => R.setDirection(s, direction))

export const selectPath = (path: string[]): void => updateView((s) => R.setSelectedPath(s, path))

export const setCollapsed = (entityId: string, collapsed: boolean): void =>
  updateView((s) => R.setCollapsed(s, entityId, collapsed))

export const toggleCollapse = (entityId: string): void =>
  updateView((s) => R.toggleCollapsed(s, entityId))

/** Replace the whole folded set — what "collapse everything" writes. */
export const setCollapsedSet = (collapsed: string[]): void =>
  updateView((s) => R.setCollapsedSet(s, collapsed))

export const setFind = (find: string | null): void => updateView((s) => R.setFind(s, find))

export const setSectionsOnly = (on: boolean): void => updateView((s) => R.setSectionsOnly(s, on))

// --- Editing ----------------------------------------------------------------

export const setEdit = (edit: EditState | null): void => updateView((s) => R.setEdit(s, edit))

export const setDraft = (draft: string): void => updateView((s) => R.setDraft(s, draft))

export const startEdit = (path: string[], text: string): void =>
  setEdit({ mode: 'edit', path, draft: text })

export const setCreateAfter = (after: string | null): void =>
  updateView((s) => R.setCreateAfter(s, after))

/**
 * Begin creating a child of `path`, to be linked after `after`.
 *
 * A folded parent stays folded: the input row is spliced in after whatever of its
 * subtree is on screen, which for a folded one is nothing, so the box lands
 * directly beneath it either way. Unfolding would drag a subtree into view purely
 * as a side effect of adding to it.
 */
export function startCreate(
  path: string[],
  values: Record<string, unknown> = {},
  after: string | null = null,
): void {
  if (!path.length) return
  setEdit({ mode: 'create', path, draft: '', values, after })
}
