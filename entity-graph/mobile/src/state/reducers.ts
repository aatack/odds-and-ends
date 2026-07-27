import type { LinkDirection } from '../core/types'
import { newLevel, type EditState, type Level, type ViewState } from './types'

// Pure transitions over the latent state. The only things that know its shape:
// actions and tools are written in terms of these, and nothing here reads a
// query, a cache or the DOM.

/** Replace the level on screen. */
function updateTop(s: ViewState, fn: (level: Level) => Level): ViewState {
  const at = s.stack.length - 1
  const next = fn(s.stack[at])
  if (next === s.stack[at]) return s
  const stack = s.stack.slice()
  stack[at] = next
  return { ...s, stack }
}

// --- Navigation -------------------------------------------------------------

/**
 * Drill into an entity: a new level on top, reading outwards from it.
 *
 * The direction isn't inherited. Following inbound links is a question you asked
 * about one entity ("what refers to this?"), and carrying it into the answer
 * turns the next screen inside out for no reason the user gave.
 *
 * An edit in progress is dropped rather than carried: it belongs to the level it
 * was started on, and its commit-on-blur has already run by the time a navigation
 * gesture lands.
 */
export const pushLevel = (s: ViewState, rootId: string): ViewState =>
  rootId && rootId !== s.stack[s.stack.length - 1].rootId
    ? { ...s, stack: [...s.stack, newLevel(rootId)], edit: null, find: null }
    : s

/** Back one level. A no-op at the outermost one, which has nowhere to go. */
export const popLevel = (s: ViewState): ViewState =>
  s.stack.length > 1
    ? { ...s, stack: s.stack.slice(0, -1), edit: null, find: null }
    : s

/** Back to a level by index — how a crumb in the header is followed. */
export const popToLevel = (s: ViewState, index: number): ViewState =>
  index >= 0 && index < s.stack.length - 1
    ? { ...s, stack: s.stack.slice(0, index + 1), edit: null, find: null }
    : s

/** Replace the whole stack with one level. Used by "go to the root". */
export const resetTo = (s: ViewState, rootId: string): ViewState => ({
  ...s,
  stack: [newLevel(rootId)],
  edit: null,
  find: null,
})

export const setDirection = (s: ViewState, direction: LinkDirection): ViewState =>
  updateTop(s, (level) =>
    level.direction === direction
      ? level
      : // The selection falls back to the root: the rows about to arrive are a
        // different tree, and a path through the old one won't be in it.
        { ...level, direction, selectedPath: [level.rootId] },
  )

// --- Selection and folding --------------------------------------------------

export const setSelectedPath = (s: ViewState, path: string[]): ViewState =>
  updateTop(s, (level) => ({ ...level, selectedPath: path }))

export function setCollapsed(s: ViewState, entityId: string, collapsed: boolean): ViewState {
  const has = s.collapsed.includes(entityId)
  if (has === collapsed) return s
  return {
    ...s,
    collapsed: collapsed ? [...s.collapsed, entityId] : s.collapsed.filter((id) => id !== entityId),
  }
}

export const toggleCollapsed = (s: ViewState, entityId: string): ViewState =>
  setCollapsed(s, entityId, !s.collapsed.includes(entityId))

/** Replace the folded set wholesale — "collapse everything" and its opposite. */
export const setCollapsedSet = (s: ViewState, collapsed: string[]): ViewState =>
  collapsed.length === s.collapsed.length && collapsed.every((id) => s.collapsed.includes(id))
    ? s
    : { ...s, collapsed }

// --- Filters ----------------------------------------------------------------

export const setFind = (s: ViewState, find: string | null): ViewState =>
  s.find === find ? s : { ...s, find }

export const setSectionsOnly = (s: ViewState, sectionsOnly: boolean): ViewState =>
  s.sectionsOnly === sectionsOnly ? s : { ...s, sectionsOnly }

// --- Editing ----------------------------------------------------------------

export const setEdit = (s: ViewState, edit: EditState | null): ViewState =>
  s.edit === edit ? s : { ...s, edit }

export const setDraft = (s: ViewState, draft: string): ViewState =>
  s.edit ? { ...s, edit: { ...s.edit, draft } } : s

/**
 * Tell a create in progress which sibling it now follows.
 *
 * This exists for one flow: typing a list. Committing a line and starting the next
 * one has to happen in the same beat, or the box unmounts and the keyboard shuts
 * between every item — so the next box opens before the write that decides what it
 * comes after has finished, and is told once the id lands.
 */
export const setCreateAfter = (s: ViewState, after: string | null): ViewState =>
  s.edit?.mode === 'create' ? { ...s, edit: { ...s.edit, after } } : s
