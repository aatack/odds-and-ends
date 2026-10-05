import type { Entity, Focus } from '../../core/types.ts'

/**
 * Latent UI state: the minimum. Everything else is derived below and never
 * written back.
 */
export interface State {
  /** Entities focused, oldest first; `at` is the one on screen. */
  trail: string[]
  at: number
  /** Selected child id, per focused entity. Ids survive reordering. */
  cursors: Record<string, string>
  /** Composer text, per focused entity. */
  drafts: Record<string, string>
  /** Whether the composer has the keyboard. Not persisted. */
  composing: boolean
}

const trailLimit = 200

export function initialState(root: string): State {
  return { trail: [root], at: 0, cursors: {}, drafts: {}, composing: false }
}

export function focused(state: State): string {
  return state.trail[state.at]
}

/** Chat reads bottom up: its cursor starts at the newest message. */
export function startsAtEnd(entity: Entity | null): boolean {
  return entity?.type === 'slack.conversation' || entity?.type === 'slack.message'
}

export function cursorIndex(state: State, focus: Focus | null): number {
  if (!focus || focus.children.length === 0) return -1
  const selected = state.cursors[focused(state)]
  const index = selected ? focus.children.findIndex((child) => child.id === selected) : -1
  if (index >= 0) return index
  return startsAtEnd(focus.entity) ? focus.children.length - 1 : 0
}

export function selected(state: State, focus: Focus | null): Entity | null {
  const index = cursorIndex(state, focus)
  return index < 0 ? null : focus!.children[index]
}

export function navigate(state: State, id: string): State {
  if (focused(state) === id) return state
  const trail = [...state.trail.slice(0, state.at + 1), id].slice(-trailLimit)
  return { ...state, trail, at: trail.length - 1, composing: false }
}

export function back(state: State): State {
  return state.at > 0 ? { ...state, at: state.at - 1, composing: false } : state
}

export function forward(state: State): State {
  return state.at < state.trail.length - 1 ? { ...state, at: state.at + 1, composing: false } : state
}

export function select(state: State, id: string): State {
  return { ...state, cursors: { ...state.cursors, [focused(state)]: id } }
}

export function move(state: State, focus: Focus | null, delta: number): State {
  if (!focus || focus.children.length === 0) return state
  const index = Math.max(0, Math.min(focus.children.length - 1, cursorIndex(state, focus) + delta))
  return select(state, focus.children[index].id)
}

export function setDraft(state: State, text: string): State {
  const drafts = { ...state.drafts }
  if (text) drafts[focused(state)] = text
  else delete drafts[focused(state)]
  return { ...state, drafts }
}

export function setComposing(state: State, composing: boolean): State {
  return state.composing === composing ? state : { ...state, composing }
}

/** What is kept across reloads. */
export function persisted(state: State): Omit<State, 'composing'> {
  const { composing: _, ...rest } = state
  return rest
}

export function restore(saved: unknown, root: string): State {
  const value = saved as Partial<State> | undefined
  if (!value || !Array.isArray(value.trail) || value.trail.length === 0) return initialState(root)
  return {
    trail: value.trail,
    at: Math.min(Math.max(value.at ?? 0, 0), value.trail.length - 1),
    cursors: value.cursors ?? {},
    drafts: value.drafts ?? {},
    composing: false,
  }
}
