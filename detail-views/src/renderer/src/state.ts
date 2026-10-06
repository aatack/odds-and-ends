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
  /** An action started on the focus and waiting for Enter. Not persisted. */
  acting: string | null
  /** An image shown full size over everything, by its ref. Not persisted. */
  viewing: string | null
  /**
   * Floating windows peeking at a link or an entity, bottom first. Transient
   * ones follow the hover and stack: one opened from inside another is its
   * child. Pinned ones stay, and are persisted.
   */
  peeks: Peek[]
}

export interface Rect {
  x: number
  y: number
  width: number
  height: number
}

export type PeekTarget = { kind: 'url'; url: string } | { kind: 'entity'; id: string }

export interface Peek {
  key: string
  target: PeekTarget
  /** Where the hover was; a transient peek is placed beside it. */
  anchor: Rect
  /** Set once the window is moved or resized, which also pins it. */
  rect: Rect | null
  pinned: boolean
  /** The peek this one was opened from, or null for the main view. */
  parent: string | null
}

export function sameTarget(a: PeekTarget, b: PeekTarget): boolean {
  return a.kind === 'url' ? b.kind === 'url' && a.url === b.url : b.kind === 'entity' && a.id === b.id
}

export function transientPeek(state: State): Peek | null {
  return state.peeks.findLast((peek) => !peek.pinned) ?? null
}

/** A peek and the peeks it was opened from, by key. */
export function peekChain(state: State, key: string | null): Set<string> {
  const chain = new Set<string>()
  let current = key
  while (current && !chain.has(current)) {
    chain.add(current)
    current = state.peeks.find((peek) => peek.key === current)?.parent ?? null
  }
  return chain
}

/**
 * Opens a transient peek on top of the one it came from. Transient peeks off
 * that line (siblings and their children) close; pinned ones stay.
 */
export function showPeek(state: State, peek: Peek): State {
  const keep = peekChain(state, peek.parent)
  return { ...state, peeks: [...state.peeks.filter((other) => other.pinned || keep.has(other.key)), peek] }
}

/** Closes transient peeks the pointer is no longer in or under. */
export function keepHovered(state: State, hovering: string | null): State {
  const keep = peekChain(state, hovering)
  const peeks = state.peeks.filter((peek) => peek.pinned || keep.has(peek.key))
  return peeks.length === state.peeks.length ? state : { ...state, peeks }
}

/** Closes one peek, and any transient ones stacked on it. `null` closes the top transient one. */
export function closePeek(state: State, key: string | null): State {
  const target = key ?? transientPeek(state)?.key
  if (!target) return state
  const peeks = state.peeks.filter((peek) => peek.key !== target && !(!peek.pinned && peekChain(state, peek.key).has(target)))
  return peeks.length === state.peeks.length ? state : { ...state, peeks }
}

/** Moving or resizing pins a peek and brings it to the front. */
export function placePeek(state: State, key: string, rect: Rect): State {
  const peek = state.peeks.find((candidate) => candidate.key === key)
  if (!peek) return state
  return { ...state, peeks: [...state.peeks.filter((other) => other !== peek), { ...peek, rect, pinned: true }] }
}

export function raisePeek(state: State, key: string): State {
  const peek = state.peeks.find((candidate) => candidate.key === key)
  if (!peek || state.peeks.at(-1) === peek) return state
  return { ...state, peeks: [...state.peeks.filter((other) => other !== peek), peek] }
}

const trailLimit = 200

export function initialState(root: string): State {
  return { trail: [root], at: 0, cursors: {}, drafts: {}, composing: false, acting: null, viewing: null, peeks: [] }
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
  return { ...state, trail, at: trail.length - 1, composing: false, acting: null, viewing: null, peeks: state.peeks.filter((peek) => peek.pinned) }
}

export function back(state: State): State {
  return state.at > 0 ? { ...state, at: state.at - 1, composing: false, acting: null } : state
}

export function forward(state: State): State {
  return state.at < state.trail.length - 1 ? { ...state, at: state.at + 1, composing: false, acting: null } : state
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
  if (text) drafts[draftKey(state)] = text
  else delete drafts[draftKey(state)]
  return { ...state, drafts }
}

export function view(state: State, ref: string | null): State {
  return state.viewing === ref ? state : { ...state, viewing: ref }
}

export function setComposing(state: State, composing: boolean): State {
  if (state.composing === composing) return state
  // Leaving the prompt abandons the action.
  return { ...state, composing, acting: composing ? state.acting : null }
}

export function startAction(state: State, action: string): State {
  return { ...state, acting: action, composing: true }
}

/** Drafts for an action are kept apart from the focus's own composer. */
export function draftKey(state: State): string {
  return state.acting ? `${focused(state)}#${state.acting}` : focused(state)
}

/** What is kept across reloads. */
export function persisted(state: State): Omit<State, 'composing' | 'viewing' | 'acting'> {
  const { composing: _, viewing: __, acting: ___, ...rest } = state
  return { ...rest, peeks: rest.peeks.filter((peek) => peek.pinned) }
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
    acting: null,
    viewing: null,
    peeks: Array.isArray(value.peeks) ? value.peeks.filter((peek) => peek.pinned).map((peek) => ({ ...peek, parent: peek.parent ?? null })) : [],
  }
}
