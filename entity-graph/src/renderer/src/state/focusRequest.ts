import { atom } from './atom'

// Where the keyboard should go next, when a tool needs to put it somewhere.
//
// A tool can't focus a DOM node — the state layer has no DOM and the tools layer
// has no components — so it names a field instead and the component that owns
// that field answers, through `useFocusRequest`. Everything stays declarative in
// the direction that matters: state down, requests up.
//
// This is a signal rather than state: nothing renders differently for it, and
// re-firing at the same target is meaningful, which is why each request carries
// a nonce. Runtime only — a reload should not steal the caret.

export interface FocusRequest {
  /** The field asked for, or '' when nothing has been asked for yet. */
  target: string
  /** Bumped per request, so asking twice for the same field lands twice. */
  nonce: number
}

export const focusRequestAtom = atom<FocusRequest>({ target: '', nonce: 0 })

/** Name a frame's find field. */
export const findField = (frameId: string): string => `find:${frameId}`

export const requestFocus = (target: string): void =>
  focusRequestAtom.set((r) => ({ target, nonce: r.nonce + 1 }))

/**
 * Mark the standing request answered. A request is a one-shot: left standing, it
 * would be taken again by the next field of that name to mount — switching tabs
 * back and forth would keep stealing the caret.
 */
export const focusTaken = (): void => focusRequestAtom.set((r) => ({ ...r, target: '' }))
