// What the content pane shows: a stack of views, kept in local storage so it
// survives a reload. Opening a view puts it on top; Back takes it off.

import { atom } from 'jotai'
import { atomWithStorage, createJSONStorage } from 'jotai/utils'

/**
 * A view. `items` is every item, to filter, open or add to: what shows when the
 * stack is empty. `event` shows one event as its type is shown (an item as its
 * chat, anything else as its details); `debug` shows its whole state as JSON.
 * `integrations` and `mcp` are the app's settings pages. More kinds of view go here.
 */
export type View =
  | { kind: 'items' }
  | { kind: 'event'; id: string }
  | { kind: 'debug'; id: string }
  | { kind: 'integrations' }
  | { kind: 'mcp' }

// Storage can throw (a private window, cleared site data): then the stack just isn't kept.
const storage = createJSONStorage<View[]>(() => {
  try {
    return localStorage
  } catch {
    return sessionStorage
  }
})

export const viewsAtom = atomWithStorage<View[]>('workflows.views', [], storage, { getOnInit: true })

/**
 * The views Back took off, the most recent last: Forward puts them back on, as a
 * browser's history does. Going anywhere new clears it.
 */
export const forwardAtom = atomWithStorage<View[]>('workflows.forward', [], storage, { getOnInit: true })

/** Two views are the same view: the same kind, of the same event. */
export const sameView = (a: View | undefined, b: View): boolean =>
  !!a && a.kind === b.kind && ('id' in a ? a.id : null) === ('id' in b ? b.id : null)

/** Bumped to put the cursor in the items view's search line (Ctrl+L, when that view is already showing). */
export const searchFocusAtom = atom(0)

/** The right-click menu on an event, while it is open: where, and for which event. */
export const menuAtom = atom<{ x: number; y: number; id: string } | null>(null)
