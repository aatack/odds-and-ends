// What the content pane shows: a stack of views, kept in local storage so it
// survives a reload. Opening a view puts it on top; Back takes it off.

import { atom } from 'jotai'
import { atomWithStorage, createJSONStorage } from 'jotai/utils'

/**
 * One view of one event. `event` shows it as its type is shown (an item as its
 * chat, anything else as its details); `debug` shows its whole state as JSON.
 * More kinds of view go here.
 */
export type View = { kind: 'event'; id: string } | { kind: 'debug'; id: string }

// Storage can throw (a private window, cleared site data): then the stack just isn't kept.
const storage = createJSONStorage<View[]>(() => {
  try {
    return localStorage
  } catch {
    return sessionStorage
  }
})

export const viewsAtom = atomWithStorage<View[]>('workflows.views', [], storage, { getOnInit: true })

/** The right-click menu on an event, while it is open: where, and for which event. */
export const menuAtom = atom<{ x: number; y: number; id: string } | null>(null)
