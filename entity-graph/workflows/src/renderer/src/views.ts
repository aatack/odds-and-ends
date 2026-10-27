// What the content pane shows: a stack of views, kept in local storage so it
// survives a reload. Opening a view puts it on top; Back takes it off.

import { atom } from 'jotai'
import { atomWithStorage, createJSONStorage } from 'jotai/utils'

/**
 * A view. `items` is every item, to filter, open or add to: what shows when the
 * stack is empty. `event` shows one event as its type is shown (an item as its
 * chat, anything else as its details); `debug` shows its whole state as JSON.
 * `integrations` and `mcp` are the app's settings pages; `types` the event types and their actions. `bots` lists the
 * bots (each opens as an `event`: its settings, runs and own chat). More kinds of view go here.
 */
export type View =
  | { kind: 'items' }
  | { kind: 'event'; id: string }
  | { kind: 'debug'; id: string }
  | { kind: 'integrations' }
  | { kind: 'mcp' }
  | { kind: 'phone' }
  | { kind: 'types' }
  | { kind: 'bots' }
  /** The edit log: of one event, given its id; else of everything. */
  | { kind: 'log'; id?: string }

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

/**
 * What was last opened straight from each task (by its id): Forward, with nothing ahead, goes there. Only a direct
 * step counts (a reference, a parent, a child); not one from the task list or Next.
 */
export const reachedAtom = atomWithStorage<Record<string, string>>('workflows.reached', {}, createJSONStorage(() => localStorage), { getOnInit: true })

/** How many views the stack, and what Back took off, keep: the oldest go first. */
export const HISTORY_LIMIT = 50

/** Two views are the same view: the same kind, of the same event. */
export const sameView = (a: View | undefined, b: View): boolean =>
  !!a && a.kind === b.kind && ('id' in a ? a.id : null) === ('id' in b ? b.id : null) 

/** Bumped to put the cursor in the items view's search line (Ctrl+L, when that view is already showing). */
export const searchFocusAtom = atom(0)

/** The right-click menu on an event, while it is open: where, and for which event. */
export const menuAtom = atom<{ x: number; y: number; id: string } | null>(null)

/** The task whose title is being renamed in its view's header, as the event menu's Rename asks. */
export const renameAtom = atom<string | null>(null)

/**
 * What is typed in each item's chat input and not yet sent, by item id: kept so
 * that going to another view mid-sentence loses nothing. Sending clears it.
 */
export const draftsAtom = atomWithStorage<Record<string, string>>('workflows.drafts', {}, createJSONStorage(() => localStorage), {
  getOnInit: true,
})

/** The drafts without the empty ones, or the same object when there are none to drop. */
export const withoutEmpty = (drafts: Record<string, string>): Record<string, string> =>
  Object.values(drafts).every((t) => t.trim())
    ? drafts
    : Object.fromEntries(Object.entries(drafts).filter(([, t]) => t.trim()))

/** Ctrl+F's phrase in the view on screen, while its find line is open; null while it is closed. */
export const findAtom = atom<string | null>(null)

/** Bumped to put the cursor in the find line (Ctrl+F, when it is already open). */
export const findFocusAtom = atom(0)

/** The tasks the person here has had on screen lately, the most recent first: the finder's "Recently viewed". Kept here, not in the store. */
export const viewedAtom = atomWithStorage<string[]>('workflows.viewed', [], createJSONStorage(() => localStorage), { getOnInit: true })
