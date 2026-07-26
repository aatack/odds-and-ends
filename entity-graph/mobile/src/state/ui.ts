import { atom } from './atom'

// Which sheet is open, and nothing else. Runtime only: a sheet is a gesture in
// progress, and reopening one on a reload would be a jump-scare rather than a
// convenience.
//
// One at a time by construction — a phone has room for one — so this is a single
// value rather than a set of flags.

export type Sheet =
  /** The tool list, scoped to the selection: the phone's command palette. */
  | { kind: 'actions' }
  /** The navigation stack, as a list to jump back through. */
  | { kind: 'crumbs' }
  /** Connection, author, theme. */
  | { kind: 'settings' }
  /** Prompting for a tool's remaining arguments. */
  | { kind: 'args'; toolId: string; args: Record<string, unknown> }
  /** Picking an entity by tapping a row — the far end of a move or a link. */
  | { kind: 'pick'; toolId: string; args: Record<string, unknown>; argName: string; prompt: string }

export const sheetAtom = atom<Sheet | null>(null)

export const openSheet = (sheet: Sheet): void => sheetAtom.set(sheet)
export const closeSheet = (): void => sheetAtom.set(null)

/** The pick in progress, if the app is waiting for a row to be tapped. */
export const pickingSheet = (): Extract<Sheet, { kind: 'pick' }> | null => {
  const sheet = sheetAtom.get()
  return sheet?.kind === 'pick' ? sheet : null
}
