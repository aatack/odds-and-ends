import { atom } from './atom'

// What the sources page has selected. Held here rather than in the canvas
// because deleting is a *key*, and there is one key listener in this app: the
// tool that Backspace runs has to be able to ask what is selected without
// reaching into a component.

export interface SourceSelection {
  nodes: string[]
  edges: string[]
}

const EMPTY: SourceSelection = { nodes: [], edges: [] }

/** Runtime only: it means nothing once the page is closed. */
export const sourceSelectionAtom = atom<SourceSelection>(EMPTY)

export const reportSourceSelection = (selection: SourceSelection): void =>
  sourceSelectionAtom.set(selection)

export const clearSourceSelection = (): void => sourceSelectionAtom.set(EMPTY)
