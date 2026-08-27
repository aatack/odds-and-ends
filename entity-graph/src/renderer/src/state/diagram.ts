import { atom } from './atom'

// What the user is doing inside a diagram's canvas: which of its shapes are
// selected, and which one is being typed into.
//
// It is here rather than inside the canvas because the keyboard is not the
// canvas's to take. Backspace over a selected rectangle and Enter to type into
// one are tools like every other key, resolved through the one listener at the
// top of the app, and a tool has to be able to ask what is selected — see
// `tools/diagramTools`. Runtime only: a selection is not worth surviving a
// reload, and neither is a half-typed label.
//
// One canvas at a time holds it. Several diagrams can be on screen at once, and
// each speaks only for itself: a canvas with nothing going on lets go of the
// slot if it was the one holding it, and leaves it alone if it wasn't.

export interface DiagramFocus {
  /** The diagram entity whose canvas this is about. */
  entityId: string
  /** The keys of the shapes selected on it. */
  selected: string[]
  /** The shape being typed into, if any. */
  editing: string | null
}

export const diagramAtom = atom<DiagramFocus | null>(null)

/** Whether that slot is this diagram's, so a canvas can read its own state out. */
export const focusOn = (focus: DiagramFocus | null, entityId: string): DiagramFocus | null =>
  focus?.entityId === entityId ? focus : null

const empty = (focus: DiagramFocus): boolean => !focus.selected.length && focus.editing == null

/** Whether two of these say the same thing, so saying it twice costs a render. */
const same = (a: DiagramFocus | null, b: DiagramFocus | null): boolean =>
  a === b ||
  (!!a &&
    !!b &&
    a.entityId === b.entityId &&
    a.editing === b.editing &&
    a.selected.length === b.selected.length &&
    a.selected.every((key, i) => b.selected[i] === key))

/** Take the slot, or let go of it when there is nothing left in it to hold. */
function hold(entityId: string, next: (held: DiagramFocus) => DiagramFocus): void {
  diagramAtom.set((held) => {
    const mine = focusOn(held, entityId)
    const made = next(mine ?? { entityId, selected: [], editing: null })
    if (empty(made)) return mine ? null : held
    return same(made, mine) ? held : made
  })
}

/** What a canvas has selected, said by that canvas and by nothing else. */
export const reportDiagramSelection = (entityId: string, selected: string[]): void =>
  hold(entityId, (held) => ({ ...held, selected }))

/** Start typing into one of a diagram's shapes, or stop. */
export const editDiagramShape = (entityId: string, key: string | null): void =>
  hold(entityId, (held) => ({ ...held, editing: key }))

/**
 * Let go, whatever was in it — what a canvas does as it goes away. Rows are only
 * mounted near the viewport, so a diagram scrolled off the screen must not still
 * be holding the Backspace key.
 */
export const releaseDiagram = (entityId: string): void =>
  diagramAtom.set((held) => (focusOn(held, entityId) ? null : held))
