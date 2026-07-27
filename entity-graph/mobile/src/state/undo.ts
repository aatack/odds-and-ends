import type { AppEvent } from '../core/types'
import { persistentAtom } from './atom'

// The undo stack, exactly as the desktop app keeps it: undo doesn't reconstruct
// anything, it takes the most recent events off the store and holds them here, so
// redo is simply writing them back.
//
// That makes this stack the only place those events exist — which is why it is
// persisted despite looking like history, and why there is no "clear undo" tool:
// it would destroy data, not tidy a list.

export interface UndoStep {
  /** The source the events came off. Replaying one store's events into another
   * would invent entities there, so a step is only redoable against its own. */
  sourceId: string
  at: number
  /** The events removed, oldest first — the order to write them back in. */
  events: AppEvent[]
}

export const undoAtom = persistentAtom<UndoStep[]>('entity-graph-mobile.undo', [], (v) =>
  Array.isArray(v),
)

export const pushUndo = (step: UndoStep): void => undoAtom.set((stack) => [...stack, step])

/** The step redo would replay, if it belongs to the open source. */
export function redoable(sourceId: string | null): UndoStep | null {
  const stack = undoAtom.get()
  const top = stack[stack.length - 1]
  return top && top.sourceId === sourceId ? top : null
}

export const popUndo = (): void => undoAtom.set((stack) => stack.slice(0, -1))

/**
 * Discard the stack. Called for any write that didn't come from the stack itself:
 * those events are no longer the store's most recent, so writing them back would
 * restore them after the newer edits rather than before.
 */
export const clearUndo = (): void => undoAtom.set((stack) => (stack.length ? [] : stack))
