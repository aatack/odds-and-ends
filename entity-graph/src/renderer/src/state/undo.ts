import type { AppEvent } from '../../../core/events'
import { persistentAtom } from './atom'

// The undo stack. Undo doesn't reconstruct anything: it takes the most recent
// events off the store and keeps them here, so redo is simply writing them back.
// That makes the stack the only place those events exist — hence persisted, so
// closing the app doesn't discard them.
//
// It is not a cache: nothing else can reproduce its contents, which is why this
// is latent state despite looking like history.

export interface UndoStep {
  /**
   * The source the events came off. Replaying one store's events into another
   * would invent entities there, so a step is only redoable against its own
   * source.
   */
  sourceId: string
  /** When the step was undone (not when the events were originally written). */
  at: number
  /** The events removed, oldest first — the order to write them back in. */
  events: AppEvent[]
}

export const undoAtom = persistentAtom<UndoStep[]>('entity-graph.undo', [], (v) => Array.isArray(v))

/** Push a freshly undone step. Most recent last. */
export const pushUndo = (step: UndoStep): void => undoAtom.set((stack) => [...stack, step])

/** The step redo would replay, if it belongs to the open source. */
export function redoable(sourceId: string | null): UndoStep | null {
  const top = undoAtom.get()[undoAtom.get().length - 1]
  return top && top.sourceId === sourceId ? top : null
}

/** Remove the top step, once it has been written back. */
export const popUndo = (): void => undoAtom.set((stack) => stack.slice(0, -1))

/**
 * Discard the stack. Called for any write that didn't come from the stack
 * itself: those events are no longer the most recent ones in the store, so
 * writing them back would restore them out of order, after the newer edits.
 */
export const clearUndo = (): void => undoAtom.set((stack) => (stack.length ? [] : stack))
