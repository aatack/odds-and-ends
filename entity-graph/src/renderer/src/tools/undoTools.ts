import { POP_AGE_LIMIT_MS } from '../../../core/source/permissions'
import { popUndo, pushUndo, redoable, undoAtom } from '../state/undo'
import { popEvents, writeEvents } from '../source/entity'
import { currentSourceId } from '../source/transport'
import type { ToolSpec } from './types'

// Undo and redo. Both are ordinary tools — the only unusual thing about them is
// `preservesUndo`, which stops the call machine clearing the stack they work on.
//
// Undo is destructive at the store: the events come off the database and the
// stack becomes the only copy. Redo writes them back with their original
// timestamps, so the store ends up exactly as it was rather than with the edit
// re-applied at the current time.
//
// Because it deletes, undo only reaches back so far: the store refuses to give
// up anything older than POP_AGE_LIMIT_MS, so an edit left alone for that long
// is permanent. Nothing here needs to check the age — asking simply comes back
// empty — but the message says so, since "nothing to undo" would otherwise read
// as a lie with the edit still on screen.

export const UNDO_TOOLS: ToolSpec[] = [
  {
    id: 'undo',
    label: 'Undo',
    aliases: ['revert', 'back', 'take back'],
    hint: 'Edit',
    scope: 'app',
    reach: 'source',
    mutates: true,
    preservesUndo: true,
    keys: [{ key: 'z', mod: true }],
    run: async () => {
      const sourceId = currentSourceId()
      if (!sourceId) throw new Error('No source is open')
      const events = await popEvents()
      if (events.length === 0) {
        const minutes = Math.round(POP_AGE_LIMIT_MS / 60_000)
        return { message: `Nothing to undo — edits settle after ${minutes} minutes`, mutated: false }
      }
      pushUndo({ sourceId, at: Date.now(), events })
      return {
        data: events,
        message: `Undid ${events.length} event${events.length === 1 ? '' : 's'}`,
      }
    },
  },
  {
    id: 'redo',
    label: 'Redo',
    aliases: ['again', 'forward', 'reapply'],
    hint: 'Edit',
    scope: 'app',
    reach: 'source',
    mutates: true,
    preservesUndo: true,
    keys: [{ key: 'y', mod: true }],
    run: async () => {
      const sourceId = currentSourceId()
      const step = redoable(sourceId)
      if (!step) {
        // Either the stack is empty or its top belongs to another source; say
        // which, since the second is confusing to hit.
        const stranded = undoAtom.get().length > 0
        return {
          message: stranded ? 'Nothing to redo against this source' : 'Nothing to redo',
          mutated: false,
        }
      }
      await writeEvents(step.events)
      popUndo()
      return {
        message: `Redid ${step.events.length} event${step.events.length === 1 ? '' : 's'}`,
      }
    },
  },
]

// Note the absence of a "clear undo history" tool. The stack holds the only copy
// of those events, so clearing it destroys data rather than tidying a list.
