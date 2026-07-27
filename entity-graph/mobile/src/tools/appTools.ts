import { canCall, currentSourceId, refreshCapabilities } from '../source/connection'
import { popEvents, writeEvents } from '../source/entity'
import { themeAtom } from '../state/store'
import { clearUndo, popUndo, pushUndo, redoable, undoAtom } from '../state/undo'
import { openSheet } from '../state/ui'
import type { ToolSpec } from './types'

// Tools about the app rather than the graph: undo, the settings sheet, the theme.

/** Undo and redo both need the source that the events belong to. */
const sourceId = (): string => {
  const id = currentSourceId()
  if (!id) throw new Error('Not connected to a source')
  return id
}

export const APP_TOOLS: ToolSpec[] = [
  {
    id: 'app.undo',
    label: 'Undo',
    aliases: ['back', 'revert', 'oops'],
    hint: 'App',
    mutates: true,
    // Works *on* the stack, so it must not clear it.
    preservesUndo: true,
    // Absent from a source that can't take events off again, which is how the
    // client knows undo is unavailable at all.
    enabled: () => canCall('popEvents'),
    run: async () => {
      const events = await popEvents()
      if (!events.length) {
        // The store never gives up an event older than five minutes, so an outline
        // you haven't touched in a while has nothing to undo. Say so, rather than
        // looking broken.
        return { mutated: false, message: 'Nothing recent enough to undo' }
      }
      pushUndo({ sourceId: sourceId(), at: Date.now(), events })
      return { message: `Undid ${events.length} change${events.length === 1 ? '' : 's'}` }
    },
  },
  {
    id: 'app.redo',
    label: 'Redo',
    aliases: ['again', 'forward', 'reapply'],
    hint: 'App',
    mutates: true,
    preservesUndo: true,
    enabled: () => redoable(currentSourceId()) != null,
    run: async () => {
      const step = redoable(currentSourceId())
      if (!step) return { mutated: false, message: 'Nothing to redo' }
      // Written back verbatim — the original timestamps and authors, not the edit
      // re-applied at the current time.
      await writeEvents(step.events)
      popUndo()
      return { message: `Redid ${step.events.length} change${step.events.length === 1 ? '' : 's'}` }
    },
  },
  {
    id: 'app.actions',
    label: 'Actions',
    hint: 'App',
    listed: false,
    run: () => openSheet({ kind: 'actions' }),
  },
  {
    id: 'app.settings',
    label: 'Settings',
    aliases: ['connection', 'source', 'server', 'token', 'theme', 'sign out'],
    hint: 'App',
    run: () => openSheet({ kind: 'settings' }),
  },
  {
    id: 'app.theme',
    label: 'Change the theme',
    aliases: ['dark', 'light', 'appearance', 'night'],
    hint: 'App',
    run: () => {
      const next =
        themeAtom.get() === 'system' ? 'light' : themeAtom.get() === 'light' ? 'dark' : 'system'
      themeAtom.set(next)
      return { message: `Theme: ${next}` }
    },
  },
  {
    id: 'app.reload',
    label: 'Check what this source can do',
    aliases: ['capabilities', 'tools', 'reconnect'],
    hint: 'App',
    run: async () => {
      await refreshCapabilities()
      return { message: 'Re-read the source’s tools' }
    },
  },
  {
    id: 'app.forgetUndo',
    label: 'Discard the undo history',
    aliases: ['clear undo'],
    hint: 'App',
    // Deliberately hidden from the list: this stack is the only copy of the events
    // in it, so running it destroys data rather than tidying a list. It exists for
    // the one case that needs it — a stack belonging to a source you have left.
    listed: false,
    preservesUndo: true,
    enabled: () => undoAtom.get().length > 0,
    run: () => {
      const count = undoAtom.get().length
      clearUndo()
      return { message: `Discarded ${count} undo step${count === 1 ? '' : 's'}` }
    },
  },
]
