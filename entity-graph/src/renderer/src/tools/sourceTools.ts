import { sourceSelectionAtom } from '../state/sources'
import { uiAtom } from '../state/ui'
import type { ToolSpec } from './types'

// The sources page's one keystroke. Everything else there is a click — a field
// typed into, a switch, a line dragged between two nodes — but deleting has to
// be a key, and there is one key listener in this app, so it is a tool.
//
// It sits at the front of the registry for the same reason the diagram's
// selection tools do: Backspace already means "take this row out of its parent",
// and the router hands a press to the first tool that binds the key and says it
// applies.

const api = window.entityGraph

const selection = (): { nodes: string[]; edges: string[] } | null => {
  if (uiAtom.get().page !== 'sources') return null
  const selected = sourceSelectionAtom.get()
  return selected.nodes.length || selected.edges.length ? selected : null
}

export const SOURCE_TOOLS: ToolSpec[] = [
  {
    id: 'sources.delete',
    label: 'Delete what is selected on the sources page',
    aliases: ['remove node', 'unplug', 'delete edge', 'sources'],
    hint: 'Sources',
    scope: 'app',
    reach: 'ui',
    keys: [{ key: 'Backspace' }, { key: 'Delete' }],
    enabled: () => selection() != null,
    run: async () => {
      const selected = selection()
      if (!selected) return
      // Edges first: deleting a node takes its own edges with it, and asking for
      // an edge that has already gone would be the one error worth avoiding.
      for (const id of selected.edges) await api.disconnectNodes(id)
      for (const id of selected.nodes) await api.removeNode(id)
      const count = selected.nodes.length + selected.edges.length
      return { message: `Removed ${count} thing${count === 1 ? '' : 's'}` }
    },
  },
]
