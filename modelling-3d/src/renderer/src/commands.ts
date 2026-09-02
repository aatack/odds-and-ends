/**
 * Everything the user can invoke, in one list.
 *
 * A hotkey, a header button and a menu item all dispatch the same command, so
 * they cannot drift apart, and there is exactly one key listener — in `App` —
 * which walks this list and runs the first enabled command whose binding
 * matches. Nothing else in the app listens for a key.
 */

import { exportGlb } from '@core/glb'
import { meshOf } from '@core/scene'
import type { ModellingAPI } from '@core/api'
import type { Actions } from './state/actions'
import { evaluationOf, openModel, previewScene } from './state/derive'
import type { AppState } from './state/store'

/** What the views lend the registry: the things only a rendered view can do. */
export interface ViewHandles {
  frameCamera(): void
  focusSearch(): void
}

export interface CommandContext {
  state: AppState
  actions: Actions
  api: ModellingAPI | null
  views: ViewHandles
}

export interface Command {
  id: string
  label: string
  /** Bindings in the form `mod+e`, `shift+f`, `Delete`. First match wins. */
  keys?: string[]
  /** A binding without a modifier is ignored while text is being typed. */
  enabled?(context: CommandContext): boolean
  run(context: CommandContext): void | Promise<void>
}

const hasModel = ({ state }: CommandContext): boolean => openModel(state) !== null

export const COMMANDS: Command[] = [
  {
    id: 'model.new',
    label: 'New model',
    keys: ['mod+n'],
    run: ({ actions }) => void actions.createModel(),
  },
  {
    id: 'selection.all',
    label: 'Select every node',
    keys: ['mod+a'],
    enabled: hasModel,
    run: ({ state, actions }) => actions.setSelection(Object.keys(openModel(state)!.nodes)),
  },
  {
    id: 'selection.clear',
    label: 'Select nothing',
    keys: ['Escape'],
    enabled: ({ state }) => state.selection.length > 0,
    run: ({ actions }) => actions.setSelection([]),
  },
  {
    id: 'nodes.delete',
    label: 'Delete the selected nodes',
    keys: ['Delete', 'Backspace'],
    enabled: ({ state }) => state.selection.length > 0,
    run: ({ state, actions }) => actions.deleteNodes(state.selection),
  },
  {
    id: 'navigator.search',
    label: 'Find a transform',
    keys: ['mod+f'],
    run: ({ views }) => views.focusSearch(),
  },
  {
    id: 'preview.frame',
    label: 'Frame what is shown',
    keys: ['f'],
    run: ({ views }) => views.frameCamera(),
  },
  {
    id: 'model.export',
    label: 'Export as glTF',
    keys: ['mod+e'],
    enabled: hasModel,
    async run({ state, actions, api }) {
      const model = openModel(state)
      if (!model) return
      const scene = previewScene(state, evaluationOf(model, state.models))
      const mesh = meshOf(scene)
      if (mesh.triangles.length === 0) {
        actions.notify({ text: 'There are no triangles to export.' })
        return
      }
      const bytes = exportGlb(mesh, { generator: 'modelling-3d' })
      if (!api) {
        actions.notify({ text: 'Exporting needs the desktop app.' })
        return
      }
      const path = await api.saveModel(`${model.name}.glb`, bytes)
      actions.notify({
        text: `${mesh.triangles.length} triangles written to ${path}`,
        path,
      })
    },
  },
]

export const commandById = (id: string): Command =>
  COMMANDS.find((command) => command.id === id) ?? COMMANDS[0]

const isTyping = (target: EventTarget | null): boolean => {
  const element = target as HTMLElement | null
  if (!element) return false
  const tag = element.tagName
  return tag === 'INPUT' || tag === 'TEXTAREA' || element.isContentEditable
}

function matches(event: KeyboardEvent, binding: string): boolean {
  const parts = binding.split('+')
  const key = parts[parts.length - 1]
  const wants = new Set(parts.slice(0, -1))
  const mod = event.metaKey || event.ctrlKey
  if (wants.has('mod') !== mod) return false
  if (wants.has('shift') !== event.shiftKey) return false
  if (wants.has('alt') !== event.altKey) return false
  return event.key.toLowerCase() === key.toLowerCase()
}

/** The one key router. Returns true when a command took the event. */
export function dispatchKey(event: KeyboardEvent, context: CommandContext): boolean {
  const typing = isTyping(event.target)
  for (const command of COMMANDS) {
    for (const binding of command.keys ?? []) {
      if (!matches(event, binding)) continue
      // A bare key belongs to whatever is being typed into.
      if (typing && !binding.includes('+')) continue
      if (command.enabled && !command.enabled(context)) continue
      void command.run(context)
      return true
    }
  }
  return false
}
