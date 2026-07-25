import * as R from '../state/reducers'
import { focusOf, getLayout, updateLayout } from '../state/store'
import { toggleTheme, updateUi, uiAtom } from '../state/ui'
import type { ToolSpec } from './types'

// Note what isn't here: opening the palette and cancelling a pending call are
// the call machine's own keys, and live with the router in ./dispatch. Keeping
// them out means no tool file imports the call machine, so the registry stays
// free of import cycles.

// Tools above the frame: the tab group they live in, and the shell around it.
// Split by scope so a key can mean one thing to a frame and another to the app —
// Escape cancels an in-place edit if there is one, and otherwise the pending
// call.

const group = (): string | null => focusOf(getLayout()).groupId

export const GROUP_TOOLS: ToolSpec[] = [
  {
    id: 'tab.new',
    label: 'New tab',
    aliases: ['add tab'],
    hint: 'Layout',
    scope: 'group',
    reach: 'ui',
    run: () => {
      const groupId = group()
      if (groupId) updateLayout((s) => R.addTab(s, groupId))
    },
  },
  {
    id: 'tab.close',
    label: 'Close tab',
    aliases: ['remove tab'],
    hint: 'Layout',
    scope: 'group',
    reach: 'ui',
    run: () => {
      const { groupId, tabId } = focusOf(getLayout())
      if (groupId && tabId) updateLayout((s) => R.closeTab(s, groupId, tabId))
    },
  },
  {
    id: 'tab.next',
    label: 'Next tab',
    aliases: ['cycle tab'],
    scope: 'group',
    reach: 'ui',
    keys: [{ key: 'Tab', mod: true }],
    run: () => {
      const groupId = group()
      if (groupId) updateLayout((s) => R.cycleTab(s, groupId, 1))
    },
  },
  {
    id: 'tab.prev',
    label: 'Previous tab',
    aliases: ['cycle tab back'],
    scope: 'group',
    reach: 'ui',
    keys: [{ key: 'Tab', mod: true, shift: true }],
    run: () => {
      const groupId = group()
      if (groupId) updateLayout((s) => R.cycleTab(s, groupId, -1))
    },
  },
  {
    id: 'tab.moveLeft',
    label: 'Move tab left',
    scope: 'group',
    reach: 'ui',
    keys: [{ key: 'ArrowLeft', alt: true, mod: true }],
    run: () => {
      const { groupId, tabId } = focusOf(getLayout())
      if (groupId && tabId) updateLayout((s) => R.moveTab(s, groupId, tabId, -1))
    },
  },
  {
    id: 'tab.moveRight',
    label: 'Move tab right',
    scope: 'group',
    reach: 'ui',
    keys: [{ key: 'ArrowRight', alt: true, mod: true }],
    run: () => {
      const { groupId, tabId } = focusOf(getLayout())
      if (groupId && tabId) updateLayout((s) => R.moveTab(s, groupId, tabId, 1))
    },
  },
]

export const APP_TOOLS: ToolSpec[] = [
  {
    id: 'group.prev',
    label: 'Focus previous tab group',
    aliases: ['left group'],
    scope: 'app',
    reach: 'ui',
    keys: [{ key: 'ArrowLeft', alt: true }],
    run: () => updateLayout((s) => R.selectAdjacentGroup(s, -1)),
  },
  {
    id: 'group.next',
    label: 'Focus next tab group',
    aliases: ['right group'],
    scope: 'app',
    reach: 'ui',
    keys: [{ key: 'ArrowRight', alt: true }],
    run: () => updateLayout((s) => R.selectAdjacentGroup(s, 1)),
  },
  {
    id: 'layout.expand',
    label: 'Expand tab group',
    aliases: ['maximize', 'solo', 'fullscreen', 'zoom', 'focus'],
    scope: 'app',
    reach: 'ui',
    keys: [{ key: 'm' }],
    run: () => updateLayout(R.toggleExpanded),
  },
  {
    id: 'activity.toggle',
    label: 'Show activity',
    aliases: ['log', 'history', 'calls'],
    hint: 'Shell',
    scope: 'app',
    reach: 'ui',
    run: () => updateUi({ activityOpen: !uiAtom.get().activityOpen }),
  },
  {
    id: 'theme.toggle',
    label: 'Toggle theme',
    aliases: ['dark mode', 'light mode', 'appearance'],
    hint: 'Shell',
    scope: 'app',
    reach: 'ui',
    run: () => toggleTheme(),
  },
  {
    id: 'page.editor',
    label: 'Go to editor',
    aliases: ['tree', 'graph', 'entities'],
    hint: 'Navigate',
    scope: 'app',
    reach: 'ui',
    run: () => updateUi({ page: 'editor' }),
  },
  {
    id: 'page.sources',
    label: 'Go to sources',
    aliases: ['servers', 'connections', 'configuration'],
    hint: 'Navigate',
    scope: 'app',
    reach: 'ui',
    run: () => updateUi({ page: 'sources' }),
  },
  {
    id: 'source.debug',
    label: 'Debug source',
    aliases: ['events', 'raw', 'tools'],
    hint: 'Shell',
    scope: 'app',
    reach: 'ui',
    run: () => updateUi({ debugSource: true }),
  },
]
