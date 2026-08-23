import { TOOL_ID } from '../../../core/builtins'
import { refreshDerived } from '../../../core/cache'
import { createEntity } from '../source/entity'
import { openExternal } from '../source/files'
import * as R from '../state/reducers'
import { focusOf, getLayout, updateLayout } from '../state/store'
import { toggleTheme, updateUi, uiAtom } from '../state/ui'
import { TOOLS_ENTITY_ID, loadUserTools } from './userTools'
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
    keys: [{ key: 't', mod: true }],
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
    // Ctrl/⌘+W means "close this tab", not "close the window" — see the menu in
    // src/main/index.ts, which gives up the accelerator so this can have it.
    keys: [{ key: 'w', mod: true }],
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
    // Not `mutates`: nothing is written. It only throws away what the scripts
    // produced last time, which is why it doesn't strand the undo stack either.
    id: 'derived.refresh',
    label: 'Recompute derived events',
    aliases: ['events', 'rerun', 'scripts', 'reload derived'],
    hint: 'Shell',
    scope: 'app',
    reach: 'ui',
    run: () => {
      refreshDerived()
      return { message: 'Running every events script again' }
    },
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
    // The one tool that leaves the app. Everything a note holds that points
    // somewhere else — a pull request, a Slack thread, a dashboard — is a string
    // until something opens it, and the browser it should open in is the one the
    // user is already signed into rather than a window of ours.
    //
    // `url` comes from the context, so a row holding one has it filled in
    // already: right-click, Open link. Anything else is typed, or passed by a
    // script — `tool.openLink(values.pullRequest)`.
    id: 'link.open',
    label: 'Open link',
    aliases: ['url', 'browser', 'open in browser', 'visit', 'website'],
    hint: 'Shell',
    scope: 'app',
    reach: 'external',
    args: [{ name: 'url', label: 'URL', fromContext: 'url', placeholder: 'https://…' }],
    run: async ({ url }) => {
      const href = String(url ?? '').trim()
      if (!href) throw new Error('Which link?')
      // The scheme is checked on the other side of the bridge, where it matters.
      await openExternal(href)
      return { message: `Opened ${href}` }
    },
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
  {
    // The skeleton, because assembling one by hand means creating a note, then
    // linking `@tools` to it by typing the id, then remembering which values it
    // has to carry. `execute` is written in rather than left out so that the
    // inspector has a string to show: it edits a string as itself, on more than
    // one line, and anything else as JSON on one escaped line.
    id: 'source.newTool',
    label: 'New tool of your own',
    aliases: ['define tool', 'user tool', 'write a tool', 'custom command'],
    hint: 'Shell',
    scope: 'app',
    reach: 'source',
    mutates: true,
    args: [{ name: 'name', label: 'Tool name', placeholder: 'e.g. greet' }],
    run: async ({ name }) => {
      const toolName = String(name ?? '').trim()
      if (!toolName) throw new Error('Tool name is required')
      const id = await createEntity(
        {
          // `text` is what the outline reads; `name` is what the palette does.
          // The same word to begin with, and free to diverge after.
          text: toolName,
          name: toolName,
          // Not what makes it a tool — the loader below reads a name and a body
          // and never looks at this — but it is what puts the fields, and what
          // each one is for, in the inspector rather than only in the docs.
          type: TOOL_ID,
          // A function of no arguments, since it has none declared yet. Adding
          // one to `arguments` means adding a parameter here, in the same order.
          execute: '() => {\n  // What the tool does.\n}',
        },
        TOOLS_ENTITY_ID,
      )
      // Straight into the inspector, which is where the body is written, along
      // with `arguments`, `key`, and whatever else the definition wants to say.
      updateUi({ inspectEntityId: id })
      return { message: `Fill ${toolName} in, then reload your tools` }
    },
  },
  {
    // The tools the user wrote are read once, when the source opens, so editing
    // one has no effect until they are read again. Reloading is a tool rather
    // than something a write triggers: a definition is edited a value at a time,
    // and rebuilding the registry on every keystroke would mean binding half a
    // key and running half a body.
    id: 'source.reloadTools',
    label: 'Reload your tools',
    aliases: ['user tools', 'refresh tools', 'rebuild registry'],
    hint: 'Shell',
    scope: 'app',
    reach: 'source',
    run: async () => {
      const { tools, skipped, warnings, linked } = await loadUserTools()
      // Which is the whole use of it. A definition that isn't quite one is passed
      // over, and a reload that only counted what it took would say the same
      // "0 tools" whether the note was wrong or merely unlinked.
      if (linked === 0) return { message: 'Nothing is linked under @tools' }
      const found = tools.length
        ? `${tools.length} tool${tools.length === 1 ? '' : 's'}: ${tools.map((t) => t.id).join(', ')}`
        : 'No tools'
      const said = [
        found,
        ...(skipped.length ? [`Skipped ${skipped.map((s) => `${s.id} (${s.why})`).join(', ')}`] : []),
        ...warnings.map((w) => `${w.id}: ${w.why}`),
      ]
      return { message: said.join('. ') }
    },
  },
]
