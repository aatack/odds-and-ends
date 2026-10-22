import { refreshEntities } from '../../../core/cache'
import { openExternal, readClipboardText, revealPath } from '../source/files'
import { copyText } from '../helpers/clipboard'
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
    // For a change nothing here made: an agent that has been writing notes over
    // MCP for the last ten minutes, or somebody editing the same store from a
    // phone. The app's own writes need none of this — each one tells the cache
    // what it changed as it goes — so this is a gesture rather than something on
    // the end of every keystroke.
    //
    // Not `mutates`: it writes nothing, and so leaves the undo stack standing.
    id: 'source.refresh',
    label: 'Read the store again',
    aliases: ['refresh', 'reload', 'refetch', 'sync', 'update'],
    hint: 'Shell',
    scope: 'app',
    reach: 'source',
    run: () => {
      refreshEntities()
      return { message: 'Reading every entity again' }
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
    // somewhere else — a document, a ticket, a dashboard — is a string
    // until something opens it, and the browser it should open in is the one the
    // user is already signed into rather than a window of ours.
    //
    // `url` comes from the context, so a row holding one has it filled in
    // already: right-click, Open link. Anything else is typed, or passed by a
    // script — `tool.openLink(values.url)`.
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
    // The other half of what you do with something written down. `entity.copy`
    // copies a *row*; this copies whatever it is handed, which is what lets a
    // button in a note put its own URL, its branch name or its id on the
    // clipboard — `tool.copyToClipboard(values.permalink)`.
    id: 'clipboard.copy',
    label: 'Copy to clipboard',
    aliases: ['copy text', 'clipboard', 'copy url', 'copy link'],
    hint: 'Shell',
    scope: 'app',
    reach: 'ui',
    args: [{ name: 'text', label: 'Text', placeholder: 'What to copy' }],
    run: async ({ text }) => {
      const value = String(text ?? '')
      if (!value) throw new Error('Nothing to copy')
      await copyText(value)
      return { message: 'Copied' }
    },
  },
  {
    // The other direction, and the one the app had no way to go: `resource.paste`
    // takes *bytes* off the clipboard, and nothing took words. What it read is the
    // result, so a script can be "take the link I just copied and hang it under
    // this note" — `tool['entity.create']({ text: tool.readTheClipboard() })`.
    id: 'clipboard.read',
    label: 'Read the clipboard',
    aliases: ['paste text', 'clipboard text', 'what did I copy'],
    hint: 'Shell',
    scope: 'app',
    reach: 'ui',
    run: async () => {
      const text = await readClipboardText()
      if (!text) throw new Error('Nothing on the clipboard')
      return { data: text, message: text.length > 80 ? `${text.slice(0, 80)}…` : text }
    },
  },
  {
    // `link.open` opens what a note points at; this one says where it *is*, which
    // for a path on this machine is the more useful half —
    // `tool.revealInFileManager(values.path)` — so the argument is a plain path and
    // the context key is `path`.
    id: 'file.reveal',
    label: 'Reveal in file manager',
    aliases: ['show in folder', 'finder', 'explorer', 'locate', 'where is it'],
    hint: 'Shell',
    scope: 'app',
    reach: 'external',
    args: [{ name: 'path', label: 'Path', fromContext: 'path', placeholder: '~/repos/x' }],
    run: async ({ path }) => {
      const target = String(path ?? '').trim()
      if (!target) throw new Error('Which path?')
      return { message: `Showed ${await revealPath(target)}` }
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
    // The one way to the sources page, and the one tool about sources: what is
    // there is a graph of pensives, and everything that used to be a tool about
    // them — a server, a token, a debug console — is a node on it.
    id: 'page.sources',
    label: 'Configure sources',
    aliases: ['sources', 'pensives', 'connections', 'configuration', 'tokens', 'broadcast'],
    hint: 'Navigate',
    scope: 'app',
    reach: 'ui',
    run: () => updateUi({ page: 'sources' }),
  },
]
