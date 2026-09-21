import type { Session } from './session'
import type { ComposerMode } from './state'

export interface Binding {
  key: string
  ctrl?: boolean
  meta?: boolean
  shift?: boolean
}

export type Scope = 'app' | 'feed' | 'composer' | 'overlay'

export interface Tool {
  id: string
  title: string
  /** Shown next to the title in the command palette and the help sheet. */
  hint?: string
  section: 'Move around' | 'Do something' | 'Put it down' | 'The app'
  scope: Scope
  bindings: Binding[]
  enabled(session: Session): boolean
  run(session: Session): void | Promise<void>
}

const focused = (session: Session): boolean => session.getState().focus !== null

function compose(mode: ComposerMode, key: string, title: string, hint: string): Tool {
  return {
    id: `compose.${mode}`,
    title,
    hint,
    section: 'Do something',
    scope: 'feed',
    bindings: [{ key }],
    enabled: focused,
    run: (session) => session.openComposer(mode),
  }
}

function switchMode(mode: ComposerMode, key: string, title: string): Tool {
  return {
    id: `composer.mode.${mode}`,
    title,
    section: 'Do something',
    scope: 'composer',
    bindings: [{ key, ctrl: true }, { key, meta: true }],
    enabled: (session) => session.getState().composer !== null,
    run: (session) => session.setComposerMode(mode),
  }
}

/**
 * Everything the app can do, in one place. Hotkeys, the command palette and the
 * hints in the composer all read from here, so they cannot drift apart.
 */
export const tools: Tool[] = [
  {
    id: 'queue.next',
    title: 'Go to the next open topic',
    hint: 'the whole point of the app',
    section: 'Move around',
    scope: 'app',
    bindings: [{ key: '.' }],
    enabled: () => true,
    run: (session) => session.nextOpen(),
  },
  {
    id: 'navigate.back',
    title: 'Back to where I was',
    section: 'Move around',
    scope: 'app',
    bindings: [{ key: '[' }],
    enabled: (session) => session.getState().trail.length > 0,
    run: (session) => session.back(),
  },
  {
    id: 'cursor.down',
    title: 'Down the feed',
    section: 'Move around',
    scope: 'feed',
    bindings: [{ key: 'j' }, { key: 'ArrowDown' }],
    enabled: focused,
    run: (session) => session.moveCursor(1),
  },
  {
    id: 'cursor.up',
    title: 'Up the feed',
    section: 'Move around',
    scope: 'feed',
    bindings: [{ key: 'k' }, { key: 'ArrowUp' }],
    enabled: focused,
    run: (session) => session.moveCursor(-1),
  },
  {
    id: 'row.toggle',
    title: 'Expand or collapse a subtopic',
    section: 'Move around',
    scope: 'feed',
    bindings: [{ key: ' ' }, { key: 'Tab' }],
    enabled: (session) => session.cursorRow()?.expandable === true,
    run: (session) => {
      const row = session.cursorRow()
      if (row) session.toggleExpanded(row.node.topic.id)
    },
  },
  {
    id: 'row.focus',
    title: 'Look at this subtopic on its own',
    section: 'Move around',
    scope: 'feed',
    bindings: [{ key: 'o' }],
    enabled: (session) => session.cursorRow() !== null,
    run: (session) => {
      const row = session.cursorRow()
      if (row) session.focusTopic(row.node.topic.id)
    },
  },
  {
    id: 'row.promote',
    title: 'Open this subtopic as work of its own',
    hint: 'and wait for it before coming back here',
    section: 'Put it down',
    scope: 'feed',
    bindings: [{ key: 'p' }],
    enabled: (session) => session.cursorRow() !== null,
    run: (session) => {
      const row = session.cursorRow()
      if (row) return session.promote(row.node.topic.id)
    },
  },
  {
    id: 'cursor.clear',
    title: 'Drop the cursor',
    section: 'Move around',
    scope: 'feed',
    bindings: [{ key: 'Escape' }],
    enabled: (session) => session.getState().cursor !== null,
    run: (session) => session.update((state) => ({ ...state, cursor: null })),
  },

  compose('note', 'Enter', 'Write a note', 'enter again to keep it'),
  compose('slack', 's', 'Reply in Slack', 'goes to the thread this topic came from'),
  compose('claude', 'c', 'Ask Claude', 'in this topic’s own worktree'),
  compose('subtopic', 't', 'Start a subtopic here', ''),
  {
    id: 'topic.new',
    title: 'Start a new topic',
    section: 'Do something',
    scope: 'app',
    bindings: [{ key: 'n' }],
    enabled: () => true,
    run: (session) => session.openComposer('topic'),
  },
  {
    id: 'topic.rename',
    title: 'Rename this topic',
    section: 'Do something',
    scope: 'feed',
    bindings: [{ key: 'e' }],
    enabled: focused,
    run: (session) => session.openComposer('rename', session.detail().data?.topic.text ?? ''),
  },

  {
    id: 'topic.details',
    title: 'Show what this topic knows',
    hint: 'its metadata',
    section: 'The app',
    scope: 'feed',
    bindings: [{ key: 'i' }],
    enabled: focused,
    run: (session) => session.update((state) => ({ ...state, details: !state.details })),
  },
  {
    id: 'topic.block',
    title: 'Put this down until…',
    hint: 'pick something to wait for',
    section: 'Put it down',
    scope: 'feed',
    bindings: [{ key: 'b' }],
    enabled: focused,
    run: (session) => session.openOverlay('blockers'),
  },
  {
    id: 'topic.resolve',
    title: 'Sign this off',
    section: 'Put it down',
    scope: 'feed',
    bindings: [{ key: 'r' }],
    enabled: (session) => focused(session) && session.detail().data?.topic.resolved !== true,
    run: (session) => session.setResolved(true),
  },
  {
    id: 'topic.unresolve',
    title: 'Take the sign-off back',
    section: 'Put it down',
    scope: 'feed',
    bindings: [{ key: 'R', shift: true }],
    enabled: (session) => session.detail().data?.topic.resolved === true,
    run: (session) => session.setResolved(false),
  },
  {
    id: 'topic.reopen',
    title: 'Pick this back up now',
    hint: 'drops everything it was waiting for',
    section: 'Put it down',
    scope: 'feed',
    bindings: [{ key: 'u' }],
    enabled: (session) => session.detail().data?.topic.open === false,
    run: (session) => session.reopen(),
  },

  {
    id: 'topic.link',
    title: 'Associate another topic with this one',
    section: 'Do something',
    scope: 'feed',
    bindings: [{ key: 'l' }],
    enabled: focused,
    run: (session) => session.openOverlay('link'),
  },
  {
    id: 'app.commands',
    title: 'Everything I can do',
    section: 'The app',
    scope: 'app',
    bindings: [
      { key: 'k', ctrl: true },
      { key: 'k', meta: true },
    ],
    enabled: () => true,
    run: (session) => session.openOverlay('commands'),
  },
  {
    id: 'app.search',
    title: 'Find a topic',
    section: 'The app',
    scope: 'app',
    bindings: [{ key: '/' }],
    enabled: () => true,
    run: (session) => session.openOverlay('search'),
  },
  {
    id: 'app.help',
    title: 'Keys',
    section: 'The app',
    scope: 'app',
    bindings: [{ key: '?', shift: true }],
    enabled: () => true,
    run: (session) => session.openOverlay('help'),
  },
  {
    id: 'app.theme',
    title: 'Light or dark',
    section: 'The app',
    scope: 'app',
    bindings: [{ key: 'd' }],
    enabled: () => true,
    run: (session) => session.toggleTheme(),
  },
  {
    id: 'app.theme.system',
    title: 'Follow the machine’s appearance',
    section: 'The app',
    scope: 'app',
    bindings: [],
    enabled: (session) => session.getState().theme !== 'system',
    run: (session) => session.setTheme('system'),
  },
  {
    id: 'app.sidebar',
    title: 'Show or hide the queue',
    section: 'The app',
    scope: 'app',
    bindings: [{ key: '\\' }],
    enabled: () => true,
    run: (session) => session.update((state) => ({ ...state, sidebar: !state.sidebar })),
  },

  {
    id: 'composer.submit',
    title: 'Send what I typed',
    section: 'Do something',
    scope: 'composer',
    bindings: [{ key: 'Enter' }],
    enabled: (session) => session.getState().composer !== null,
    run: async (session) => {
      const composer = session.getState().composer
      if (!composer) return
      const text = composer.text.trim()
      if (text.length === 0) {
        session.closeComposer()
        return
      }
      if (composer.mode === 'note') await session.addNote(text)
      else if (composer.mode === 'slack') await session.sendSlack(text)
      else if (composer.mode === 'claude') await session.promptClaude(text)
      else if (composer.mode === 'subtopic') await session.createSubtopic(text)
      else if (composer.mode === 'topic') await session.createTopic(text)
      else if (composer.mode === 'rename') {
        await session.rename(text)
        session.closeComposer()
      }
    },
  },
  {
    id: 'composer.cancel',
    title: 'Put the box away',
    section: 'Do something',
    scope: 'composer',
    bindings: [{ key: 'Escape' }],
    enabled: (session) => session.getState().composer !== null,
    run: (session) => session.closeComposer(),
  },
  switchMode('note', 'd', 'Turn it into a note'),
  switchMode('slack', 's', 'Turn it into a Slack message'),
  switchMode('claude', 'p', 'Turn it into a prompt for Claude'),
  switchMode('subtopic', 't', 'Turn it into a subtopic'),

  {
    id: 'overlay.down',
    title: 'Next choice',
    section: 'Move around',
    scope: 'overlay',
    bindings: [{ key: 'ArrowDown' }, { key: 'n', ctrl: true }],
    enabled: (session) => session.getState().overlay !== null,
    run: (session) => session.updateOverlay((overlay) => ({ ...overlay, index: overlay.index + 1 })),
  },
  {
    id: 'overlay.up',
    title: 'Previous choice',
    section: 'Move around',
    scope: 'overlay',
    bindings: [{ key: 'ArrowUp' }, { key: 'p', ctrl: true }],
    enabled: (session) => session.getState().overlay !== null,
    run: (session) => session.updateOverlay((overlay) => ({ ...overlay, index: Math.max(overlay.index - 1, 0) })),
  },
  {
    id: 'overlay.close',
    title: 'Never mind',
    section: 'Move around',
    scope: 'overlay',
    bindings: [{ key: 'Escape' }],
    enabled: (session) => session.getState().overlay !== null,
    run: (session) => session.closeOverlay(),
  },
]

export const toolsById = new Map(tools.map((tool) => [tool.id, tool]))

export function keyLabel(binding: Binding): string {
  const parts: string[] = []
  if (binding.ctrl) parts.push('⌃')
  if (binding.meta) parts.push('⌘')
  if (binding.shift && binding.key.length > 1) parts.push('⇧')
  const names: Record<string, string> = {
    Enter: '↵',
    ArrowDown: '↓',
    ArrowUp: '↑',
    Escape: 'esc',
    ' ': 'space',
    Tab: 'tab',
  }
  parts.push(names[binding.key] ?? binding.key)
  return parts.join('')
}

export function toolLabel(tool: Tool): string {
  const binding = tool.bindings[0]
  return binding ? keyLabel(binding) : ''
}
