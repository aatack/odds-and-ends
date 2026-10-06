import type { Session } from './session.ts'

/**
 * The single registry of what a person can do. Keys are matched by
 * `dispatch.ts`; nothing else listens for them.
 *
 * Scopes, innermost first: `input` while typing, then `list`, then `app`.
 */
export interface Tool {
  id: string
  keys: string[]
  scope: 'input' | 'list' | 'app'
  enabled?(session: Session): boolean
  run(session: Session): void
}

const hasChildren = (session: Session) => (session.get().focus?.children.length ?? 0) > 0
const focusType = (session: Session) => session.get().focus?.entity?.type

export const tools: Tool[] = [
  {
    id: 'composer.send',
    scope: 'input',
    keys: ['Enter'],
    run: (s) => (s.hasDraft() ? void s.send() : s.compose(false)),
  },
  { id: 'composer.leave', scope: 'input', keys: ['Escape'], run: (s) => s.compose(false) },

  {
    id: 'image.close',
    scope: 'list',
    keys: ['Escape', 'A', 'Backspace', 'ArrowLeft'],
    enabled: (s) => s.get().state.viewing !== null,
    run: (s) => s.view(null),
  },
  { id: 'cursor.down', scope: 'list', keys: ['s', 'ArrowDown'], run: (s) => s.move(1) },
  { id: 'cursor.up', scope: 'list', keys: ['w', 'ArrowUp'], run: (s) => s.move(-1) },
  { id: 'cursor.top', scope: 'list', keys: ['g', 'Home'], run: (s) => s.move(-Infinity) },
  { id: 'cursor.bottom', scope: 'list', keys: ['G', 'End'], run: (s) => s.move(Infinity) },
  { id: 'cursor.pageDown', scope: 'list', keys: ['PageDown', 'Ctrl+d'], run: (s) => s.move(15) },
  { id: 'cursor.pageUp', scope: 'list', keys: ['PageUp', 'Ctrl+u'], run: (s) => s.move(-15) },
  { id: 'focus.open', scope: 'list', keys: ['d', 'ArrowRight'], enabled: hasChildren, run: (s) => s.open() },
  {
    id: 'task.toggle',
    scope: 'list',
    keys: ['x', ' '],
    enabled: (s) => s.selected()?.type === 'task',
    run: (s) => s.toggle(),
  },
  {
    id: 'slack.markRead',
    scope: 'list',
    keys: ['m'],
    enabled: (s) => focusType(s) === 'slack.conversation',
    run: (s) => s.markRead(),
  },

  { id: 'focus.back', scope: 'app', keys: ['A', 'ArrowLeft', 'Backspace', 'Alt+ArrowLeft'], run: (s) => s.back() },
  { id: 'focus.forward', scope: 'app', keys: ['Alt+ArrowRight'], run: (s) => s.forward() },
  { id: 'focus.refresh', scope: 'app', keys: ['r', 'F5'], run: (s) => s.refresh() },
  {
    id: 'composer.enter',
    scope: 'app',
    keys: ['Enter'],
    enabled: (s) => Boolean(s.get().focus?.compose),
    run: (s) => s.compose(true),
  },
  ...Array.from({ length: 9 }, (_, index): Tool => ({
    id: `module.${index + 1}`,
    scope: 'app',
    keys: [String(index + 1), `Alt+${index + 1}`],
    run: (s) => s.openModule(index),
  })),
]
