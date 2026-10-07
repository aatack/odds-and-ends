import type { Session } from './session.ts'
import * as S from './state.ts'

/**
 * The single registry of what a person can do. Keys are matched by
 * `dispatch.ts`; nothing else listens for them.
 *
 * Scopes, innermost first: `input` while typing, then `list` (the view on
 * screen), then `app`. The first enabled tool bound to a key wins, which is how
 * Escape means "stop editing", "give up this move" and "close the peek"
 * without any of them knowing about the others.
 */
export interface Tool {
  id: string
  keys: string[]
  scope: 'input' | 'list' | 'app'
  /** `field` is the text field the key was pressed in (its `data-field`), if any. */
  enabled?(session: Session, field: string | null): boolean
  run(session: Session, field: string | null): void
}

const selected = (s: Session) => s.selected()
const notRoot = (s: Session) => (selected(s)?.depth ?? 0) > 0
const picking = (s: Session, tool?: S.PickTool) => {
  const pick = s.get().state.picking
  return tool ? pick?.tool === tool : pick !== null
}
const rootType = (s: Session) => s.root()?.type

export const tools: Tool[] = [
  // --- Typing ---------------------------------------------------------------------
  // The find field: Enter hands the keyboard back to the tree, keeping the
  // filter; Escape clears it.
  { id: 'find.done', scope: 'input', keys: ['Enter'], enabled: (_, field) => field === 'find', run: () => blur() },
  { id: 'find.clear', scope: 'input', keys: ['Escape'], enabled: (_, field) => field === 'find', run: (s) => (blur(), s.clearFind()) },
  { id: 'edit.commit', scope: 'input', keys: ['Enter'], enabled: (s) => s.hasEdit(), run: (s) => s.commitEdit() },
  { id: 'edit.cancel', scope: 'input', keys: ['Escape'], enabled: (s) => s.hasEdit(), run: (s) => s.cancelEdit() },
  {
    id: 'composer.send',
    scope: 'input',
    keys: ['Enter'],
    enabled: (s) => s.get().state.acting !== null,
    run: (s) => void s.perform(),
  },
  { id: 'composer.submit', scope: 'input', keys: ['Enter'], enabled: (s) => s.composerOpen(), run: (s) => void s.send() },
  { id: 'composer.leave', scope: 'input', keys: ['Escape'], run: (s) => s.compose(false) },

  // --- Escape, innermost first; the find last ------------------------------------
  { id: 'pick.cancel', scope: 'list', keys: ['Escape'], enabled: (s) => picking(s), run: (s) => s.cancelPick() },
  {
    id: 'peek.close',
    scope: 'list',
    keys: ['Escape'],
    enabled: (s) => S.transientPeek(s.get().state) !== null,
    run: (s) => s.closePeek(null),
  },
  {
    id: 'image.close',
    scope: 'list',
    keys: ['Escape', 'A', 'Backspace', 'ArrowLeft'],
    enabled: (s) => s.get().state.viewing !== null,
    run: (s) => s.view(null),
  },

  { id: 'find.close', scope: 'list', keys: ['Escape'], enabled: (s) => s.hasFind(), run: (s) => s.clearFind() },

  // --- Moving around the tree -------------------------------------------------------
  { id: 'select.down', scope: 'list', keys: ['s', 'ArrowDown'], run: (s) => s.move(1) },
  { id: 'select.up', scope: 'list', keys: ['w', 'ArrowUp'], run: (s) => s.move(-1) },
  { id: 'select.start', scope: 'list', keys: ['g', 'Home'], run: (s) => s.move(-Infinity) },
  { id: 'select.end', scope: 'list', keys: ['G', 'End'], run: (s) => s.move(Infinity) },
  { id: 'select.pageDown', scope: 'list', keys: ['PageDown', 'Ctrl+d'], run: (s) => s.move(15) },
  { id: 'select.pageUp', scope: 'list', keys: ['PageUp', 'Ctrl+u'], run: (s) => s.move(-15) },
  { id: 'expand', scope: 'list', keys: ['ArrowRight'], enabled: notRoot, run: (s) => s.fold(true) },
  { id: 'collapse', scope: 'list', keys: ['ArrowLeft'], enabled: notRoot, run: (s) => s.fold(false) },
  { id: 'view.push', scope: 'list', keys: ['d'], enabled: notRoot, run: (s) => s.open() },
  { id: 'select.parent', scope: 'list', keys: ['a'], enabled: notRoot, run: (s) => s.selectParent() },

  // --- Whatever is selected ----------------------------------------------------------
  { id: 'note.create', scope: 'list', keys: ['Enter'], enabled: (s) => selected(s) !== null, run: (s) => s.startCreate() },
  { id: 'note.section', scope: 'list', keys: ['/'], enabled: (s) => selected(s) !== null, run: (s) => s.startCreate({ section: true }) },
  { id: 'note.checkbox', scope: 'list', keys: ['?'], enabled: (s) => selected(s) !== null, run: (s) => s.startCreate({ open: true }) },
  { id: 'edit.start', scope: 'list', keys: ['e'], enabled: (s) => selected(s) !== null, run: (s) => s.startEdit() },
  { id: 'unlink', scope: 'list', keys: ['Backspace', 'Delete'], enabled: notRoot, run: (s) => s.unlinkSelected() },
  // The second press of each finishes it on whatever is selected then, in any view.
  { id: 'move', scope: 'list', keys: ['x'], enabled: (s) => picking(s, 'move') || notRoot(s), run: (s) => s.pick('move') },
  { id: 'link', scope: 'list', keys: ['r'], enabled: (s) => picking(s, 'link') || selected(s) !== null, run: (s) => s.pick('link') },
  {
    id: 'link.reverse',
    scope: 'list',
    keys: ['R'],
    enabled: (s) => picking(s, 'linkReverse') || selected(s) !== null,
    run: (s) => s.pick('linkReverse'),
  },
  { id: 'checkbox.toggle', scope: 'list', keys: [' '], enabled: (s) => s.canToggle(), run: (s) => s.toggle() },
  {
    id: 'chat.hide',
    scope: 'list',
    keys: ['Shift+Backspace', 'Shift+Delete'],
    enabled: (s) => selected(s)?.entity.type === 'slack.message' || rootType(s) === 'slack.message',
    run: (s) => s.hideChatOfMessage(),
  },
  { id: 'slack.markRead', scope: 'list', keys: ['m'], enabled: (s) => rootType(s) === 'slack.conversation', run: (s) => s.markRead() },

  // --- The view's root ----------------------------------------------------------------
  ...(
    [
      ['approve', 'p'],
      ['close', 'X'],
    ] as const
  ).map(
    ([action, key]): Tool => ({
      id: `action.${action}`,
      scope: 'app',
      keys: [key],
      enabled: (s) => Boolean(s.get().view?.actions.some((offered) => offered.id === action && !offered.disabled)),
      run: (s) => s.startAction(action),
    }),
  ),
  { id: 'view.older', scope: 'app', keys: ['o'], enabled: (s) => Boolean(s.get().view?.older), run: (s) => s.older() },
  { id: 'view.refresh', scope: 'app', keys: ['F5'], run: (s) => s.refresh() },
  { id: 'view.find', scope: 'app', keys: ['Ctrl+f'], run: (s) => s.openFind() },
  // In a text field these keys are the field's own (its typing's undo), so
  // they only reach the store from the tree.
  { id: 'undo', scope: 'app', keys: ['Ctrl+z'], enabled: (_, field) => field === null, run: (s) => void s.undo() },
  { id: 'redo', scope: 'app', keys: ['Ctrl+y', 'Ctrl+Z'], enabled: (_, field) => field === null, run: (s) => void s.redo() },
  { id: 'view.back', scope: 'app', keys: ['A', 'Alt+ArrowLeft'], run: (s) => s.back() },
  { id: 'view.forward', scope: 'app', keys: ['Alt+ArrowRight'], run: (s) => s.forward() },
  ...Array.from({ length: 9 }, (_, index): Tool => ({
    id: `module.${index + 1}`,
    scope: 'app',
    keys: [String(index + 1), `Alt+${index + 1}`],
    run: (s) => s.openModule(index),
  })),
]

/** Hands the keyboard back from a text field to the tree. */
function blur(): void {
  if (document.activeElement instanceof HTMLElement) document.activeElement.blur()
}

const keyNames: Record<string, string> = {
  Shift: '⇧',
  Ctrl: 'Ctrl',
  Alt: 'Alt',
  Backspace: '⌫',
  Delete: 'Del',
  Enter: '↵',
  Escape: 'Esc',
  ArrowLeft: '←',
  ArrowRight: '→',
  ArrowUp: '↑',
  ArrowDown: '↓',
  ' ': 'Space',
}

/** A key as a key cap shows it: `Shift+Backspace` → `⇧⌫`. */
export function keyLabel(key: string): string {
  return key
    .split('+')
    .map((part) => keyNames[part] ?? part)
    .join('')
}

/**
 * The key a tool is bound to, as shown on its button: the first of its keys.
 * Read from the registry, so a button can't show a key that doesn't do it.
 */
export function hotkeyOf(toolId: string): string | undefined {
  const key = tools.find((tool) => tool.id === toolId)?.keys[0]
  return key === undefined ? undefined : keyLabel(key)
}
