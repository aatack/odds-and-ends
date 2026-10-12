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
  /**
   * What it is called where there is no key to press: the phone's buttons. A
   * tool with a label is one of the app's actions, on every platform; one
   * without is a key's own business (moving the cursor, typing).
   */
  label?: string | ((session: Session) => string)
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

export const tools: Tool[] = [
  // --- Typing ---------------------------------------------------------------------
  // A window's prompt box (the side panel's, a peek's): its own, whatever else is waiting.
  { id: 'window.submit', scope: 'input', keys: ['Enter'], enabled: (_, field) => field === 'window', run: (s) => void s.sendWindow() },
  // Ctrl+Enter opened the side panel; in a window's box, Ctrl+Enter closes the window again.
  { id: 'window.closeFromPrompt', scope: 'input', keys: ['Ctrl+Enter'], enabled: (_, field) => field === 'window', run: (s) => s.closeWindow() },
  { id: 'window.leave', scope: 'input', keys: ['Escape'], enabled: (_, field) => field === 'window', run: (s) => s.leavePrompt() },
  // The find field: Enter hands the keyboard back to the tree, keeping the
  // filter; Escape clears it.
  { id: 'find.done', scope: 'input', keys: ['Enter'], enabled: (_, field) => field === 'find', run: () => blur() },
  { id: 'find.clear', scope: 'input', keys: ['Escape'], enabled: (_, field) => field === 'find', run: (s) => (blur(), s.clearFind()) },
  // Only in the box itself: a draft left waiting while I type elsewhere is not touched.
  { id: 'edit.commit', scope: 'input', keys: ['Enter'], enabled: (s, field) => field === 'edit' && s.hasEdit(), run: (s) => s.commitEdit() },
  { id: 'edit.cancel', scope: 'input', keys: ['Escape'], enabled: (s, field) => field === 'edit' && s.hasEdit(), run: (s) => s.cancelEdit() },
  {
    id: 'composer.send',
    scope: 'input',
    keys: ['Enter'],
    enabled: (s) => s.get().state.acting !== null,
    run: (s) => void s.perform(),
  },
  // A chat's prompt box: on to the next (or back to the previous) chat waiting for me.
  { id: 'chat.next', scope: 'input', keys: ['Tab'], enabled: (_, field) => field === 'prompt', run: (s) => s.nextChat(1) },
  { id: 'chat.previous', scope: 'input', keys: ['Shift+Tab'], enabled: (_, field) => field === 'prompt', run: (s) => s.nextChat(-1) },
  // Not while the new-session form is over it: Enter there is the form's.
  { id: 'composer.submit', scope: 'input', keys: ['Enter'], enabled: (s) => s.composerOpen() && !s.hasDialog(), run: (s) => void s.send() },
  // The new-session form: Enter makes it, Escape gives up.
  { id: 'dialog.submit', scope: 'input', keys: ['Enter'], enabled: (s, field) => s.hasDialog() && field?.startsWith('dialog') === true, run: (s) => s.submitDialog() },
  { id: 'dialog.cancel', scope: 'input', keys: ['Escape'], enabled: (s) => s.hasDialog(), run: (s) => s.cancelDialog() },
  { id: 'composer.leave', scope: 'input', keys: ['Escape'], run: (s) => s.compose(false) },

  // --- Escape, innermost first; the find last ------------------------------------
  { id: 'inspect.close', scope: 'list', keys: ['Escape', '3'], enabled: (s) => s.get().state.inspecting !== null, run: (s) => s.closeInspector() },
  { id: 'pick.cancel', label: 'Cancel', scope: 'list', keys: ['Escape'], enabled: (s) => picking(s), run: (s) => s.cancelPick() },
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

  { id: 'find.close', label: 'Clear find', scope: 'list', keys: ['Escape'], enabled: (s) => s.hasFind(), run: (s) => s.clearFind() },

  // --- Moving around the tree -------------------------------------------------------
  { id: 'select.down', scope: 'list', keys: ['s', 'ArrowDown'], run: (s) => s.move(1) },
  { id: 'select.up', scope: 'list', keys: ['w', 'ArrowUp'], run: (s) => s.move(-1) },
  { id: 'select.start', scope: 'list', keys: ['g', 'Home'], run: (s) => s.move(-Infinity) },
  { id: 'select.end', scope: 'list', keys: ['G', 'End'], run: (s) => s.move(Infinity) },
  { id: 'select.pageDown', scope: 'list', keys: ['PageDown', 'Ctrl+d'], run: (s) => s.move(15) },
  { id: 'select.pageUp', scope: 'list', keys: ['PageUp', 'Ctrl+u'], run: (s) => s.move(-15) },
  { id: 'expand', label: 'Unfold', scope: 'list', keys: ['ArrowRight'], enabled: notRoot, run: (s) => s.fold(true) },
  { id: 'collapse', label: 'Fold', scope: 'list', keys: ['ArrowLeft'], enabled: notRoot, run: (s) => s.fold(false) },
  { id: 'view.push', label: 'Open', scope: 'list', keys: ['d'], enabled: notRoot, run: (s) => s.open() },
  { id: 'select.parent', label: 'Parent', scope: 'list', keys: ['a'], enabled: notRoot, run: (s) => s.selectParent() },

  // --- Whatever is selected ----------------------------------------------------------
  { id: 'note.create', label: 'Note', scope: 'list', keys: ['Enter'], enabled: (s) => selected(s) !== null, run: (s) => s.startCreate() },
  { id: 'note.section', label: 'Heading', scope: 'list', keys: ['/'], enabled: (s) => selected(s) !== null, run: (s) => s.startCreate({ section: true }) },
  { id: 'note.checkbox', label: 'Checkbox', scope: 'list', keys: ['?'], enabled: (s) => selected(s) !== null, run: (s) => s.startCreate({ open: true }) },
  { id: 'edit.start', label: 'Edit', scope: 'list', keys: ['e'], enabled: (s) => selected(s) !== null, run: (s) => s.startEdit() },
  { id: 'unlink', label: 'Remove', scope: 'list', keys: ['Backspace', 'Delete'], enabled: notRoot, run: (s) => s.unlinkSelected() },
  // The second press of each finishes it on whatever is selected then, in any view.
  { id: 'move', label: (s) => (picking(s, 'move') ? 'Move here' : 'Move'), scope: 'list', keys: ['x'], enabled: (s) => picking(s, 'move') || notRoot(s), run: (s) => s.pick('move') },
  { id: 'link', label: (s) => (picking(s, 'link') ? 'Link here' : 'Link to…'), scope: 'list', keys: ['r'], enabled: (s) => picking(s, 'link') || selected(s) !== null, run: (s) => s.pick('link') },
  {
    id: 'link.reverse', label: (s) => (picking(s, 'linkReverse') ? 'Link here' : 'Link from…'),
    scope: 'list',
    keys: ['R'],
    enabled: (s) => picking(s, 'linkReverse') || selected(s) !== null,
    run: (s) => s.pick('linkReverse'),
  },
  {
    id: 'context', label: (s) => (picking(s, 'context') ? 'Share context here' : 'Context with…'),
    scope: 'list',
    keys: ['c'],
    enabled: (s) => picking(s, 'context') || selected(s) !== null,
    run: (s) => s.pick('context'),
  },
  { id: 'split', label: 'Split', scope: 'list', keys: ['b'], enabled: (s) => s.canSplit(), run: (s) => s.split() },
  { id: 'inspect', label: 'Inspect', scope: 'list', keys: ['3'], enabled: (s) => selected(s) !== null, run: (s) => s.inspect() },
  { id: 'claude.new', label: 'Claude session', scope: 'list', keys: ['K'], run: (s) => s.openClaudeDialog() },
  { id: 'session.send', label: 'To session', scope: 'list', keys: ['k'], enabled: (s) => s.canSendToSession(), run: (s) => s.sendToSession() },
  { id: 'side.open', label: 'In context', scope: 'app', keys: ['Ctrl+Enter'], run: (s) => s.openSide() },
  { id: 'side.close', label: 'Close side', scope: 'app', keys: [], enabled: (s) => s.get().state.side !== null, run: (s) => s.closeSide() },
  { id: 'checkbox.toggle', label: 'Tick', scope: 'list', keys: [' '], enabled: (s) => s.canToggle(), run: (s) => s.toggle() },
  {
    id: 'cache.wipe',
    label: (s) => ['Wipe cache', 'Wipe the cache? Press again', 'Really wipe it? Press to wipe'][s.get().state.wiping],
    scope: 'app',
    keys: [],
    run: (s) => s.wipeCache(),
  },
  { id: 'cache.wipeCancel', label: 'Keep cache', scope: 'app', keys: [], enabled: (s) => s.get().state.wiping > 0, run: (s) => s.cancelWipe() },
  {
    id: 'slack.pause',
    label: (s) => (s.slackPaused() ? 'Resume' : 'Pause'),
    scope: 'app',
    keys: [],
    enabled: (s) => s.root()?.type === 'slack.home',
    run: (s) => s.pauseSlack(),
  },
  { id: 'slack.markRead', label: 'Mark read', scope: 'list', keys: ['m'], enabled: (s) => s.readTarget() !== null, run: (s) => s.markRead() },

  // --- The view's root ----------------------------------------------------------------
  ...(
    [
      ['approve', 'p'],
      ['close', 'X'],
    ] as const
  ).map(
    ([action, key]): Tool => ({
      id: `action.${action}`,
      label: (s) => s.get().view?.actions.find((offered) => offered.id === action)?.label ?? action,
      scope: 'app',
      keys: [key],
      enabled: (s) => Boolean(s.get().view?.actions.some((offered) => offered.id === action && !offered.disabled)),
      run: (s) => s.startAction(action),
    }),
  ),
  { id: 'view.older', label: 'Older', scope: 'app', keys: ['o'], enabled: (s) => Boolean(s.get().view?.older), run: (s) => s.older() },
  { id: 'view.refresh', label: 'Refresh', scope: 'app', keys: ['F5'], run: (s) => s.refresh() },
  { id: 'view.find', label: 'Find', scope: 'app', keys: ['Ctrl+f'], run: (s) => s.openFind() },
  // In a text field these keys are the field's own (its typing's undo), so
  // they only reach the store from the tree.
  { id: 'undo', label: 'Undo', scope: 'app', keys: ['Ctrl+z'], enabled: (_, field) => field === null, run: (s) => void s.undo() },
  { id: 'redo', label: 'Redo', scope: 'app', keys: ['Ctrl+y'], enabled: (_, field) => field === null, run: (s) => void s.redo() },
  { id: 'view.back', label: 'Back', scope: 'app', keys: ['A', 'Alt+ArrowLeft'], run: (s) => s.back() },
  { id: 'view.forward', scope: 'app', keys: ['Alt+ArrowRight'], run: (s) => s.forward() },
  ...Array.from({ length: 9 }, (_, index): Tool => ({
    id: `module.${index + 1}`,
    scope: 'app',
    // `3` is the inspector's; the third module is Alt+3.
    keys: index === 2 ? [`Alt+${index + 1}`] : [String(index + 1), `Alt+${index + 1}`],
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

/** A tool's label, for a session as it stands. */
export function labelOf(tool: Tool, session: Session): string | null {
  if (!tool.label) return null
  return typeof tool.label === 'string' ? tool.label : tool.label(session)
}

/**
 * The app's actions that can be done right now, in the registry's order: what
 * the phone shows as buttons, so it can do what the keys do, and no more.
 */
export function actionsNow(session: Session): { id: string; label: string }[] {
  return tools.flatMap((tool) => {
    const label = labelOf(tool, session)
    if (!label || !(tool.enabled?.(session, null) ?? true)) return []
    return [{ id: tool.id, label }]
  })
}

/** Runs a tool by id, as if its key were pressed outside any text field. */
export function runTool(session: Session, id: string): void {
  const tool = tools.find((candidate) => candidate.id === id)
  if (tool && (tool.enabled?.(session, null) ?? true)) tool.run(session, null)
}
