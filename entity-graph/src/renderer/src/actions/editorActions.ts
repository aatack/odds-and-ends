// The editor's command registry: every action the user can trigger against the
// open entity tree, defined once and separately from the state it mutates. Both
// the keyboard handler and the command palette dispatch through this list, so a
// hotkey and its palette entry can never drift apart.
//
// Each action's `run` receives an EditorController — a small imperative handle
// the useEditor hook provides — so the definitions here hold no state of their
// own.

/** The imperative operations the editor exposes for actions to call. */
export interface EditorController {
  moveSelection: (delta: number) => void
  selectParent: () => void
  collapseSelected: () => void
  expandSelected: () => void
  startEdit: () => void
  startCreate: () => void
  unlinkSelected: () => void
  toggleMove: () => void
  toggleLink: (reverse: boolean) => void
  cancelPending: () => void
  exportSelected: () => void
  debugSelected: () => void
}

export interface KeyBinding {
  key: string
  shift?: boolean
}

export interface EditorAction {
  id: string
  label: string
  /** Keys that trigger it. Omitted for actions only reachable via menu/palette. */
  keys?: KeyBinding[]
  /** Whether it appears in the command palette (default true). */
  palette?: boolean
  run: (c: EditorController) => void
}

export const EDITOR_ACTIONS: EditorAction[] = [
  { id: 'move-up', label: 'Move selection up', keys: [{ key: 'w' }], run: (c) => c.moveSelection(-1) },
  { id: 'move-down', label: 'Move selection down', keys: [{ key: 's' }], run: (c) => c.moveSelection(1) },
  { id: 'select-parent', label: 'Select parent', keys: [{ key: 'a' }], run: (c) => c.selectParent() },
  { id: 'collapse', label: 'Collapse', keys: [{ key: 'ArrowLeft' }], run: (c) => c.collapseSelected() },
  { id: 'expand', label: 'Expand', keys: [{ key: 'ArrowRight' }], run: (c) => c.expandSelected() },
  { id: 'edit', label: 'Edit text', keys: [{ key: 'e' }], run: (c) => c.startEdit() },
  { id: 'create-child', label: 'Create child', keys: [{ key: 'Enter' }], run: (c) => c.startCreate() },
  {
    id: 'unlink',
    label: 'Remove from parent',
    keys: [{ key: 'Backspace' }, { key: 'Delete' }],
    run: (c) => c.unlinkSelected(),
  },
  { id: 'move', label: 'Move to…', keys: [{ key: 'x' }], run: (c) => c.toggleMove() },
  { id: 'link', label: 'Link to…', keys: [{ key: 'r', shift: false }], run: (c) => c.toggleLink(false) },
  {
    id: 'link-reverse',
    label: 'Link to… (reversed)',
    keys: [{ key: 'r', shift: true }],
    run: (c) => c.toggleLink(true),
  },
  {
    id: 'cancel',
    label: 'Cancel pending move/link',
    keys: [{ key: 'Escape' }],
    palette: false,
    run: (c) => c.cancelPending(),
  },
  { id: 'export', label: 'Export subtree as markdown', run: (c) => c.exportSelected() },
  { id: 'debug', label: 'Debug entity', run: (c) => c.debugSelected() },
]

/** The action a key event triggers, if any. */
export function matchAction(e: { key: string; shiftKey: boolean }): EditorAction | undefined {
  const key = e.key.toLowerCase()
  return EDITOR_ACTIONS.find((a) =>
    a.keys?.some((k) => k.key.toLowerCase() === key && (k.shift ?? false) === e.shiftKey),
  )
}

const KEY_SYMBOLS: Record<string, string> = {
  Enter: 'Enter',
  Backspace: '⌫',
  Delete: '⌦',
  ArrowLeft: '←',
  ArrowRight: '→',
  Escape: 'Esc',
}

/** A short human label for an action's first hotkey, for the palette. */
export function hotkeyHint(action: EditorAction): string | undefined {
  const binding = action.keys?.[0]
  if (!binding) return undefined
  const key = KEY_SYMBOLS[binding.key] ?? binding.key.toUpperCase()
  return binding.shift ? `⇧${key}` : key
}
