// Shared key-binding vocabulary for the action registries. A registry is just a
// list of actions that each optionally declare the keys that trigger them; the
// hotkey handler and the command palette both read from the same list, so a
// shortcut and its palette entry can never drift apart.

export interface KeyBinding {
  key: string
  shift?: boolean
  /** Requires Ctrl (Windows/Linux) or ⌘ (Mac). */
  mod?: boolean
  /** Requires Alt (⌥). */
  alt?: boolean
}

/** The subset of a keyboard event a binding cares about. */
export interface KeyEvent {
  key: string
  shiftKey: boolean
  ctrlKey: boolean
  metaKey: boolean
  altKey: boolean
}

function bindingMatches(b: KeyBinding, e: KeyEvent): boolean {
  return (
    b.key.toLowerCase() === e.key.toLowerCase() &&
    (b.shift ?? false) === e.shiftKey &&
    (b.mod ?? false) === (e.ctrlKey || e.metaKey) &&
    (b.alt ?? false) === e.altKey
  )
}

/** The first action in `actions` whose keys match the event, if any. */
export function matchAction<T extends { keys?: KeyBinding[] }>(
  actions: T[],
  e: KeyEvent,
): T | undefined {
  return actions.find((a) => a.keys?.some((k) => bindingMatches(k, e)))
}

const KEY_SYMBOLS: Record<string, string> = {
  Enter: 'Enter',
  Backspace: '⌫',
  Delete: '⌦',
  ArrowLeft: '←',
  ArrowRight: '→',
  Escape: 'Esc',
  Tab: '⇥',
}

/** A short human label for the first of some key bindings, for palettes/menus. */
export function hotkeyHint(keys?: KeyBinding[]): string | undefined {
  const b = keys?.[0]
  if (!b) return undefined
  let hint = KEY_SYMBOLS[b.key] ?? b.key.toUpperCase()
  if (b.shift) hint = `⇧${hint}`
  if (b.alt) hint = `Alt ${hint}`
  if (b.mod) hint = `Ctrl ${hint}`
  return hint
}
