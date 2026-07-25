// The shared key-binding vocabulary. A binding is matched against a keyboard
// event; tools declare their own, and the router in ./dispatch is the only place
// that listens for keystrokes.

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

export function matchesKey(b: KeyBinding, e: KeyEvent): boolean {
  return (
    b.key.toLowerCase() === e.key.toLowerCase() &&
    (b.shift ?? false) === e.shiftKey &&
    (b.mod ?? false) === (e.ctrlKey || e.metaKey) &&
    (b.alt ?? false) === e.altKey
  )
}

export const bindsKey = (keys: KeyBinding[] | undefined, e: KeyEvent): boolean =>
  !!keys?.some((k) => matchesKey(k, e))

const SYMBOLS: Record<string, string> = {
  Enter: 'Enter',
  Backspace: '⌫',
  Delete: '⌦',
  ArrowLeft: '←',
  ArrowRight: '→',
  ArrowUp: '↑',
  ArrowDown: '↓',
  Escape: 'Esc',
  Tab: '⇥',
}

/** A short human label for the first of some bindings, for palette rows. */
export function keyHint(keys?: KeyBinding[]): string | undefined {
  const b = keys?.[0]
  if (!b) return undefined
  let hint = SYMBOLS[b.key] ?? b.key.toUpperCase()
  if (b.shift) hint = `⇧${hint}`
  if (b.alt) hint = `Alt ${hint}`
  if (b.mod) hint = `Ctrl ${hint}`
  return hint
}
