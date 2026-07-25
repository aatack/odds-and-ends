import { pendingAtom } from '../state/store'
import { cancelCall, pickForPending, runTool, togglePalette } from './call'
import { bindsKey, matchesKey, type KeyBinding } from './keys'
import { TOOLS, findTool } from './registry'
import type { ToolScope } from './types'

// The only keydown listener in the app. Keys act on global state, and resolve
// through the focus chain rather than through whichever component happens to
// have DOM focus — so with many frames mounted a bare `w` moves the selection in
// exactly one of them.

/** ⌘/Ctrl+P belongs to the call machine, not to any tool. */
export const PALETTE_KEY: KeyBinding = { key: 'p', mod: true }

/** Innermost first: a frame shadows its group, which shadows the app. */
const SCOPES: ToolScope[] = ['frame', 'group', 'app']

export function isEditableTarget(target: EventTarget | null): boolean {
  if (!(target instanceof HTMLElement)) return false
  const tag = target.tagName
  return tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || target.isContentEditable
}

/**
 * Modifier combos a focused text field owns. Bare keys already belong to it, but
 * these look like app shortcuts and aren't: ⌘Z inside an in-place edit should
 * undo the typing, not take events off the database.
 */
const TEXT_EDITING_KEYS = new Set(['z', 'y', 'x', 'c', 'v', 'a'])

const isTextEditingKey = (e: KeyboardEvent): boolean =>
  (e.ctrlKey || e.metaKey) && !e.altKey && TEXT_EDITING_KEYS.has(e.key.toLowerCase())

/** Route one keystroke. Returns true when it was consumed. */
export function handleKey(e: KeyboardEvent): boolean {
  if (matchesKey(PALETTE_KEY, e)) {
    togglePalette()
    return true
  }

  const pending = pendingAtom.get()
  if (pending) {
    // On screen as a palette, it owns the keyboard: its input handles the rest.
    if (pending.display.kind === 'palette') return false
    if (e.key === 'Escape') {
      cancelCall()
      return true
    }
    // Pressing the waiting tool's own key again supplies the argument it is
    // waiting on from the live selection.
    const tool = pending.toolId ? findTool(pending.toolId) : null
    if (tool && bindsKey(tool.keys, e)) {
      pickForPending()
      return true
    }
    // Everything else falls through, so the selection can still be moved to
    // choose a target.
  }

  // A focused text field owns bare keys and the editing combos; Escape and other
  // modifier combos still route, so ctrl+Tab works mid-edit.
  if (isEditableTarget(e.target)) {
    if (isTextEditingKey(e)) return false
    if (e.key !== 'Escape' && !(e.ctrlKey || e.metaKey || e.altKey)) return false
  }

  for (const scope of SCOPES) {
    const tool = TOOLS.find(
      (t) => t.scope === scope && bindsKey(t.keys, e) && (t.enabled?.() ?? true),
    )
    if (tool) {
      runTool(tool.id)
      return true
    }
  }
  return false
}

/** Bind the router to the window. Returns the teardown. */
export function installKeyRouter(): () => void {
  const onKeyDown = (e: KeyboardEvent): void => {
    if (handleKey(e)) e.preventDefault()
  }
  window.addEventListener('keydown', onKeyDown)
  return () => window.removeEventListener('keydown', onKeyDown)
}
