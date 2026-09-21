import type { Session } from './session'
import type { Scope } from './tools'
import { tools } from './tools'
import { clampIndex, overlayItems } from './overlay'
import type { Binding } from './tools'

export interface KeyPress {
  key: string
  ctrlKey: boolean
  metaKey: boolean
  shiftKey: boolean
  altKey: boolean
}

function matches(binding: Binding, press: KeyPress): boolean {
  if (binding.key.length === 1 && press.key.length === 1) {
    if (binding.key.toLowerCase() !== press.key.toLowerCase()) return false
    // An upper case binding means the shifted key, not the same key with shift.
    if (binding.key !== binding.key.toLowerCase() && !press.shiftKey) return false
  } else if (binding.key !== press.key || (binding.shift ?? false) !== press.shiftKey) {
    return false
  }
  if ((binding.ctrl ?? false) !== press.ctrlKey) return false
  if ((binding.meta ?? false) !== press.metaKey) return false
  if (binding.shift === true && !press.shiftKey) return false
  return true
}

/**
 * The order keys are offered in: whatever is in front gets first refusal, and the
 * app only sees a key that nothing nearer has claimed.
 */
export function scopeChain(session: Session): Scope[] {
  const state = session.getState()
  if (state.overlay) return ['overlay', 'app']
  if (state.composer) return ['composer', 'app']
  return ['feed', 'app']
}

/**
 * The single listener. While I am typing, only the box I am typing in and things
 * held down with a modifier get a say, so a letter is always just a letter.
 */
export function dispatchKey(session: Session, press: KeyPress): boolean {
  const state = session.getState()
  const typing = state.composer !== null || state.overlay !== null
  const chain = scopeChain(session)

  if (state.overlay && press.key === 'Enter' && !press.ctrlKey && !press.metaKey) {
    const items = overlayItems(session)
    const item = items[clampIndex(items, state.overlay.index)]
    if (item) {
      void item.run(session)
      return true
    }
    return true
  }

  for (const scope of chain) {
    for (const tool of tools) {
      if (tool.scope !== scope) continue
      for (const binding of tool.bindings) {
        if (!matches(binding, press)) continue
        const held = binding.ctrl === true || binding.meta === true
        if (typing && scope !== chain[0] && !held) continue
        if (!tool.enabled(session)) continue
        void tool.run(session)
        return true
      }
    }
  }
  return false
}

export function runTool(session: Session, id: string): void {
  const tool = tools.find((entry) => entry.id === id)
  if (tool && tool.enabled(session)) void tool.run(session)
}
