import type { Session } from './session.ts'
import type { Tool } from './tools.ts'

/** `Ctrl+Alt+k`. Shift is only named for keys whose name it doesn't change. */
export function keyName(event: KeyboardEvent): string {
  const parts: string[] = []
  if (event.ctrlKey) parts.push('Ctrl')
  if (event.altKey) parts.push('Alt')
  if (event.metaKey) parts.push('Meta')
  if (event.shiftKey && event.key.length > 1) parts.push('Shift')
  parts.push(event.key)
  return parts.join('+')
}

function typing(target: EventTarget | null): boolean {
  return target instanceof HTMLElement && (target.isContentEditable || ['INPUT', 'TEXTAREA'].includes(target.tagName))
}

/**
 * The only key listener. Walks the scopes innermost first and runs the first
 * enabled tool bound to the key. While typing, only the input scope and
 * modified keys get a look in.
 */
export function installDispatch(session: Session, tools: Tool[]): () => void {
  const listener = (event: KeyboardEvent) => {
    if (event.isComposing) return
    const key = keyName(event)
    const inInput = typing(event.target)
    const scopes: Tool['scope'][] = inInput ? ['input', 'list', 'app'] : ['list', 'app']
    const modified = event.ctrlKey || event.altKey || event.metaKey
    for (const scope of scopes) {
      if (inInput && scope !== 'input' && !modified) continue
      const tool = tools.find((t) => t.scope === scope && t.keys.includes(key) && (t.enabled?.(session) ?? true))
      if (tool) {
        event.preventDefault()
        tool.run(session)
        return
      }
    }
  }
  window.addEventListener('keydown', listener)
  return () => window.removeEventListener('keydown', listener)
}
