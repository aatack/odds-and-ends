import { useEffect, useRef } from 'react'
import { matchAction, type KeyBinding } from '../actions/keys'

interface HotkeyAction<C> {
  keys?: KeyBinding[]
  run: (c: C) => void
}

interface HotkeyOptions {
  /**
   * Skip when the keystroke is going to a text field (input/textarea/select or
   * a contenteditable). Set for bare-key registries so navigation doesn't fire
   * while the user is typing in the palette, a rename box, or an in-place edit.
   */
  ignoreEditable?: boolean
}

export function isEditableTarget(target: EventTarget | null): boolean {
  if (!(target instanceof HTMLElement)) return false
  const tag = target.tagName
  return tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || target.isContentEditable
}

// Binds a registry's hotkeys at the window level and dispatches them through the
// given controller. Only actions that declare `keys` do anything here; keyless
// ones are reachable via the palette or a button instead. The controller is held
// in a ref so its latest closures are used without re-binding the listener.
export function useHotkeys<C>(
  actions: HotkeyAction<C>[],
  controller: C,
  options: HotkeyOptions = {},
): void {
  const controllerRef = useRef(controller)
  controllerRef.current = controller
  const { ignoreEditable = false } = options

  useEffect(() => {
    const onKey = (e: KeyboardEvent): void => {
      // While typing, skip only bare/shift shortcuts (they'd collide with input);
      // Ctrl/⌘/Alt combos still fire, so e.g. ctrl+tab works mid-edit.
      if (ignoreEditable && isEditableTarget(e.target) && !(e.ctrlKey || e.metaKey || e.altKey))
        return
      const action = matchAction(actions, e)
      if (!action) return
      e.preventDefault()
      action.run(controllerRef.current)
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [actions, ignoreEditable])
}
