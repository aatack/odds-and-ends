import { useEffect, useRef } from 'react'
import { matchAction, type KeyBinding } from '../actions/keys'

interface HotkeyAction<C> {
  keys?: KeyBinding[]
  run: (c: C) => void
}

// Binds a registry's hotkeys at the window level and dispatches them through the
// given controller. Only actions that declare `keys` do anything here; keyless
// ones are reachable via the palette or a button instead. The controller is held
// in a ref so its latest closures are used without re-binding the listener.
export function useHotkeys<C>(actions: HotkeyAction<C>[], controller: C): void {
  const controllerRef = useRef(controller)
  controllerRef.current = controller

  useEffect(() => {
    const onKey = (e: KeyboardEvent): void => {
      const action = matchAction(actions, e)
      if (!action) return
      e.preventDefault()
      action.run(controllerRef.current)
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [actions])
}
