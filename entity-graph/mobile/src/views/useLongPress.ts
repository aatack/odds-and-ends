import type React from 'react'
import { useRef } from 'react'

// Long-press, which is what a right-click became.
//
// Written on pointer events rather than `onClick` because the two gestures have to
// be told apart from one stream: a press that lasts long enough is a long-press and
// the tap that would have followed it must not fire, and a press that moves is a
// scroll and neither should. `onClick` alone can't see any of that.

const HOLD_MS = 450
/** Movement past this many pixels is a scroll, not a press. */
const SLOP = 10

export interface PressHandlers {
  onPointerDown: (e: React.PointerEvent) => void
  onPointerMove: (e: React.PointerEvent) => void
  onPointerUp: (e: React.PointerEvent) => void
  onPointerCancel: () => void
  onContextMenu: (e: React.SyntheticEvent) => void
}

export function useLongPress(onTap: () => void, onLongPress: () => void): PressHandlers {
  const timer = useRef(0)
  const origin = useRef<{ x: number; y: number } | null>(null)
  const held = useRef(false)
  const moved = useRef(false)

  const cancel = (): void => {
    window.clearTimeout(timer.current)
    origin.current = null
  }

  return {
    onPointerDown: (e) => {
      held.current = false
      moved.current = false
      origin.current = { x: e.clientX, y: e.clientY }
      timer.current = window.setTimeout(() => {
        held.current = true
        // A press that has become a long-press should say so before the finger
        // lifts; on Android that means a tick of haptics.
        navigator.vibrate?.(10)
        onLongPress()
      }, HOLD_MS)
    },
    onPointerMove: (e) => {
      const from = origin.current
      if (!from) return
      if (Math.abs(e.clientX - from.x) > SLOP || Math.abs(e.clientY - from.y) > SLOP) {
        moved.current = true
        cancel()
      }
    },
    onPointerUp: () => {
      cancel()
      if (!held.current && !moved.current) onTap()
    },
    onPointerCancel: cancel,
    // Android's own long-press menu (select / copy) would otherwise land on top of
    // ours, and the text of a row isn't selectable here anyway.
    onContextMenu: (e) => e.preventDefault(),
  }
}
