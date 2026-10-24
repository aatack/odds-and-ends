import { useAtom, useSetAtom } from 'jotai'
import { useEffect, useLayoutEffect, useRef, useState, type MouseEvent, type TouchEvent } from 'react'
import { menuAtom, sameView, viewsAtom, type View } from './views'

/**
 * What to spread on anything standing for an event, so a right-click (desktop)
 * or a long-press (touch) on it opens the event menu. A long-press is ~500ms
 * with the finger still; moving more than 10px or lifting early cancels it, and
 * when it does fire the lift is swallowed so it neither taps nor clicks through.
 */
export function useEventMenu() {
  const setMenu = useSetAtom(menuAtom)
  const press = useRef<{ timer: number; x: number; y: number; fired: boolean } | null>(null)
  const clear = () => {
    if (press.current) clearTimeout(press.current.timer)
    press.current = null
  }
  return (id: string) => ({
    onContextMenu: (e: MouseEvent) => {
      e.preventDefault()
      e.stopPropagation()
      setMenu({ x: e.clientX, y: e.clientY, id })
    },
    onTouchStart: (e: TouchEvent) => {
      const { clientX: x, clientY: y } = e.touches[0]
      clear()
      const timer = window.setTimeout(() => {
        if (press.current) press.current.fired = true
        setMenu({ x, y, id })
      }, 500)
      press.current = { timer, x, y, fired: false }
    },
    onTouchMove: (e: TouchEvent) => {
      const p = press.current
      if (!p) return
      const t = e.touches[0]
      if (Math.abs(t.clientX - p.x) > 10 || Math.abs(t.clientY - p.y) > 10) clear()
    },
    onTouchEnd: (e: TouchEvent) => {
      // The press that opened the menu must not also tap or click the row through.
      if (press.current?.fired) e.preventDefault()
      clear()
    },
  })
}

/** The right-click menu on an event: Open it, or Debug it. Each puts a view on top of the stack. */
export function EventMenu() {
  const [menu, setMenu] = useAtom(menuAtom)
  const [views, setViews] = useAtom(viewsAtom)
  useEffect(() => {
    if (!menu) return
    const close = () => setMenu(null)
    const onKey = (e: KeyboardEvent) => e.key === 'Escape' && close()
    window.addEventListener('mousedown', close)
    window.addEventListener('touchstart', close)
    window.addEventListener('keydown', onKey)
    window.addEventListener('blur', close)
    return () => {
      window.removeEventListener('mousedown', close)
      window.removeEventListener('touchstart', close)
      window.removeEventListener('keydown', onKey)
      window.removeEventListener('blur', close)
    }
  }, [menu, setMenu])
  if (!menu) return null
  const push = (view: View) => {
    if (!sameView(views[views.length - 1], view)) setViews([...views, view])
    setMenu(null)
  }
  return <Popover menu={menu} push={push} />
}

/** The menu itself, clamped so it stays within the viewport (it can be summoned at a screen edge). */
function Popover({ menu, push }: { menu: { x: number; y: number; id: string }; push: (view: View) => void }) {
  const ref = useRef<HTMLDivElement>(null)
  const [pos, setPos] = useState({ left: menu.x, top: menu.y })
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    const r = el.getBoundingClientRect()
    setPos({
      left: Math.max(8, Math.min(menu.x, window.innerWidth - r.width - 8)),
      top: Math.max(8, Math.min(menu.y, window.innerHeight - r.height - 8)),
    })
  }, [menu])
  const stop = (e: MouseEvent | TouchEvent) => e.stopPropagation()
  return (
    <div ref={ref} className="menu" style={pos} onMouseDown={stop} onTouchStart={stop}>
      <button onClick={() => push({ kind: 'event', id: menu.id })}>Open</button>
      <button onClick={() => push({ kind: 'debug', id: menu.id })}>Debug</button>
    </div>
  )
}
