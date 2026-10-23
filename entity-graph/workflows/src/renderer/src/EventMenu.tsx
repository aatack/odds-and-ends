import { useAtom, useSetAtom } from 'jotai'
import { useEffect, type MouseEvent } from 'react'
import { menuAtom, sameView, viewsAtom, type View } from './views'

/** What to spread on anything standing for an event, so a right-click on it opens the event menu. */
export function useEventMenu() {
  const setMenu = useSetAtom(menuAtom)
  return (id: string) => ({
    onContextMenu: (e: MouseEvent) => {
      e.preventDefault()
      e.stopPropagation()
      setMenu({ x: e.clientX, y: e.clientY, id })
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
    window.addEventListener('keydown', onKey)
    window.addEventListener('blur', close)
    return () => {
      window.removeEventListener('mousedown', close)
      window.removeEventListener('keydown', onKey)
      window.removeEventListener('blur', close)
    }
  }, [menu, setMenu])
  if (!menu) return null
  const push = (view: View) => {
    if (!sameView(views[views.length - 1], view)) setViews([...views, view])
    setMenu(null)
  }
  return (
    <div className="menu" style={{ left: menu.x, top: menu.y }} onMouseDown={(e) => e.stopPropagation()}>
      <button onClick={() => push({ kind: 'event', id: menu.id })}>Open</button>
      <button onClick={() => push({ kind: 'debug', id: menu.id })}>Debug</button>
    </div>
  )
}
