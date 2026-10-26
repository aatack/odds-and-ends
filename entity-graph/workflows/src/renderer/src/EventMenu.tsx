import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/react-dom'
import { useAtom, useSetAtom } from 'jotai'
import { useEffect, useState, type MouseEvent } from 'react'
import type { CustomAction, ItemEvent } from '../../shared/types'
import { eventActions } from './eventActions'
import { api } from './useWorkflows'
import { menuAtom, renameAtom, sameView, viewsAtom, type View } from './views'

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

/**
 * The right-click menu on an event: Open it, or Debug it (each puts a view on
 * top of the stack), and below, what can be done to it (`eventActions`), as its
 * view's header offers.
 */
export function EventMenu({
  actOn,
  skip,
  custom,
}: {
  custom: CustomAction[]
  actOn: (id: string) => (fn: () => Promise<unknown>) => Promise<void>
  skip: (id: string) => void
}) {
  const [menu, setMenu] = useAtom(menuAtom)
  const [views, setViews] = useAtom(viewsAtom)
  const setRename = useSetAtom(renameAtom)
  // The event itself, read as the menu opens, for which actions it has.
  const [event, setEvent] = useState<ItemEvent | null>(null)
  // Placed by Floating UI at the cursor: flipped above it, or shifted along, so it always stays in the window,
  // and placed again as it grows (its actions come in a moment after it opens).
  const { refs, floatingStyles } = useFloating({
    strategy: 'fixed',
    placement: 'bottom-start',
    middleware: [offset(2), flip({ padding: 8 }), shift({ padding: 8 })],
    whileElementsMounted: autoUpdate,
  })
  useEffect(() => {
    if (!menu) return
    const { x, y } = menu
    refs.setReference({
      getBoundingClientRect: () => ({ x, y, left: x, top: y, right: x, bottom: y, width: 0, height: 0 }),
    })
  }, [menu, refs])
  useEffect(() => {
    setEvent(null)
    if (menu) void api().eventDetail(menu.id).then((d) => setEvent(d?.event ?? null))
  }, [menu])
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
    <div className="menu" ref={refs.setFloating} style={floatingStyles} onMouseDown={(e) => e.stopPropagation()}>
      <button onClick={() => push({ kind: 'event', id: menu.id })}>Open</button>
      <button onClick={() => push({ kind: 'debug', id: menu.id })}>Debug</button>
      {/* A task on screen is renamed in its header. */}
      {event?.id === menu.id && event.type === 'task' && sameView(views[views.length - 1], { kind: 'event', id: event.id }) && (
        <button
          onClick={() => {
            setRename(event.id)
            setMenu(null)
          }}
        >
          Rename
        </button>
      )}
      {event?.id === menu.id &&
        eventActions(event, { act: actOn(event.id), skip, custom }).map((a) => (
          <button
            key={a.id}
            className="menu-action"
            title={a.title}
            onClick={() => {
              setMenu(null)
              a.run()
            }}
          >
            <a.icon className={`icon ${a.tone}`} />
            {a.label}
          </button>
        ))}
    </div>
  )
}
