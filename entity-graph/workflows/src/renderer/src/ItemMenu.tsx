import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/react-dom'
import { useAtom, useAtomValue, useSetAtom } from 'jotai'
import { useEffect, useState, type MouseEvent } from 'react'
import type { CustomAction, Item, ItemInfo } from '../../shared/types'
import { ItemPicker } from './ItemPicker'
import type { Recent } from './sections'
import { itemActions } from './itemActions'
import { userAtom } from './people'
import { api } from './useWorkflows'
import { menuAtom, renameAtom, sameView, viewsAtom, type View } from './views'

/** What to spread on anything standing for an item, so a right-click on it opens the item menu. */
export function useItemMenu() {
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
 * The right-click menu on an item: Open it, or Debug it (each puts a view on
 * top of the stack), and below, what can be done to it (`itemActions`), as its
 * view's header offers.
 */
export function ItemMenu({
  actOn,
  skip,
  custom,
  tasks,
  recent,
}: {
  custom: CustomAction[]
  /** What "Also put in" picks from. */
  tasks: Item[]
  recent: Recent
  actOn: (id: string) => (fn: () => Promise<unknown>) => Promise<void>
  skip: (id: string) => void
}) {
  const [menu, setMenu] = useAtom(menuAtom)
  const [views, setViews] = useAtom(viewsAtom)
  const setRename = useSetAtom(renameAtom)
  const me = useAtomValue(userAtom)
  // The item itself, read as the menu opens, for which actions it has.
  const [info, setInfo] = useState<ItemInfo | null>(null)
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
    setInfo(null)
    if (menu) void api().itemInfoDetail(menu.id).then((d) => setInfo(d?.info ?? null))
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
  // "Also put in…": the item a task is being picked for, once the menu has closed.
  const [adding, setAdding] = useState<string | null>(null)
  const picker = adding && (
    <ItemPicker
      title="Also put it in"
      items={tasks.filter((t) => t.id !== adding && !t.closed)}
      recent={recent}
      verb="Put it there"
      onPick={(to) => {
        const id = adding
        setAdding(null)
        void actOn(to)(() => api().link(to, [id]))
      }}
      onClose={() => setAdding(null)}
    />
  )
  if (!menu) return picker || null
  const push = (view: View) => {
    if (!sameView(views[views.length - 1], view)) setViews([...views, view])
    setMenu(null)
  }
  return (
    <div className="menu" ref={refs.setFloating} style={floatingStyles} onMouseDown={(e) => e.stopPropagation()}>
      <button onClick={() => push({ kind: 'event', id: menu.id })}>Open</button>
      <button onClick={() => push({ kind: 'debug', id: menu.id })}>Debug</button>
      <button onClick={() => push({ kind: 'log', id: menu.id })}>Edit log</button>
      <button
        title="Put it in another task as well: it stays where it is, and shows there too"
        onClick={() => {
          setAdding(menu.id)
          setMenu(null)
        }}
      >
        Also put in…
      </button>
      {/* A task on screen is renamed in its header. */}
      {info?.id === menu.id && info.type === 'task' && sameView(views[views.length - 1], { kind: 'event', id: info.id }) && (
        <button
          onClick={() => {
            setRename(info.id)
            setMenu(null)
          }}
        >
          Rename
        </button>
      )}
      {/* A message (anything but a task, a bot or a person) can be given a thumbs-up. */}
      {info?.id === menu.id && me && !['task', 'bot', 'user'].includes(info.type) && (
        <button
          onClick={() => {
            setMenu(null)
            void actOn(info.id)(() => api().react(info.id, !info.reactions.includes(me)))
          }}
        >
          {info.reactions.includes(me) ? 'Remove thumbs up' : 'Thumbs up'}
        </button>
      )}
      {info?.id === menu.id &&
        itemActions(info, { act: actOn(info.id), skip, custom }).map((a) => (
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
