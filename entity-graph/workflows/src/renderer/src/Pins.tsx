import { NotebookPen, X } from 'lucide-react'
import { useState } from 'react'
import type { Catalog, ItemInfo } from '../../shared/types'
import { useItemMenu } from './ItemMenu'
import { ItemInfoIcon } from './icons'
import { ItemCard } from './sizes'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * What is pinned to an item (`pins` links from it: subtasks, pull requests, files, links), and the items it is pinned
 * to, each as its card: beside a task's chat, and in the debug view. A pin can be taken off with its ×; right-click
 * anything to pin it to the item on screen. New notes, atop, makes a set of notes pinned here, and opens it.
 */
export function Pins(props: { id: string; pins: ItemInfo[]; pinnedIn: ItemInfo[]; catalog: Catalog; act: Act; onOpen: (id: string) => void; className?: string }) {
  const { id, pins, pinnedIn } = props
  return (
    <div className={`pins ${props.className ?? ''}`}>
      <NewNotes pinTo={id} act={props.act} onOpen={props.onOpen} />
      {!pins.length && !pinnedIn.length && <p className="muted">Nothing pinned, and pinned nowhere.</p>}
      <PinList title="Pinned" items={pins} {...props} unpin={(other) => void props.act(() => api().pin(id, [other], false))} />
      <PinList title="Pinned in" items={pinnedIn} {...props} unpin={(other) => void props.act(() => api().pin(other, [id], false))} />
    </div>
  )
}

function PinList({ title, items, catalog, act, onOpen, unpin }: { title: string; items: ItemInfo[]; catalog: Catalog; act: Act; onOpen: (id: string) => void; unpin: (id: string) => void }) {
  const menu = useItemMenu()
  if (!items.length) return null
  return (
    <section className="pin-list">
      <h3 className="muted">{title}</h3>
      {items.map((e) => (
        <div key={e.id} className="pin-card" {...menu(e.id)}>
          <ItemInfoIcon info={e} />
          <div className="pin-body">
            <ItemCard info={e} find="" catalog={catalog} act={act} onOpen={onOpen} />
          </div>
          <button data-ui className="pin-off" title="Unpin" aria-label="Unpin" onClick={() => unpin(e.id)}>
            <X className="icon" />
          </button>
        </div>
      ))}
    </section>
  )
}

/** New notes: its title typed in place, then Enter makes the note (pinned to `pinTo`) and opens its tree. */
function NewNotes({ pinTo, act, onOpen }: { pinTo: string; act: Act; onOpen: (id: string) => void }) {
  const [title, setTitle] = useState<string | null>(null)
  if (title === null)
    return (
      <button data-ui className="new-notes" onClick={() => setTitle('')}>
        <NotebookPen className="icon" />
        New notes
      </button>
    )
  return (
    <input
      className="new-notes-title"
      autoFocus
      value={title}
      placeholder="Title, then Enter"
      aria-label="Title of the new notes"
      onChange={(e) => setTitle(e.target.value)}
      onBlur={() => !title.trim() && setTitle(null)}
      onKeyDown={(e) => {
        if (e.key === 'Escape') {
          e.preventDefault()
          e.stopPropagation()
          setTitle(null)
        } else if (e.key === 'Enter' && title.trim()) {
          e.preventDefault()
          const text = title.trim()
          setTitle(null)
          void act(async () => onOpen(await api().newNotes(text, pinTo)))
        }
      }}
    />
  )
}
