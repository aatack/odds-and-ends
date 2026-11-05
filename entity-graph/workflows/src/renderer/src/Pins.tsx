import { X } from 'lucide-react'
import type { Catalog, ItemInfo } from '../../shared/types'
import { useItemMenu } from './ItemMenu'
import { ItemInfoIcon } from './icons'
import { ItemCard } from './sizes'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * What is pinned to an item (`pins` links from it: subtasks, pull requests, files, links), and the items it is pinned
 * to, each as its card: beside a task's chat, and in the debug view. A pin can be taken off with its ×; right-click
 * anything to pin it to the item on screen.
 */
export function Pins(props: { id: string; pins: ItemInfo[]; pinnedIn: ItemInfo[]; catalog: Catalog; act: Act; onOpen: (id: string) => void; className?: string }) {
  const { id, pins, pinnedIn } = props
  if (!pins.length && !pinnedIn.length) return <p className="muted">Nothing pinned, and pinned nowhere.</p>
  return (
    <div className={`pins ${props.className ?? ''}`}>
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
