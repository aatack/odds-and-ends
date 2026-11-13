import { Plus, X } from 'lucide-react'
import { useState } from 'react'
import type { Item, ItemInfo } from '../../shared/types'
import { pillLabel } from '../../shared/views'
import { ItemInfoIcon } from './icons'
import { ItemPicker } from './ItemPicker'
import type { Recent } from './sections'
import { Pill } from './sizes'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * What is pinned to an item (`pins` links from it: subtasks, pull requests, files, links), and the items it is pinned
 * to, each a list of pills: in the right pane, and in the debug view on a phone. A pin opens when pressed; its × takes
 * it off. Each list's + opens the picker: pick anything to pin it, or type a title and Tab to make a set of notes there.
 */
export function Pins(props: { id: string; pins: ItemInfo[]; pinnedIn: ItemInfo[]; items: Item[]; recent: Recent; act: Act; onOpen: (id: string) => void }) {
  const { id, pins, pinnedIn, act } = props
  return (
    <>
      <PinList
        title="Pinned"
        {...props}
        shown={pins}
        pin={(other) => void act(() => api().pin(id, [other], true))}
        unpin={(other) => void act(() => api().pin(id, [other], false))}
        make={(title) => void act(() => api().newNotes(title, id))}
      />
      <PinList
        title="Pinned in"
        {...props}
        shown={pinnedIn}
        pin={(other) => void act(() => api().pin(other, [id], true))}
        unpin={(other) => void act(() => api().pin(other, [id], false))}
        make={(title) =>
          void act(async () => {
            const made = await api().newNotes(title, null)
            await api().pin(made, [id], true)
          })
        }
      />
    </>
  )
}

function PinList(props: {
  title: string
  id: string
  shown: ItemInfo[]
  items: Item[]
  recent: Recent
  onOpen: (id: string) => void
  pin: (id: string) => void
  unpin: (id: string) => void
  make: (title: string) => void
}) {
  const { title, id, shown, onOpen } = props
  const [picking, setPicking] = useState(false)
  return (
    <section className="side-section">
      <h3 className="side-heading">
        {title}
        <button data-ui className="side-add" title={`${title}: add`} aria-label={`${title}: add`} onClick={() => setPicking(true)}>
          <Plus className="icon" />
        </button>
      </h3>
      {shown.length === 0 ? (
        <p className="muted side-none">Nothing.</p>
      ) : (
        <div className="side-pills">
          {shown.map((e) => (
            <span key={e.id} className="side-pill">
              <Pill id={e.id} icon={<ItemInfoIcon info={e} />} label={pillLabel(e)} onOpen={() => onOpen(e.id)} />
              <button data-ui className="pin-off" title="Unpin" aria-label="Unpin" onClick={() => props.unpin(e.id)}>
                <X className="icon" />
              </button>
            </span>
          ))}
        </div>
      )}
      {picking && (
        <ItemPicker
          title={title}
          verb="Pin"
          items={props.items.filter((i) => i.id !== id && !shown.some((s) => s.id === i.id))}
          recent={props.recent}
          onPick={(other) => {
            setPicking(false)
            props.pin(other)
          }}
          onTab={(text) => {
            setPicking(false)
            if (text.trim()) props.make(text.trim())
          }}
          onClose={() => setPicking(false)}
        />
      )}
    </section>
  )
}
