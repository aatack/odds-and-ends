import { Flag } from 'lucide-react'
import type { Item } from '../../shared/types'
import { minutes } from './Chat'
import { useEventMenu } from './EventMenu'
import { ItemIcon } from './icons'

/** Items as the finder's rows, for the pages that list a few: each opens, and has its right-click menu. */
export function ItemRows({ items, onOpen, empty, note }: { items: Item[]; onOpen: (id: string) => void; empty: string; note?: (i: Item) => string }) {
  const menu = useEventMenu()
  const now = Date.now()
  if (!items.length) return <p className="muted">{empty}</p>
  return (
    <div className="list">
      {items.map((i) => (
        <div key={i.id} className="row" onClick={() => onOpen(i.id)} {...menu(i.id)}>
          <ItemIcon item={i} />
          <span className="row-title">{i.title}</span>
          {i.priority && !i.closed && <Flag className="icon priority-flag" aria-label="Priority" />}
          {note && <span className="muted row-status">{note(i)}</span>}
          <span className="muted row-age" title="Since anything last happened in it">
            {minutes(Math.max(0, now - i.since))}
          </span>
        </div>
      ))}
    </div>
  )
}
