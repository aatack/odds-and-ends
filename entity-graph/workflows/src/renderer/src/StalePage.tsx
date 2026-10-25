import { useCallback, useEffect, useState } from 'react'
import type { Item } from '../../shared/types'
import { ItemRows } from './ItemRows'
import { api } from './useWorkflows'

const HOURS = [4, 24, 72, 168]
const label = (h: number) => (h < 24 ? `${h} hours` : h === 24 ? 'a day' : `${h / 24} days`)

/**
 * What you may have lost track of: the open tasks you made, or had at any time,
 * in which nothing has happened for a while, the longest untouched first.
 */
export function StalePage({ onOpen }: { onOpen: (id: string) => void }) {
  const [hours, setHours] = useState(24)
  const [items, setItems] = useState<Item[] | null>(null)
  const refresh = useCallback(async () => setItems(await api().stale(hours)), [hours])
  useEffect(() => {
    void refresh()
    return api().onChange(() => void refresh())
  }, [refresh])
  return (
    <div className="page stale-page">
      <div className="choices">
        <span className="muted">Untouched for</span>
        {HOURS.map((h) => (
          <button key={h} data-ui className={hours === h ? 'chosen' : ''} onClick={() => setHours(h)}>
            {label(h)}
          </button>
        ))}
      </div>
      <section className="card">
        {items && <ItemRows items={items} onOpen={onOpen} empty="Nothing: everything you have has moved lately." note={(i) => (i.withHuman ? 'yours' : '')} />}
      </section>
    </div>
  )
}
