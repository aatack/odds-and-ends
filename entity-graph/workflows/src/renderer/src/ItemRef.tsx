import { useEffect, useState } from 'react'
import type { Item } from '../../shared/types'
import { ItemIcon } from './icons'
import { Pill } from './sizes'
import { api } from './useWorkflows'

/**
 * A reference to an item in text (`[itemId]()`, as `@` inserts): its pill (`Pill`), its icon and
 * title, which open it. An item the overview doesn't have (closed a while ago)
 * is looked up for its title.
 */
export function ItemRef({ id, item, label, onOpen }: { id: string; item: Item | undefined; label?: string; onOpen: (id: string) => void }) {
  // undefined: not looked up yet; null: no such item.
  const [found, setFound] = useState<string | null | undefined>(undefined)
  useEffect(() => {
    if (!item) void api().itemInfoDetail(id).then((d) => setFound(d?.info.summary ?? null))
  }, [id, item])
  // A URL that is no item (one in text no note brought in) is a link, as it was.
  if (!item && found === null && /^https?:/.test(id))
    return (
      <a href={id} target="_blank" rel="noreferrer">
        {label || id}
      </a>
    )
  return <Pill id={id} icon={item && <ItemIcon item={item} />} label={label || (item?.title ?? found ?? id)} onOpen={() => onOpen(id)} />
}
