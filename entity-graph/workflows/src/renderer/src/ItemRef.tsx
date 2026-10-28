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
export function ItemRef({ id, item, onOpen }: { id: string; item: Item | undefined; onOpen: (id: string) => void }) {
  const [found, setFound] = useState<string | null>(null)
  useEffect(() => {
    if (!item) void api().itemInfoDetail(id).then((d) => setFound(d?.event.summary ?? null))
  }, [id, item])
  return <Pill id={id} icon={item && <ItemIcon item={item} />} label={item?.title ?? found ?? id} onOpen={() => onOpen(id)} />
}
