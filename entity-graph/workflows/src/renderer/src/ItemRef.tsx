import { useEffect, useState } from 'react'
import type { Item } from '../../shared/types'
import { useEventMenu } from './EventMenu'
import { ItemIcon } from './icons'
import { api } from './useWorkflows'

/**
 * A reference to an item in text (`[itemId]()`, as `@` inserts): its icon and
 * title, which open it. An item the overview doesn't have (closed a while ago)
 * is looked up for its title.
 */
export function ItemRef({ id, item, onOpen }: { id: string; item: Item | undefined; onOpen: (id: string) => void }) {
  const menu = useEventMenu()
  const [found, setFound] = useState<string | null>(null)
  useEffect(() => {
    if (!item) void api().eventDetail(id).then((d) => setFound(d?.event.summary ?? null))
  }, [id, item])
  return (
    <button data-ui className="link item-ref" onClick={() => onOpen(id)} {...menu(id)}>
      {item && <ItemIcon item={item} />}
      {item?.title ?? found ?? id}
    </button>
  )
}
