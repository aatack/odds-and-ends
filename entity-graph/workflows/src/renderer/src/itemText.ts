// An item as text (shared/text.ts, as bots read it), worked out here from what the window already has: to copy it
// without waiting for the backend. Only what isn't here yet (an item not opened, a tree not read) is fetched first.

import type { ItemInfo, ItemInfoDetail, Overview } from '../../shared/types'
import { viewText, type Reader, type Size } from '../../shared/text'
import { focusKind } from '../../shared/views'
import { nameOf } from './people'
import { api, cache } from './useWorkflows'

export async function itemText(id: string, size: Size, overview: Overview): Promise<string | null> {
  const details = new Map<string, ItemInfoDetail>()
  const known = (i: string) => cache.get().details[i] ?? details.get(i) ?? null
  const fetch = async (i: string) => {
    const d = known(i) ?? (await api().itemInfoDetail(i))
    if (d) details.set(i, d)
    return d
  }
  const root = await fetch(id)
  if (!root) return null
  // A tree is read whole (the notes view's own read), only when it is what is shown.
  const notes = new Map<string, ItemInfo[]>()
  if (size === 'focus' && focusKind(root.info) === 'notes') {
    const all = await api().subtree(id)
    for (const i of all) if (i.parentId && i.via === 'notes') notes.set(i.parentId, [...(notes.get(i.parentId) ?? []), i])
    for (const i of all) if (!details.has(i.id)) details.set(i.id, { info: i, parent: null, pins: [], pinnedIn: [], item: null, typeScripts: [] })
  }
  const read: Reader = {
    info: (i) => known(i)?.info ?? null,
    children: (i) => known(i)?.item?.children ?? [],
    notes: (i) => notes.get(i) ?? [],
  }
  const who = { name: (a: string) => nameOf(overview, a.replace(/^bot:/, '')), person: (a: string) => overview.users.some((u) => u.id === a) }
  return viewText(read, id, size, { ids: false, who })
}
