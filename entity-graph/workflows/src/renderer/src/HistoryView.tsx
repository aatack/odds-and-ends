import { useEffect, useMemo, useState } from 'react'
import type { Item } from '../../shared/types'
import { ItemsList } from './ItemsList'
import type { Recent } from './sections'
import { api } from './useWorkflows'
import { HISTORY_LIMIT, type View } from './views'

/**
 * The History view: what Back goes through (the stack below the view on screen, never what Forward would bring back),
 * the nearest first, each item once, in the finder (`ItemsList`) with its one section, History. Picking a row goes
 * back to it, as pressing Back that many times would (what is passed goes ahead, for Forward). Only views of an item
 * show: a page (Bots, Types) has no row. It takes no item.
 */
export function HistoryView({
  views,
  recent,
  version,
  onBackTo,
  onEscape,
  perSection = HISTORY_LIMIT,
  mobile = false,
}: {
  views: View[]
  recent: Recent
  /** Anything that changes when the store does: the rows are read again (a title changed). */
  version: unknown
  /** Back to the view at this index on the stack. */
  onBackTo: (index: number) => void
  onEscape?: () => void
  perSection?: number
  mobile?: boolean
}) {
  // The nearest view of each item below the top, by id: where picking it goes back to.
  const at = useMemo(() => {
    const at = new Map<string, number>()
    for (let i = views.length - 2; i >= 0; i--) {
      const v = views[i]
      if ('id' in v && v.id && !at.has(v.id)) at.set(v.id, i)
    }
    return at
  }, [views])
  const ids = [...at.keys()]
  const key = ids.join(',')
  const [history, setHistory] = useState<Item[]>([])
  useEffect(() => {
    let live = true
    void api()
      .items(key ? key.split(',') : [])
      .then((got) => live && setHistory(got), () => live && setHistory([]))
    return () => {
      live = false
    }
  }, [key, version])
  const withHistory = useMemo(() => ({ ...recent, history }), [recent, history])
  return (
    <ItemsList
      items={[]}
      recent={withHistory}
      sections={['history']}
      perSection={perSection}
      verb="Go back"
      onOpen={(id) => {
        const i = at.get(id)
        if (i !== undefined) onBackTo(i)
      }}
      onEscape={onEscape}
      mobile={mobile}
    />
  )
}
