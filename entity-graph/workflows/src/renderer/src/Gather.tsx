import { CornerDownRight, Paperclip } from 'lucide-react'
import type { ItemDetail, Overview } from '../../shared/types'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** How long a new item offers to gather. */
const NEW_FOR = 60 * 60_000
const SHOWN = 4

/**
 * On a new item, what the human attached lately, to group it with in a press:
 * the items they gathered things in (attach this item there), and the things
 * they gathered (attach them here). Above the chat input, until the item is an
 * hour old or has been attached either way.
 */
export function Gather({ detail, overview, act }: { detail: ItemDetail; overview: Overview; act: Act }) {
  const { item } = detail
  const recent = overview.attachments
  const linked = detail.events.some((e) => e.type === 'attachment') || recent.some((a) => a.event === item.id)
  if (item.closed || linked || Date.now() - item.createdAt > NEW_FOR) return null
  const open = new Map(overview.items.filter((i) => !i.closed).map((i) => [i.id, i]))
  const targets = [...new Set(recent.map((a) => a.target))].filter((id) => id !== item.id && open.has(id)).slice(0, SHOWN)
  const things = [...new Map(recent.filter((a) => a.event !== item.id && a.target !== item.id).map((a) => [a.event, a])).values()].slice(0, SHOWN)
  if (!targets.length && !things.length) return null
  return (
    <section className="gather">
      {targets.length > 0 && (
        <div className="gather-line">
          <span className="muted">Attach to</span>
          {targets.map((id) => (
            <button key={id} data-ui className="gather-chip" onClick={() => act(() => api().attach(item.id, id))}>
              <CornerDownRight className="icon" />
              {open.get(id)!.title}
            </button>
          ))}
        </div>
      )}
      {things.length > 0 && (
        <div className="gather-line">
          <span className="muted">Gather here</span>
          {things.map((a) => (
            <button key={a.event} data-ui className="gather-chip" onClick={() => act(() => api().attach(a.event, item.id))}>
              <Paperclip className="icon" />
              {a.title}
            </button>
          ))}
        </div>
      )}
    </section>
  )
}
