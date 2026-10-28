import { Bot, SquareCode, User } from 'lucide-react'
import { useState, type ReactNode } from 'react'
import type { Item, Overview } from '../../shared/types'
import { ItemIcon } from './icons'
import { ItemPicker } from './ItemPicker'
import { everyone, nameOf } from './people'
import type { Recent } from './sections'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * A multiselect as a pill: with nothing chosen, `none`; with one, its icon and name; with more, their pictures.
 * Clicking it opens the finder as a multiselect over `people` (bots and people) and `items`: what is chosen shows
 * first, ticked (pick it again to take it off), and picking anything else adds it. Who has a task (top right of its
 * view) is this, and so are the edit log's who and what.
 */
export function PickPill(props: {
  title: string
  none: string
  value: string[]
  setValue: (ids: string[]) => void
  overview: Overview
  /** Bots and people to offer. */
  people?: boolean
  /** Items to offer, and what the finder reads as recent. */
  items?: Item[]
  recent?: Recent
  verb?: string
  /** Picture a chosen one as working (a bot on it now). */
  working?: string | null
}) {
  const { title, none, value, setValue, overview, people = false, items = [], recent, verb = 'Choose', working = null } = props
  const [picking, setPicking] = useState(false)
  const item = (id: string) => items.find((i) => i.id === id)
  const name = (id: string) => {
    if (item(id)) return item(id)!.title
    const n = nameOf(overview, id)
    return n === 'you' ? 'You' : n
  }
  const icon = (id: string): ReactNode =>
    item(id) ? <ItemIcon item={item(id)!} /> : overview.bots.some((b) => b.id === id) ? <Bot className="icon" /> : overview.scripts.some((s) => s.id === id) ? <SquareCode className="icon" /> : <User className="icon" />
  return (
    <>
      <button data-ui className="assignee-pill" title={value.length ? `${title}: ${value.map(name).join(', ')}` : `${title}: ${none}`} onClick={() => setPicking(true)}>
        {value.length === 0 && <span className="muted">{none}</span>}
        {value.length === 1 && (
          <>
            {icon(value[0])}
            <span className="pill-label">{name(value[0])}</span>
          </>
        )}
        {value.length > 1 &&
          value.map((id) => (
            <span key={id} className={`assignee-avatar ${working === id ? 'working' : ''}`} title={name(id)}>
              {icon(id)}
            </span>
          ))}
      </button>
      {picking && (
        <ItemPicker
          title={title}
          items={items}
          recent={recent ?? { viewed: [], users: overview.users, bots: overview.bots }}
          sections={items.length ? undefined : []}
          verb={verb}
          bots={people ? everyone(overview).map((p) => ({ ...p, name: p.id === overview.me.id ? `${p.name} (you)` : p.name })) : []}
          multiple
          value={value}
          setValue={setValue}
          onClose={() => setPicking(false)}
        />
      )}
    </>
  )
}

/** Who has the task, in the top right of its view (`PickPill` over bots and people). */
export function AssigneePill({ item, overview, act }: { item: Item; overview: Overview; act: Act }) {
  return (
    <PickPill
      title="Assignees"
      none="Nobody"
      verb="Assign"
      people
      overview={overview}
      value={item.assignees}
      working={item.bot}
      setValue={(next) =>
        void act(async () => {
          for (const id of next.filter((id) => !item.assignees.includes(id))) await api().assign(item.id, id, true)
          for (const id of item.assignees.filter((id) => !next.includes(id))) await api().assign(item.id, id, false)
        })
      }
    />
  )
}
