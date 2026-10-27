import { Bot, SquareCode, User } from 'lucide-react'
import { useState } from 'react'
import type { Item, Overview } from '../../shared/types'
import { ItemPicker } from './ItemPicker'
import { everyone, nameOf } from './people'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * Who has the task, in the top right of its view: a pill with their name, or
 * with more than one, their pictures. Clicking it opens the finder as a
 * multiselect: who has it shows first, ticked (pick them to remove them), and
 * picking anyone else adds them.
 */
export function AssigneePill({ item, overview, act }: { item: Item; overview: Overview; act: Act }) {
  const [picking, setPicking] = useState(false)
  const isBot = (id: string) => overview.bots.some((b) => b.id === id)
  const name = (id: string) => {
    const n = nameOf(overview, id)
    return n === 'you' ? 'You' : n
  }
  const isScript = (id: string) => overview.scripts.some((s) => s.id === id)
  const icon = (id: string) => (isBot(id) ? <Bot className="icon" /> : isScript(id) ? <SquareCode className="icon" /> : <User className="icon" />)
  const people = everyone(overview)
  return (
    <>
      <button
        data-ui
        className="assignee-pill"
        title={item.assignees.length ? `With ${item.assignees.map(name).join(', ')}` : 'With nobody: pick who has it'}
        onClick={() => setPicking(true)}
      >
        {item.assignees.length === 0 && <span className="muted">Nobody</span>}
        {item.assignees.length === 1 && (
          <>
            {icon(item.assignees[0])}
            <span>{name(item.assignees[0])}</span>
          </>
        )}
        {item.assignees.length > 1 &&
          item.assignees.map((id) => (
            <span key={id} className={`assignee-avatar ${item.bot === id ? 'working' : ''}`} title={name(id)}>
              {icon(id)}
            </span>
          ))}
      </button>
      {picking && (
        <ItemPicker
          title="Assignees"
          items={[]}
          recent={{ viewed: [], users: overview.users }}
          sections={[]}
          verb="Assign"
          bots={people.map((p) => ({ ...p, name: p.id === overview.me.id ? `${p.name} (you)` : p.name }))}
          multiple
          value={item.assignees}
          setValue={(next) =>
            void act(async () => {
              for (const id of next.filter((id) => !item.assignees.includes(id))) await api().assign(item.id, id, true)
              for (const id of item.assignees.filter((id) => !next.includes(id))) await api().assign(item.id, id, false)
            })
          }
          onClose={() => setPicking(false)}
        />
      )}
    </>
  )
}
