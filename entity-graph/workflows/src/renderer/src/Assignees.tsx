import { Bot, Plus, User, X } from 'lucide-react'
import type { Item, Overview } from '../../shared/types'
import { Dropdown, DropdownItem } from './components/ui/Dropdown'
import { everyone, nameOf } from './people'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * Who has the task, above its blockers: people and bots, each to take off (×),
 * and + to hand it to someone else too. A bot that is working on it now says so.
 */
export function Assignees({ item, overview, act }: { item: Item; overview: Overview; act: Act }) {
  const name = (id: string) => {
    const n = nameOf(overview, id)
    return n === 'you' ? 'You' : n
  }
  const others = everyone(overview).filter((s) => !item.assignees.includes(s.id))
  return (
    <div className="assignees">
      <span>With</span>
      {!item.assignees.length && <span className="assignee">nobody</span>}
      {item.assignees.map((id) => (
        <span key={id} className={`assignee ${item.bot === id ? 'working' : ''}`} title={item.bot === id ? 'Working on it now' : undefined}>
          {overview.bots.some((b) => b.id === id) ? <Bot className="icon" /> : <User className="icon" />}
          {name(id)}
          {item.bot === id && ' · working'}
          <button title={`Take it off ${name(id)}`} aria-label="Unassign" onClick={() => void act(() => api().assign(item.id, id, false))}>
            <X className="icon" />
          </button>
        </span>
      ))}
      {others.length > 0 && (
        <Dropdown
          menuClassName="popover"
          trigger={({ toggle }) => (
            <button className="add-assignee" title="Hand it to someone too" aria-label="Assign" onClick={toggle}>
              <Plus className="icon" />
            </button>
          )}
        >
          {(close) =>
            others.map((s) => (
              <DropdownItem
                key={s.id}
                onClick={() => {
                  close()
                  void act(() => api().assign(item.id, s.id, true))
                }}
              >
                {s.id === overview.me.id ? 'You' : s.bot ? `@${s.name}` : s.name}
              </DropdownItem>
            ))
          }
        </Dropdown>
      )}
    </div>
  )
}
