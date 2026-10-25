import { Bot, Plus, User, X } from 'lucide-react'
import { HUMAN, type Agent, type Item } from '../../shared/types'
import { Dropdown, DropdownItem } from './components/ui/Dropdown'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * Who has the task, above its blockers: you and bots, each to take off (×), and
 * + to hand it to someone else too. A bot that is working on it now says so.
 * Nobody at all means it is yours.
 */
export function Assignees({ item, bots, act }: { item: Item; bots: Agent[]; act: Act }) {
  const name = (id: string) => (id === HUMAN ? 'You' : `@${bots.find((b) => b.id === id)?.name ?? 'a deleted bot'}`)
  const others = [HUMAN, ...bots.map((b) => b.id)].filter((id) => !item.assignees.includes(id))
  return (
    <div className="assignees">
      <span>With</span>
      {!item.assignees.length && <span className="assignee">You, as nobody else has it</span>}
      {item.assignees.map((id) => (
        <span key={id} className={`assignee ${item.bot === id ? 'working' : ''}`} title={item.bot === id ? 'Working on it now' : undefined}>
          {id === HUMAN ? <User className="icon" /> : <Bot className="icon" />}
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
            others.map((id) => (
              <DropdownItem
                key={id}
                onClick={() => {
                  close()
                  void act(() => api().assign(item.id, id, true))
                }}
              >
                {name(id)}
              </DropdownItem>
            ))
          }
        </Dropdown>
      )}
    </div>
  )
}
