import { CircleCheck, CircleDashed } from 'lucide-react'
import type { ItemInfo } from '../../shared/types'
import { Marked } from './components/ui/Marked'
import { isTask } from '../../shared/views'

/**
 * A task as it shows where another mentions it (in its parent's chat): its first line, with a pending ring while it is
 * a task (someone owns or has it: `isTask`) and a tick once it is done. `find` is marked in its title.
 */
export function TaskCard({ info: e, find }: { info: ItemInfo; find?: string }) {
  const [Icon, label, tone] = isTask(e) ? [CircleDashed, 'Open', 'pending'] : [CircleCheck, 'Closed', 'resolved']
  return (
    <span className={`task-card ${tone}`}>
      <Icon className="icon" aria-label={label} />
      <span className="task-card-title"><Marked text={e.summary.split('\n')[0]} phrase={find} /></span>
    </span>
  )
}
