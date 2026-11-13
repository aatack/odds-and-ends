import { CircleCheck, CircleDashed } from 'lucide-react'
import type { ItemInfo } from '../../shared/types'
import { Marked } from './components/ui/Marked'

/**
 * A topic with a status as it shows where another mentions it (in its parent's chat): its first line, with a tick once
 * closed and a pending ring while it is open. `find` is marked in its title.
 */
export function TaskCard({ info: e, find }: { info: ItemInfo; find?: string }) {
  const [Icon, label, tone] = e.params.open !== false ? [CircleDashed, 'Open', 'pending'] : [CircleCheck, 'Closed', 'resolved']
  return (
    <span className={`task-card ${tone}`}>
      <Icon className="icon" aria-label={label} />
      <span className="task-card-title"><Marked text={e.summary.split('\n')[0]} phrase={find} /></span>
    </span>
  )
}
