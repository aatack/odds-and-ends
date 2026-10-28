import { CircleCheck, CircleDashed, CircleX } from 'lucide-react'
import type { ItemInfo } from '../../shared/types'

/**
 * A task as it shows where another task mentions it (a child, in its parent's
 * chat or blockers): its title, with a tick once resolved, a cross if it was
 * cancelled, and a pending ring while it is open.
 */
export function TaskCard({ event: e }: { event: ItemInfo }) {
  const [Icon, label, tone] = !e.resolved
    ? [CircleDashed, 'Open', 'pending']
    : e.data?.how === 'cancelled'
      ? [CircleX, 'Cancelled', 'cancelled']
      : [CircleCheck, e.data?.how === 'handled_outside' ? 'Done outside' : 'Resolved', 'resolved']
  return (
    <span className={`task-card ${tone}`}>
      <Icon className="icon" aria-label={label} />
      <span className="task-card-title">{e.summary}</span>
    </span>
  )
}
