import type { EventDetail } from '../../shared/types'
import { TypeIcon } from './icons'

/** An event's whole state, as JSON: the event, the item it sits in, its runs and its trace (for an item, the item in full). */
export function DebugView({ detail }: { detail: EventDetail }) {
  return (
    <div className="page debug-view">
      <header>
        <h1>
          <TypeIcon type={detail.event.type} /> Debug: {detail.event.summary}
        </h1>
      </header>
      <pre className="output mono debug-json">{JSON.stringify(detail, null, 2)}</pre>
    </div>
  )
}
