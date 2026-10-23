import type { ItemDetail, ItemEvent } from '../../shared/types'
import { useEventMenu } from './EventMenu'
import { Avatar } from './icons'

export const time = (ms: number) => new Date(ms).toLocaleString()
const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
/** A duration to the second: 45s, 3m 07s, 2h 05m. */
export const minutes = (ms: number) => {
  const s = Math.floor(ms / 1000)
  const pad = (n: number) => String(n).padStart(2, '0')
  if (s < 60) return `${s}s`
  if (s < 3600) return `${Math.floor(s / 60)}m ${pad(s % 60)}s`
  return `${Math.floor(s / 3600)}h ${pad(Math.floor(s / 60) % 60)}m`
}

/** The human, or an outside client acting as them: their events are on the right. */
const HUMAN = new Set(['human', 'mcp'])

/**
 * The item's history as a chat: every event that has resolved, at the moment it
 * did. What the human said or did is on the right; everything else on the left,
 * with who it came from as its picture. Cancelled blockers are small notes in
 * the middle. A message shows its words and the time; pressing it shows the rest.
 */
export function Chat({ detail }: { detail: ItemDetail }) {
  const menu = useEventMenu()
  const done = detail.events.filter((e) => e.resolved && e.message !== null).sort((a, b) => a.resolvedAt! - b.resolvedAt!)
  return (
    <section className="chat">
      {done.map((e) =>
        e.cancelled ? (
          <div key={e.id} className="chat-note" title={time(e.resolvedAt!)} {...menu(e.id)}>
            {e.message}
          </div>
        ) : (
          <Message key={e.id} event={e} />
        ),
      )}
    </section>
  )
}

/** One resolved event, as a message: right-click it to open it or debug it. */
function Message({ event: e }: { event: ItemEvent }) {
  const menu = useEventMenu()
  const mine = HUMAN.has(e.actor)
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <Avatar by={e.actor} type={e.type} />}
      <div className="bubble" title={time(e.resolvedAt ?? e.createdAt)} {...menu(e.id)}>
        {e.message}
        <span className="bubble-time">{shortTime(e.resolvedAt ?? e.createdAt)}</span>
      </div>
    </div>
  )
}
