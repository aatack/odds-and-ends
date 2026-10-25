import type { ItemDetail, ItemEvent } from '../../shared/types'
import { useEventMenu } from './EventMenu'
import { Avatar } from './icons'
import { useAtomValue } from 'jotai'
import { Markdown, shows } from './components/ui/Markdown'
import { findAtom } from './views'
import { TaskCard } from './TaskCard'

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

/** Who has the task, and who watches it: small notes in the middle, like a cancel. */
const NOTES = new Set(['assign', 'watch'])

/** The human, or an outside client acting as them: their events are on the right. */
const HUMAN = new Set(['human', 'mcp'])

/**
 * The item's history as a chat: every event that has resolved, at the moment it
 * did. What the human said or did is on the right; everything else on the left,
 * with who it came from as its picture. Cancelled blockers are small notes in
 * the middle. A message shows its words and the time; pressing it shows the rest.
 * A child task, once it has closed, is a bubble like any other, holding its card
 * (while open it is among the blockers instead).
 * While Ctrl+F's find line has a phrase in it, only what shows the phrase is
 * left, with the phrase marked.
 */
export function Chat({ detail }: { detail: ItemDetail }) {
  const menu = useEventMenu()
  const find = useAtomValue(findAtom)?.trim() ?? ''
  const done = detail.events
    .filter((e) => e.resolved && e.message !== null && (!find || shows(e.type === 'item' ? e.summary : e.message, find)))
    .sort((a, b) => a.resolvedAt! - b.resolvedAt!)
  return (
    <section className="chat">
      {find && !done.length && <div className="chat-note">Nothing here says “{find}”.</div>}
      {done.map((e) =>
        (e.cancelled && e.type !== 'item') || NOTES.has(e.type) ? (
          <div key={e.id} className="chat-note" title={time(e.resolvedAt!)} {...menu(e.id)}>
            {e.message}
            {NOTES.has(e.type) && e.createdBy !== 'human' && ` (${e.createdBy.startsWith('bot:') ? 'by the bot' : `by ${e.createdBy}`})`}
          </div>
        ) : (
          <Message key={e.id} event={e} find={find} />
        ),
      )}
    </section>
  )
}

/** One resolved event, as a message: right-click it to open it or debug it. */
function Message({ event: e, find }: { event: ItemEvent; find: string }) {
  const menu = useEventMenu()
  const mine = HUMAN.has(e.actor)
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <Avatar event={e} />}
      <div className="bubble" title={time(e.resolvedAt ?? e.createdAt)} {...menu(e.id)}>
        {e.type === 'item' ? <TaskCard event={e} /> : <Markdown text={e.message ?? ''} highlight={find} />}
        <span className="bubble-time">{shortTime(e.resolvedAt ?? e.createdAt)}</span>
      </div>
    </div>
  )
}
