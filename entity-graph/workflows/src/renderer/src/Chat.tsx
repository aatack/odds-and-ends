import type { Assignment, ItemDetail, ItemEvent, Overview } from '../../shared/types'
import { isPerson, nameOf } from './people'
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

/**
 * The item's history as a chat: every event that has resolved, at the moment it
 * did. What the person looking said or did is on the right; everything else on
 * the left, with who it came from as its picture (and, for other people, their
 * name). Cancelled blockers, and who was handed the task or taken off it, are
 * small notes in the middle. A message shows its words and the time; pressing it
 * shows the rest. A child task, once it has closed, is a bubble like any other,
 * holding its card (while open it is among the blockers instead).
 * While Ctrl+F's find line has a phrase in it, only what shows the phrase is
 * left, with the phrase marked.
 */
export function Chat({ detail, overview }: { detail: ItemDetail; overview: Overview }) {
  const menu = useEventMenu()
  const find = useAtomValue(findAtom)?.trim() ?? ''
  const name = (id: string) => nameOf(overview, id)
  const said = (a: Assignment) => {
    const who = name(a.assignee)
    const text = a.on ? `${name(a.by)} handed it to ${who}` : `${name(a.by)} took it off ${who}`
    return text.charAt(0).toUpperCase() + text.slice(1)
  }
  const done: ({ at: number; event: ItemEvent } | { at: number; assignment: Assignment })[] = [
    ...detail.events
      .filter((e) => e.resolved && e.message !== null && (!find || shows(e.type === 'item' ? e.summary : e.message, find)))
      .map((event) => ({ at: event.resolvedAt!, event })),
    ...(find ? [] : detail.assignments.map((assignment) => ({ at: assignment.at, assignment }))),
  ].sort((a, b) => a.at - b.at)
  return (
    <section className="chat">
      {find && !done.length && <div className="chat-note">Nothing here says “{find}”.</div>}
      {done.map((d, i) =>
        'assignment' in d ? (
          <div key={`a${i}`} className="chat-note" title={time(d.at)}>
            {said(d.assignment)}
          </div>
        ) : d.event.cancelled && d.event.type !== 'item' ? (
          <div key={d.event.id} className="chat-note" title={time(d.at)} {...menu(d.event.id)}>
            {d.event.message}
          </div>
        ) : (
          <Message key={d.event.id} event={d.event} find={find} mine={d.event.actor === overview.me.id} who={isPerson(overview, d.event.actor) ? name(d.event.actor) : null} />
        ),
      )}
    </section>
  )
}

/** One resolved event, as a message: right-click it to open it or debug it. */
function Message({ event: e, find, mine, who }: { event: ItemEvent; find: string; mine: boolean; who: string | null }) {
  const menu = useEventMenu()
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <Avatar event={e} />}
      <div className="bubble" title={`${who ? `${who}, ` : ''}${time(e.resolvedAt ?? e.createdAt)}`} {...menu(e.id)}>
        {!mine && who && <span className="bubble-who">{who}</span>}
        {e.type === 'item' ? <TaskCard event={e} /> : <Markdown text={e.message ?? ''} highlight={find} />}
        <span className="bubble-time">{shortTime(e.resolvedAt ?? e.createdAt)}</span>
      </div>
    </div>
  )
}
