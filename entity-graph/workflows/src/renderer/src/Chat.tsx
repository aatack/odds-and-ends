import { CircleCheck, Play, ThumbsUp, Timer } from 'lucide-react'
import { memo, useCallback, useEffect, useRef, useState } from 'react'
import type { Catalog, ItemDetail, ItemEvent, Overview } from '../../shared/types'
import { isPerson, nameOf } from './people'
import { useEventMenu } from './EventMenu'
import { Avatar } from './icons'
import { useAtomValue } from 'jotai'
import { fenced, Markdown, shows } from './components/ui/Markdown'
import { findAtom } from './views'
import { TaskCard } from './TaskCard'
import { ProcessCard } from './ProcessCard'
import { Approval } from './Approval'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

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

/** The time, by the second: for what counts down (only those cards, so the rest of the chat isn't drawn again each second). */
function useNow() {
  const [now, setNow] = useState(Date.now())
  useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 1_000)
    return () => clearInterval(t)
  }, [])
  return now
}

/** The words an event shows in the chat, for Ctrl+F. */
const words = (e: ItemEvent) => (e.type === 'note' ? String(e.params.text ?? '') : [e.summary, e.message].filter(Boolean).join('\n'))

/**
 * The task's chat: every event in it (its `contains` links), in the order they were added, each as its card. What
 * the person looking added is on the right; everything else on the left, with who it came from as its picture (and,
 * for other people, their name). A note is its words; a child task, its card (which opens it); a script, what it
 * waits for and where it stands; an approval, its form while open. Cancelled events are small notes in the middle.
 * While Ctrl+F's find line has a phrase in it, only what shows the phrase is left, with the phrase marked.
 */
export function Chat(props: { detail: ItemDetail; overview: Overview; catalog: Catalog; act: Act; onOpen: (id: string) => void }) {
  const { detail, overview, catalog } = props
  const menu = useEventMenu()
  const find = useAtomValue(findAtom)?.trim() ?? ''
  // The callbacks are the same from one render to the next (calling the latest given), so a message that hasn't
  // changed isn't drawn again: a long chat stays quick.
  const latest = useRef(props)
  latest.current = props
  const act = useCallback<Act>((fn) => latest.current.act(fn), [])
  const onOpen = useCallback((id: string) => latest.current.onOpen(id), [])
  const shown = detail.events.filter((e) => !find || shows(words(e), find))
  return (
    <section className="chat">
      {find && !shown.length && <div className="chat-note">Nothing here says “{find}”.</div>}
      {shown.map((e) =>
        e.cancelled && e.type !== 'task' ? (
          <div key={e.id} className="chat-note" title={time(e.createdAt)} {...menu(e.id)}>
            {e.message}
          </div>
        ) : (
          <Message
            key={e.id}
            event={e}
            mine={e.createdBy === overview.me.id}
            me={overview.me.id}
            who={isPerson(overview, e.createdBy) ? nameOf(overview, e.createdBy) : null}
            find={find}
            catalog={catalog}
            act={act}
            onOpen={onOpen}
          />
        ),
      )}
    </section>
  )
}

type MessageProps = { event: ItemEvent; mine: boolean; me: string; who: string | null; find: string; catalog: Catalog; act: Act; onOpen: (id: string) => void }

/** One event, as a message: right-click it to open it or debug it. Drawn again only when something it shows changed. */
const Message = memo(function Message(props: MessageProps) {
  const { event: e, mine, me, who, act } = props
  const menu = useEventMenu()
  // Thumbs-ups: a count under the bubble (pressing it toggles yours), or, with none yet, a button shown on hover.
  const reacted = e.reactions.includes(me)
  const react = () => void act(() => api().react(e.id, !reacted))
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <Avatar event={e} />}
      <div className="bubble-stack">
        <div className={`bubble bubble-${e.type}`} title={`${who ? `${who}, ` : ''}${time(e.createdAt)}`} {...menu(e.id)}>
          {!mine && who && <span className="bubble-who">{who}</span>}
          <Card {...props} />
          <span className="bubble-time">{shortTime(e.createdAt)}</span>
        </div>
        <button
          data-ui
          className={`reaction ${e.reactions.length ? 'some' : ''} ${reacted ? 'on' : ''}`}
          title={e.reactions.length ? `${e.reactions.length} thumbs up` : 'Thumbs up'}
          onClick={react}
        >
          <ThumbsUp className="icon" />
          {e.reactions.length > 0 && e.reactions.length}
        </button>
      </div>
    </div>
  )
}, (a, b) => {
  const { event: ea, ...ra } = a
  const { event: eb, ...rb } = b
  return (Object.keys(ra) as (keyof typeof ra)[]).every((k) => ra[k] === rb[k]) && JSON.stringify(ea) === JSON.stringify(eb)
})

/** What an event shows in the chat, by its type. */
function Card(props: { event: ItemEvent; find: string; catalog: Catalog; act: Act; onOpen: (id: string) => void }) {
  const { event: e, find, catalog, act, onOpen } = props
  if (e.type === 'note') return <Markdown text={String(e.params.text ?? '')} highlight={find} />
  if (e.type === 'task')
    return (
      <button data-ui className="link card-link" onClick={() => onOpen(e.id)}>
        <TaskCard event={e} />
      </button>
    )
  if (e.type === 'script') return <ScriptCard event={e} act={act} find={find} />
  if (e.type === 'process') return <ProcessCard event={e} act={act} lines={8} />
  if (e.type === 'approval')
    return (
      <div className="event-card">
        <Markdown text={e.resolved ? (e.message ?? e.summary) : e.summary} highlight={find} />
        {!e.resolved && <Approval b={e} catalog={catalog} act={act} />}
      </div>
    )
  return <Markdown text={e.message ?? e.summary} highlight={find} />
}

/**
 * A script: what it waits for; then a tick and what it finished with, once done; a play button, if it has no
 * schedule (it runs when pressed); or else how many times it has run and when it runs next.
 */
function ScriptCard({ event: e, act, find }: { event: ItemEvent; act: Act; find: string }) {
  const now = useNow()
  const scheduled = Array.isArray(e.params.schedule) && (e.params.schedule as unknown[]).length > 0
  const error = typeof e.data?.error === 'string' ? e.data.error : null
  const result = e.data?.message ?? (e.data?.result === undefined || e.data?.result === null ? null : e.data.result)
  return (
    <div className="event-card script-card">
      <div className="script-line">
        <Markdown className="script-what" text={e.summary} highlight={find} />
        {e.resolved ? (
          <CircleCheck className={`icon ${error ? 'script-failed' : 'script-done'}`} aria-label={error ? 'Failed' : 'Done'} />
        ) : !scheduled ? (
          <button data-ui className="script-play" title="Run it" aria-label="Run it" onClick={() => void act(() => api().runNow(e.id))}>
            <Play className="icon" />
          </button>
        ) : (
          <span className="script-timer muted" title={e.nextCheckAt ? time(e.nextCheckAt) : undefined}>
            <Timer className="icon" />
            {e.runs}× · {e.nextCheckAt ? (e.nextCheckAt > now ? `in ${minutes(e.nextCheckAt - now)}` : 'now') : '…'}
          </span>
        )}
      </div>
      {!e.resolved && !scheduled && e.runs > 0 && <div className="muted script-runs">Run {e.runs}× and not done yet.</div>}
      {e.lastError && !e.resolved && <div className="error-text">{e.lastError}</div>}
      {e.resolved && error && <div className="error-text">{error}</div>}
      {e.resolved && !error && result !== null && (
        <div className="script-output">
          {typeof result === 'string' ? <Markdown text={result} highlight={find} /> : <Markdown text={fenced(JSON.stringify(result, null, 2), 'json')} />}
        </div>
      )}
    </div>
  )
}
