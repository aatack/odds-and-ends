import { useState } from 'react'
import type { ItemDetail, ItemEvent } from '../../shared/types'
import { Avatar } from './icons'

const time = (ms: number) => new Date(ms).toLocaleString()
const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
export const minutes = (ms: number) => (ms < 60_000 ? `${Math.max(1, Math.round(ms / 1000))}s` : ms < 3_600_000 ? `${Math.round(ms / 60_000)}m` : `${(ms / 3_600_000).toFixed(1)}h`)

/** The human, or an outside client acting as them: their events are on the right. */
const HUMAN = new Set(['human', 'mcp'])

/**
 * The item's history as a chat: every event that has resolved, at the moment it
 * did. What the human said or did is on the right; everything else on the left,
 * with who it came from as its picture. Cancelled blockers are small notes in
 * the middle. A message shows its words and the time; pressing it shows the rest.
 */
export function Chat({ detail }: { detail: ItemDetail }) {
  const done = detail.events.filter((e) => e.resolved && e.message !== null).sort((a, b) => a.resolvedAt! - b.resolvedAt!)
  return (
    <section className="chat">
      {done.map((e) =>
        e.cancelled ? (
          <div key={e.id} className="chat-note" title={time(e.resolvedAt!)}>
            {e.message}
          </div>
        ) : (
          <Message key={e.id} event={e} detail={detail} />
        ),
      )}
    </section>
  )
}

function Message({ event: e, detail }: { event: ItemEvent; detail: ItemDetail }) {
  const [open, setOpen] = useState(false)
  const mine = HUMAN.has(e.actor)
  const run = detail.runs.find((r) => r.eventIds.includes(e.id))
  const output = e.data?.output
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <Avatar by={e.actor} type={e.type} />}
      <div className={`bubble ${open ? 'open' : ''}`} onClick={() => setOpen(!open)}>
        {e.message}
        <span className="bubble-time">{shortTime(e.resolvedAt ?? e.createdAt)}</span>
        {open && (
          <div className="more" onClick={(ev) => ev.stopPropagation()}>
            <div className="bubble-meta">
              {e.type}: {e.summary} · added by {e.createdBy}
              {e.resolvedAt !== e.createdAt && ` · blocked for ${minutes(e.resolvedAt! - e.createdAt)}`}
              {run?.costUsd != null && ` · $${run.costUsd.toFixed(2)}`}
            </div>
            <Timeline e={e} />
            {typeof output === 'string' && output && <pre className="output">{output}</pre>}
            <pre className="output">{JSON.stringify({ params: e.params, data: e.data && { ...e.data, output: typeof output === 'string' ? undefined : output } }, null, 2)}</pre>
          </div>
        )}
      </div>
    </div>
  )
}

/** When each thing happened to an event. */
function Timeline({ e }: { e: ItemEvent }) {
  const t = e.times
  const rows: [string, number | null, string?][] = [
    ['Added', t.added ?? e.createdAt],
    ['First checked', t.firstChecked, t.checks > 1 ? `checked ${t.checks} times, last ${time(t.lastChecked!)}` : undefined],
    ['Bot started', t.firstRunStarted, t.runs > 1 ? `${t.runs} runs` : undefined],
    ['Bot finished', t.firstRunEnded],
    ['Resolved', t.resolved ?? e.resolvedAt],
  ]
  return (
    <table className="timeline">
      <tbody>
        {rows
          .filter(([, at]) => at !== null)
          .map(([label, at, extra]) => (
            <tr key={label}>
              <td className="muted">{label}</td>
              <td>{time(at!)}</td>
              <td className="muted">{extra}</td>
            </tr>
          ))}
      </tbody>
    </table>
  )
}
