import { useState } from 'react'
import type { ItemDetail, ItemEvent } from '../../shared/types'
import { Avatar } from './icons'

const time = (ms: number) => new Date(ms).toLocaleString()
const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
export const minutes = (ms: number) => (ms < 60_000 ? `${Math.max(1, Math.round(ms / 1000))}s` : ms < 3_600_000 ? `${Math.round(ms / 60_000)}m` : `${(ms / 3_600_000).toFixed(1)}h`)

/** One line saying what an event is for. Its type is its icon, so the line doesn't say it. */
export function summary(e: { type: ItemEvent['type']; params: Record<string, unknown> }): string {
  const p = e.params
  switch (e.type) {
    case 'message':
      return String(p.text)
    case 'action':
      return `Ran ${p.name}`
    case 'closed':
      return p.how === 'handled_outside' ? 'Done outside the tool' : 'Closed'
    case 'approval':
      return `${p.reason ?? `Run ${p.action}?`}`
    case 'bot':
      return String(p.instructions ?? 'A bot takes it up')
    case 'github':
      return p.repo && p.number ? `${p.repo}#${p.number}${p.title ? ` ${p.title}` : ''}: until ${p.until}` : `“${p.query}”: until ${p.until}`
    case 'slack':
      return `${p.message ? `Post “${p.message}”, then w` : 'W'}ait for a reply in ${p.channel}`
    case 'items':
      return `${(p.item_ids as string[]).length} item(s) to close`
    case 'after':
      return String(p.description ?? 'Then what comes next')
  }
}

/** What a resolved event says in the chat, or null for one that says nothing a message doesn't. */
function said(e: ItemEvent): { text: string; mine: boolean } | null {
  const d = e.data ?? {}
  if (e.cancelled) return null
  switch (e.type) {
    case 'message':
      return { text: String(e.params.text), mine: e.createdBy === 'human' }
    case 'action':
      return { text: `Ran ${e.params.name}`, mine: e.createdBy === 'human' || e.createdBy === 'mcp' }
    case 'closed':
      return { text: summary(e), mine: e.createdBy === 'human' }
    case 'approval':
      return { text: String(d.detail ?? `Ran ${e.params.action}`), mine: true }
    case 'bot':
      return null // what it said is its message; a failure, a system message
    case 'after':
      return d.error ? { text: `A script failed: ${d.error}`, mine: false } : null
    default:
      return { text: String(d.detail ?? d.outcome ?? 'Done'), mine: false }
  }
}

/**
 * The item's history as a chat: every event that has resolved, at the moment it
 * did. What the human said or did is on the right; everything else on the left,
 * with who it came from as its picture. Cancelled blockers are small notes in
 * the middle. A message shows its words and the time; pressing it shows the rest.
 */
export function Chat({ detail }: { detail: ItemDetail }) {
  const done = detail.events.filter((e) => e.resolved).sort((a, b) => a.resolvedAt! - b.resolvedAt!)
  return (
    <section className="chat">
      {done.map((e) => {
        if (e.cancelled)
          return (
            <div key={e.id} className="chat-note" title={time(e.resolvedAt!)}>
              Cancelled: {summary(e)}
            </div>
          )
        const s = said(e)
        return s && <Message key={e.id} event={e} text={s.text} mine={s.mine} detail={detail} />
      })}
    </section>
  )
}

function Message({ event: e, text, mine, detail }: { event: ItemEvent; text: string; mine: boolean; detail: ItemDetail }) {
  const [open, setOpen] = useState(false)
  const run = detail.runs.find((r) => r.eventIds.includes(e.id))
  const output = e.data?.output
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <Avatar by={e.createdBy} type={e.type} />}
      <div className={`bubble ${open ? 'open' : ''}`} onClick={() => setOpen(!open)}>
        {text}
        <span className="bubble-time">{shortTime(e.resolvedAt ?? e.createdAt)}</span>
        {open && (
          <div className="more" onClick={(ev) => ev.stopPropagation()}>
            <div className="bubble-meta">
              {e.type} · added by {e.createdBy}
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
