import { useState } from 'react'
import type { Catalog, Handoff, HandoffKind, HandoffSpec, ItemDetail } from '../../shared/types'
import { CompactForm } from './CompactForm'

const time = (ms: number) => new Date(ms).toLocaleString()
const minutes = (ms: number) => (ms < 60_000 ? `${Math.round(ms / 1000)}s` : `${Math.round(ms / 60_000)}m`)

/** One line saying what a handoff is for. */
export function summary(h: HandoffSpec): string {
  const p = h.params
  switch (h.type) {
    case 'human':
      return `${p.needed}${p.approval ? ` (approve ${(p.approval as { action: string }).action})` : ''}`
    case 'bot':
      return `Bot: ${p.instructions}`
    case 'github':
      return `GitHub: ${p.repo}#${p.number} until ${p.until}`
    case 'slack':
      return `Slack: ${p.message ? `post “${p.message}”, then ` : ''}wait for a reply in ${p.channel}`
    case 'items':
      return `Items: wait for ${(p.item_ids as string[]).length} to close`
    case 'script':
      return `Script${p.description ? `: ${p.description}` : ''}`
  }
}

/** What a resolution comes to, in a line. */
export function outcome(h: Handoff): string {
  const r = h.resolution ?? {}
  if (h.status === 'cancelled') return 'cancelled'
  return String(r.error ? `failed: ${r.error}` : (r.detail ?? r.decision ?? r.outcome ?? ''))
}

/** A list of handoffs to put at the front of the queue: a type and its fields each, as tight as it will go. */
export function HandoffBuilder({ catalog, value, onChange }: { catalog: Catalog; value: HandoffSpec[]; onChange: (v: HandoffSpec[]) => void }) {
  const set = (i: number, h: HandoffSpec) => onChange(value.map((x, j) => (j === i ? h : x)))
  return (
    <div className="builder">
      {value.map((h, i) => {
        const type = catalog.handoffs.find((t) => t.type === h.type)!
        return (
          <div key={i} className="builder-row compact">
            <div className="row-inline">
              <select title={type.description} value={h.type} onChange={(e) => set(i, { type: e.target.value as HandoffKind, params: {} })}>
                {catalog.handoffs.map((t) => (
                  <option key={t.type}>{t.type}</option>
                ))}
              </select>
              {value.length > 1 && (
                <button data-ui className="link" title="Remove" onClick={() => onChange(value.filter((_, j) => j !== i))}>
                  ×
                </button>
              )}
            </div>
            <CompactForm schema={type.params} value={h.params} onChange={(params) => set(i, { ...h, params })} />
          </div>
        )
      })}
      <button data-ui className="link" onClick={() => onChange([...value, { type: 'bot', params: {} }])}>
        + then…
      </button>
    </div>
  )
}

/** Who set a handoff, as the chat names them. */
function sender(createdBy: string): string {
  if (createdBy === 'bot') return 'Bot'
  if (createdBy === 'script' || createdBy === 'script:bot') return 'Script'
  if (createdBy === 'system') return 'Workflows'
  if (createdBy === 'mcp') return 'MCP client'
  return createdBy
}

/** What the human did with a human handoff, as their reply; null when the handoffs they set say it. */
function reply(h: Handoff): string | null {
  const r = h.resolution ?? {}
  const action = r.action as { name: string; changed: string[] } | undefined
  if (r.decision === 'approved' && action)
    return `Ran ${action.name}${action.changed.length ? ` (changed ${action.changed.join(', ')})` : ''}`
  if (r.decision === 'closed') return 'Closed'
  if (r.decision === 'handled_outside') return 'Done outside the tool'
  return null
}

const WAITS = ['github', 'slack', 'items']

const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

type Entry =
  | { at: number; order: number; kind: 'handoff'; handoff: Handoff }
  | { at: number; order: number; kind: 'reply'; handoff: Handoff; text: string }
  | { at: number; order: number; kind: 'event'; event: ItemDetail['events'][number] }
  | { at: number; order: number; kind: 'note'; text: string }

/**
 * The item's history as a chat: every handoff a bubble, from whoever set it.
 * What the human set or did is on the right; everything else, on the left.
 * Events between them are small notes in the middle, and what is queued comes
 * last, faded. A bubble shows its words and the time; pressing it shows the rest.
 */
export function Chat({ detail }: { detail: ItemDetail }) {
  const shown = detail.handoffs.filter((h) => h.status !== 'queued')
  const queued = detail.handoffs.filter((h) => h.status === 'queued')
  const entries: Entry[] = []
  shown.forEach((h, order) => {
    entries.push({ at: h.createdAt, order, kind: 'handoff', handoff: h })
    const text =
      h.resolution?.outcome === 'taken_over' && h.resolvedBy === 'human'
        ? 'Took over'
        : h.type === 'human' && h.resolvedBy === 'human'
          ? reply(h)
          : null
    if (text) entries.push({ at: h.resolvedAt!, order, kind: 'reply', handoff: h, text })
    // What a wait came to is news in itself: nothing after it may say so.
    if (WAITS.includes(h.type) && h.status === 'resolved' && h.resolvedBy === 'system')
      entries.push({ at: h.resolvedAt!, order, kind: 'note', text: outcome(h) })
  })
  detail.events
    .filter((e) => !['created', 'handled_outside'].includes(e.kind))
    .forEach((e) => entries.push({ at: e.at, order: shown.length, kind: 'event', event: e }))
  entries.sort((a, b) => a.at - b.at || a.order - b.order)
  return (
    <section className="chat">
      {entries.map((e, i) =>
        e.kind === 'handoff' ? (
          <Bubble key={i} h={e.handoff} detail={detail} />
        ) : e.kind === 'reply' ? (
          <div key={i} className="bubble-row mine">
            <div className="bubble">
              {e.text}
              <span className="bubble-time">{shortTime(e.at)}</span>
            </div>
          </div>
        ) : e.kind === 'note' ? (
          <div key={i} className="chat-note" title={time(e.at)}>
            {e.text}
          </div>
        ) : (
          <div key={i} className="chat-note" title={time(e.event.at)}>
            {e.event.text}
          </div>
        ),
      )}
      {queued.map((h) => (
        <Bubble key={h.id} h={h} detail={detail} />
      ))}
    </section>
  )
}

function Bubble({ h, detail }: { h: Handoff; detail: ItemDetail }) {
  const [open, setOpen] = useState(false)
  const mine = h.createdBy === 'human'
  const run = detail.runs.find((r) => r.handoffId === h.id)
  const output = h.resolution?.output as string | undefined
  const status =
    h.status === 'active'
      ? 'waiting on this now'
      : h.status === 'queued'
        ? 'queued'
        : h.status === 'cancelled'
          ? 'cancelled'
          : `${outcome(h) || 'done'}${h.startedAt && h.resolvedAt ? ` · took ${minutes(h.resolvedAt - h.startedAt)}` : ''}`
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'} ${h.status}`}>
      <div className={`bubble ${open ? 'open' : ''}`} onClick={() => setOpen(!open)}>
        {!mine && <div className="bubble-sender">{sender(h.createdBy)}</div>}
        {summary(h)}
        <span className="bubble-time">{shortTime(h.createdAt)}</span>
        {open && (
          <div className="more" onClick={(e) => e.stopPropagation()}>
            <div className="bubble-meta">
              Set by {sender(h.createdBy).toLowerCase()} · {status}
              {run?.costUsd != null && ` · $${run.costUsd.toFixed(2)}`}
            </div>
            <Timeline times={h.times} />
            {output && <pre className="output">{output}</pre>}
            <pre className="output">{JSON.stringify({ params: h.params, resolution: h.resolution && { ...h.resolution, output: undefined } }, null, 2)}</pre>
          </div>
        )}
      </div>
    </div>
  )
}

/** When each thing happened to a handoff, as far as it has got. */
function Timeline({ times: t }: { times: Handoff['times'] }) {
  const rows: [string, number | null, string?][] = [
    ['Set', t.set],
    ['Started', t.started],
    ['First seen', t.firstViewed, t.views > 1 ? `seen ${t.views} times, last ${time(t.lastViewed!)}` : undefined],
    ['Skipped', t.skips ? t.lastViewed : null, t.skips ? `${t.skips} times` : undefined],
    ['Bot started', t.firstRunStarted, t.runs > 1 ? `${t.runs} runs` : undefined],
    ['Bot finished', t.firstRunEnded],
    ['First checked', t.firstChecked, t.checks > 1 ? `checked ${t.checks} times, last ${time(t.lastChecked!)}` : undefined],
    ['Resolved', t.resolved],
    ['Cancelled', t.cancelled],
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
