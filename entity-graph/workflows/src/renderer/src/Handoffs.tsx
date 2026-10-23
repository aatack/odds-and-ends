import { Fragment, useState } from 'react'
import type { Handoff, HandoffSpec, ItemDetail } from '../../shared/types'

const time = (ms: number) => new Date(ms).toLocaleString()
const minutes = (ms: number) => (ms < 60_000 ? `${Math.round(ms / 1000)}s` : `${Math.round(ms / 60_000)}m`)

/** One line saying what a handoff is for. */
export function summary(h: HandoffSpec): string {
  const p = h.params
  switch (h.type) {
    case 'human':
      return `You: ${p.needed}${p.approval ? ` (approve ${(p.approval as { action: string }).action})` : ''}`
    case 'agent':
      return `Agent: ${p.instructions}`
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
  return String(r.error ? `failed: ${r.error}` : (r.detail ?? r.decision ?? r.outcome ?? r.state ?? ''))
}

/** Every handoff resolved so far, with the notes and changes between them. Agent output is hidden until asked for. */
export function History({ detail }: { detail: ItemDetail }) {
  const done = detail.handoffs.filter((h) => h.status === 'resolved' || h.status === 'cancelled')
  const entries = [
    ...done.map((h) => ({ at: h.resolvedAt ?? h.createdAt, handoff: h })),
    ...detail.events.filter((e) => e.kind !== 'created').map((e) => ({ at: e.at, event: e })),
  ].sort((a, b) => a.at - b.at)
  return (
    <section className="history">
      {entries.map((e, i) => (
        <Fragment key={i}>{'handoff' in e ? <HandoffEntry h={e.handoff!} detail={detail} /> : <EventEntry e={e.event!} />}</Fragment>
      ))}
      {!entries.length && <p className="muted">Nothing yet.</p>}
    </section>
  )
}

function HandoffEntry({ h, detail }: { h: Handoff; detail: ItemDetail }) {
  const [open, setOpen] = useState(false)
  const run = detail.runs.find((r) => r.handoffId === h.id)
  const output = h.resolution?.output as string | undefined
  return (
    <div className="event handoff-entry">
      <div>
        <b>{summary(h)}</b>
      </div>
      <div className="muted">
        set by {h.createdBy}
        {h.startedAt && h.resolvedAt && ` · ${minutes(h.resolvedAt - h.startedAt)}`}
        {run?.costUsd != null && ` · $${run.costUsd.toFixed(2)}`} · {h.resolvedAt ? time(h.resolvedAt) : ''}
      </div>
      <div className="event-text">→ {outcome(h)}</div>
      {(output || h.stateAtStart) && (
        <button data-ui className="link" onClick={() => setOpen(!open)}>
          {open ? 'Less' : 'More'}
        </button>
      )}
      {open && (
        <div className="more">
          {h.stateAtStart !== null && (
            <p>
              <span className="muted">State as it started: </span>
              {h.stateAtStart || '(none)'}
            </p>
          )}
          {output && <pre className="output">{output}</pre>}
          <pre className="output">{JSON.stringify({ params: h.params, resolution: { ...h.resolution, output: undefined } }, null, 2)}</pre>
        </div>
      )}
    </div>
  )
}

function EventEntry({ e }: { e: ItemDetail['events'][number] }) {
  return (
    <div className="event">
      <span className="muted">
        {time(e.at)} · {e.actor} · {e.kind}
      </span>
      <div className="event-text">{e.text}</div>
    </div>
  )
}
