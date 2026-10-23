import type { Catalog, EventDetail, ItemEvent } from '../../shared/types'
import { Approval } from './Blockers'
import { minutes, time } from './Chat'
import { TYPE_NAME } from './icons'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** Any event but an item, on its own: what it is, where it stands, and what its type has to show. */
export function EventView({
  detail,
  catalog,
  act,
  onOpen,
}: {
  detail: EventDetail
  catalog: Catalog
  act: Act
  onOpen: (id: string) => void
}) {
  const e = detail.event
  return (
    <div className="page event-view">
      <div className="meta">
        {TYPE_NAME[e.type]}
        {detail.parent && (
          <>
            {' in '}
            <button data-ui className="link" onClick={() => onOpen(detail.parent!.id)}>
              {detail.parent.title}
            </button>
          </>
        )}
      </div>

      <section className="card">
        <p>{status(e)}</p>
        {e.message && !e.cancelled && <p className="event-message">{e.message}</p>}
        {e.lastError && <p className="error-text">{e.lastError}</p>}
      </section>

      {e.type === 'approval' && !e.resolved && (
        <section className="card">
          <Approval b={e} catalog={catalog} act={act} />
        </section>
      )}
      <Special detail={detail} />

      <section className="card">
        <h2>What it was given</h2>
        <Values values={e.params} />
        {e.data && (
          <>
            <h2>What it came to</h2>
            <Values values={{ ...e.data, output: undefined }} />
          </>
        )}
      </section>

      <section className="card">
        <h2>When</h2>
        <Timeline e={e} />
      </section>
    </div>
  )
}

function status(e: ItemEvent): string {
  const by = `added by ${e.createdBy}, ${time(e.createdAt)}`
  if (!e.resolved) return `Open for ${minutes(Date.now() - e.createdAt)}; ${by}.`
  const how = e.cancelled ? 'Cancelled' : 'Resolved'
  const took = e.resolvedAt! > e.createdAt ? ` after ${minutes(e.resolvedAt! - e.createdAt)}` : ''
  return `${how} by ${e.resolvedBy}${took}; ${by}.`
}

/** What only some types have to show. */
function Special({ detail }: { detail: EventDetail }) {
  const e = detail.event
  const output = e.data?.output
  if (e.type === 'bot')
    return (
      <section className="card">
        <h2>Runs</h2>
        {detail.runs.map((r) => (
          <p key={r.id} className="muted">
            {time(r.startedAt)} · {r.endedAt ? `took ${minutes(r.endedAt - r.startedAt)}` : 'running'}
            {r.costUsd != null && ` · $${r.costUsd.toFixed(2)}`}
            {r.turns != null && ` · ${r.turns} turns`}
          </p>
        ))}
        {typeof output === 'string' && output && (
          <>
            <h2>What it said on the way</h2>
            <pre className="output">{output}</pre>
          </>
        )}
      </section>
    )
  const code = e.type === 'after' ? e.params.script : e.type === 'script' ? e.params.source : undefined
  if (code)
    return (
      <section className="card">
        <h2>Script</h2>
        {e.type === 'script' && (
          <p className="muted">
            {e.times.checks} run(s){e.params.schedule ? `, on [${(e.params.schedule as number[]).join(', ')}] seconds` : ''}
            {!e.resolved && e.nextCheckAt && `; next at ${new Date(e.nextCheckAt).toLocaleTimeString()}`}
          </p>
        )}
        <pre className="output mono">{String(code)}</pre>
        {Array.isArray(e.data?.logs) && (e.data!.logs as string[]).length > 0 && (
          <pre className="output">{(e.data!.logs as string[]).join('\n')}</pre>
        )}
      </section>
    )
  if (e.type === 'github' && e.params.url)
    return (
      <section className="card">
        <a href={String(e.params.url)} target="_blank" rel="noreferrer">
          {String(e.params.url)}
        </a>
      </section>
    )
  return null
}

/** Values as a table: words as words, anything else as JSON. */
function Values({ values }: { values: Record<string, unknown> }) {
  const rows = Object.entries(values).filter(([, v]) => v !== undefined)
  if (!rows.length) return <p className="muted">Nothing.</p>
  return (
    <table className="params">
      <tbody>
        {rows.map(([k, v]) => (
          <tr key={k}>
            <td className="muted">{k}</td>
            <td>
              {typeof v === 'string' ? (
                <span className="value">{v}</span>
              ) : (
                <pre className="output">{JSON.stringify(v, null, 2)}</pre>
              )}
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  )
}

/** When each thing happened to an event. */
export function Timeline({ e }: { e: ItemEvent }) {
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
