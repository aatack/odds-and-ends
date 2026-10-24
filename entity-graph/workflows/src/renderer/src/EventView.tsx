import type { Catalog, EventDetail, ItemEvent } from '../../shared/types'
import { Approval } from './Blockers'
import { minutes, time } from './Chat'
import { TYPE_NAME } from './icons'
import { fenced, Markdown } from './components/ui/Markdown'

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
        {e.message && !e.cancelled && <Markdown className="event-message" text={e.message} />}
        {e.lastError && <p className="error-text">{e.lastError}</p>}
      </section>

      {e.type === 'approval' && !e.resolved && (
        <section className="card">
          <Approval b={e} catalog={catalog} act={act} />
        </section>
      )}
      <Special detail={detail} onOpen={onOpen} />

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

/** Open or how it ended, by whom, and for how long: `Open · 4m 02s` or `Cancelled · human · 12s`. */
function status(e: ItemEvent): string {
  if (!e.resolved) return `Open · ${e.createdBy} · ${minutes(Date.now() - e.createdAt)}`
  return `${e.cancelled ? 'Cancelled' : 'Resolved'} · ${e.resolvedBy} · ${minutes(Math.max(0, e.resolvedAt! - e.createdAt))}`
}

/** What only some types have to show. */
function Special({ detail, onOpen }: { detail: EventDetail; onOpen: (id: string) => void }) {
  const e = detail.event
  if (e.type === 'attachment')
    return (
      <section className="card">
        <button data-ui className="link" onClick={() => onOpen(String(e.params.event))}>
          {String(e.params.title)}
        </button>
      </section>
    )
  const output = e.data?.output
  if (e.type === 'bot')
    return (
      <section className="card">
        <h2>Runs</h2>
        {detail.runs.map((r) => (
          <p key={r.id} className="muted">
            {time(r.startedAt)} · {r.endedAt ? minutes(r.endedAt - r.startedAt) : 'running'}
            {r.costUsd != null && ` · $${r.costUsd.toFixed(2)}`}
            {r.turns != null && ` · ${r.turns} turns`}
          </p>
        ))}
        {typeof output === 'string' && output && (
          <>
            <h2>Output</h2>
            <Markdown text={output} />
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
            {e.times.checks} runs{e.params.schedule ? ` · [${(e.params.schedule as number[]).join(', ')}]s` : ''}
            {!e.resolved && e.nextCheckAt && ` · next ${new Date(e.nextCheckAt).toLocaleTimeString()}`}
          </p>
        )}
        <Markdown text={fenced(String(code), 'js')} />
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
                <Markdown className="value" text={v} />
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
