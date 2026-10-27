import type { Catalog, EventDetail, ItemEvent } from '../../shared/types'
import { Approval } from './Approval'
import { minutes, time } from './Chat'
import { TYPE_NAME } from './icons'
import { SlackMessages } from './SlackView'
import { Widget } from './Widget'
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
      {e.type === 'widget' && (
        <section className="card">
          <Widget event={e} tall />
        </section>
      )}
      {(e.type === 'slack_message' || e.type === 'slack_channel') && <SlackMessages event={e} />}
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
  // When and by whom are known for what resolved since the app started; egdb's history of `resolved` has the rest.
  const how = e.cancelled ? 'Cancelled' : 'Resolved'
  return e.resolvedAt === null ? how : `${how} · ${e.resolvedBy} · ${minutes(Math.max(0, e.resolvedAt - e.createdAt))}`
}

/** What only some types have to show. */
function Special({ detail }: { detail: EventDetail; onOpen: (id: string) => void }) {
  const e = detail.event
  if (e.type !== 'script') return null
  return (
    <section className="card">
      <h2>Script</h2>
      <p className="muted">
        {e.params.schedule ? `every [${(e.params.schedule as number[]).join(', ')}]s` : 'run by hand'} · run {e.runs}×
        {!e.resolved && e.nextCheckAt && ` · next ${new Date(e.nextCheckAt).toLocaleTimeString()}`}
      </p>
      <Markdown text={fenced(String(e.params.source ?? ''), 'js')} />
      {Array.isArray(e.data?.logs) && (e.data!.logs as string[]).length > 0 && <pre className="output">{(e.data!.logs as string[]).join('\n')}</pre>}
    </section>
  )
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

/** When an event was added and resolved, and by whom. */
export function Timeline({ e }: { e: ItemEvent }) {
  const rows: [string, number | null, string | null][] = [
    ['Added', e.createdAt, e.createdBy],
    ['Resolved', e.resolved ? e.resolvedAt : null, e.resolvedBy],
  ]
  return (
    <table className="timeline">
      <tbody>
        {rows
          .filter(([, at]) => at !== null)
          .map(([label, at, by]) => (
            <tr key={label}>
              <td className="muted">{label}</td>
              <td>{time(at!)}</td>
              <td className="muted">{by}</td>
            </tr>
          ))}
      </tbody>
    </table>
  )
}
