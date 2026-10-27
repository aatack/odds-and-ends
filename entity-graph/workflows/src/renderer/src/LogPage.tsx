import { useContext, useEffect, useState } from 'react'
import type { LogEntry, Overview } from '../../shared/types'
import { time } from './Chat'
import { ItemRefContext } from './components/ui/Markdown'
import { ItemPicker } from './ItemPicker'
import { everyone, nameOf } from './people'
import type { Recent } from './sections'
import { api } from './useWorkflows'

const HOUR = 3_600_000
const SPANS = [
  { id: 'hour', name: 'Last hour', ms: HOUR },
  { id: 'day', name: 'Last day', ms: 24 * HOUR },
  { id: 'week', name: 'Last week', ms: 7 * 24 * HOUR },
  { id: 'all', name: 'All time', ms: null },
] as const

/** A value, in a line: text as it is, anything else as JSON; cut short. */
const brief = (v: unknown): string => {
  const s = v === null ? 'cleared' : typeof v === 'string' ? v : JSON.stringify(v)
  return s.length > 160 ? `${s.slice(0, 160)}…` : s
}

/**
 * The edit log: what was done, by whom and when, the newest first, as egdb keeps it. Choose who (people and bots;
 * nobody chosen is everyone), what (tasks or any event; none is everything) and how far back.
 */
export function LogPage({ overview, recent, entities: given = [] }: { overview: Overview; recent: Recent; entities?: string[] }) {
  const ref = useContext(ItemRefContext)
  const [authors, setAuthors] = useState<string[]>([])
  const [entities, setEntities] = useState<string[]>(given)
  const [span, setSpan] = useState<(typeof SPANS)[number]['id']>('day')
  const [picking, setPicking] = useState(false)
  const [log, setLog] = useState<LogEntry[] | null>(null)
  const [error, setError] = useState<string | null>(null)
  useEffect(() => {
    const ms = SPANS.find((s) => s.id === span)!.ms
    let live = true
    setError(null)
    api()
      .editLog({
        authors: authors.length ? authors.flatMap((a) => (overview.bots.some((b) => b.id === a) ? [`bot:${a}`] : [a])) : undefined,
        entities: entities.length ? entities : undefined,
        since: ms ? Date.now() - ms : undefined,
        limit: 300,
      })
      .then((l) => live && setLog(l))
      .catch((e) => live && setError((e as Error).message))
    return () => void (live = false)
  }, [authors, entities, span, overview.bots])
  const people = everyone(overview)
  const toggle = (id: string) => setAuthors(authors.includes(id) ? authors.filter((a) => a !== id) : [...authors, id])
  return (
    <div className="page log-page">
      <div className="log-filters">
        <div className="choices">
          <span className="muted">Who</span>
          {people.map((p) => (
            <button key={p.id} data-ui className={authors.includes(p.id) ? 'chosen' : ''} onClick={() => toggle(p.id)}>
              {p.bot ? `@${p.name}` : p.name}
            </button>
          ))}
        </div>
        <div className="choices">
          <span className="muted">What</span>
          {entities.map((id) => (
            <span key={id} className="log-chosen">
              {ref ? ref(id) : id}
              <button data-ui className="link" title="Remove" onClick={() => setEntities(entities.filter((e) => e !== id))}>
                ×
              </button>
            </span>
          ))}
          <button data-ui onClick={() => setPicking(true)}>
            {entities.length ? 'Change…' : 'Everything · choose…'}
          </button>
        </div>
        <div className="choices">
          <span className="muted">When</span>
          {SPANS.map((s) => (
            <button key={s.id} data-ui className={span === s.id ? 'chosen' : ''} onClick={() => setSpan(s.id)}>
              {s.name}
            </button>
          ))}
        </div>
      </div>
      {error && <div className="error">{error}</div>}
      {log && !log.length && <p className="muted">Nothing was done here then.</p>}
      {log && log.length > 0 && (
        <table className="log-table">
          <tbody>
            {log.map((e, i) => (
              <tr key={i}>
                <td className="muted log-when" title={time(e.at)}>
                  {new Date(e.at).toLocaleString([], { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })}
                </td>
                <td className="log-who">{nameOf(overview, e.by)}</td>
                <td className="log-what">{ref ? ref(e.entity) : e.entity}</td>
                <td className="log-key">
                  {e.kind === 'link' ? `${e.removed ? 'unlinked' : 'linked'} ${e.key}` : e.key}
                </td>
                <td className="log-value">{e.kind === 'link' ? (ref && e.other ? ref(e.other) : e.other) : brief(e.value)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
      {picking && (
        <ItemPicker
          title="What to see the log of"
          items={overview.items}
          recent={recent}
          verb="Choose"
          multiple
          value={entities}
          setValue={setEntities}
          onClose={() => setPicking(false)}
        />
      )}
    </div>
  )
}
