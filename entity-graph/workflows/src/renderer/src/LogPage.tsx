import { useContext, useEffect, useState } from 'react'
import type { LogEntry, Overview } from '../../shared/types'
import { PickPill } from './Assignees'
import { ItemRefContext } from './components/ui/Markdown'
import { nameOf } from './people'
import type { Recent } from './sections'
import { time } from './time'
import { api } from './useWorkflows'

/** How many lines a page of the log holds: Older shows another. */
const PAGE = 100

/** A value, in a line: text as it is, anything else as JSON; cut short. */
const brief = (v: unknown): string => {
  const s = typeof v === 'string' ? v : JSON.stringify(v)
  return s.length > 160 ? `${s.slice(0, 160)}…` : s
}

const when = (at: number) => new Date(at).toLocaleString([], { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })

/**
 * The edit log: what was done, by whom and when, the newest first, a page at a time, as egdb keeps it. Who (people
 * and bots) and what (any item) are chosen as an assignee is: nothing chosen is everyone, and everything. Each line
 * says what happened: set a field to a value, cleared it, linked one thing to another, or unlinked them.
 */
export function LogPage({ overview, recent, entities: given = [], mobile = false }: { overview: Overview; recent: Recent; entities?: string[]; mobile?: boolean }) {
  const ref = useContext(ItemRefContext)
  const pill = (id: string | null) => (id ? (ref ? ref(id) : id) : null)
  const [authors, setAuthors] = useState<string[]>([])
  const [entities, setEntities] = useState<string[]>(given)
  const [limit, setLimit] = useState(PAGE)
  const [log, setLog] = useState<LogEntry[] | null>(null)
  const [error, setError] = useState<string | null>(null)
  // A new choice starts again from the first page.
  useEffect(() => setLimit(PAGE), [authors, entities])
  useEffect(() => {
    let live = true
    setError(null)
    api()
      .editLog({
        // A bot's writes are by its runs (`bot:<id>`).
        authors: authors.length ? authors.map((a) => (overview.bots.some((b) => b.id === a) ? `bot:${a}` : a)) : undefined,
        entities: entities.length ? entities : undefined,
        limit,
      })
      .then((l) => live && setLog(l))
      .catch((e) => live && setError((e as Error).message))
    return () => void (live = false)
  }, [authors, entities, limit, overview.bots])
  return (
    <div className={`page log-page ${mobile ? 'mobile' : ''}`}>
      <div className="log-filters">
        <span className="muted">Who</span>
        <PickPill title="Who" none="Anyone" people overview={overview} value={authors} setValue={setAuthors} />
        <span className="muted">What</span>
        <PickPill title="What" none="Everything" overview={overview} items={overview.items} recent={recent} value={entities} setValue={setEntities} />
      </div>
      {error && <div className="error">{error}</div>}
      {log && !log.length && <p className="muted">Nothing was done here.</p>}
      {log && log.length > 0 && (
        <ol className="log-lines">
          {log.map((e, i) => (
            <li key={i} className="log-line">
              <span className="muted log-when" title={time(e.at)}>
                {when(e.at)}
              </span>
              <span className="log-who">{nameOf(overview, e.by)}</span>
              {e.kind === 'link' ? (
                <span className="log-what">
                  {e.removed ? 'unlinked' : 'linked'} {pill(e.entity)} <span className={`log-key ${e.removed ? 'removed' : ''}`}>{e.key}</span> {pill(e.other)}
                </span>
              ) : e.value === null ? (
                <span className="log-what">
                  cleared <span className="log-key">{e.key}</span> on {pill(e.entity)}
                </span>
              ) : (
                <span className="log-what">
                  set <span className="log-key">{e.key}</span> on {pill(e.entity)} to <span className="log-value">{brief(e.value)}</span>
                </span>
              )}
            </li>
          ))}
        </ol>
      )}
      {log && log.length >= limit && (
        <button data-ui onClick={() => setLimit(limit + PAGE)}>
          Older
        </button>
      )}
    </div>
  )
}
