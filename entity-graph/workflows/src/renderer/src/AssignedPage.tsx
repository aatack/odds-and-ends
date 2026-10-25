import { useCallback, useEffect, useState } from 'react'
import type { Assignment, Overview } from '../../shared/types'
import { AssignmentRows } from './BotView'
import { everyone, nameOf } from './people'
import { api } from './useWorkflows'

const DAYS = [1, 7, 30]

/** What was handed to whom lately: to you, another person, a bot, or anyone; the newest first. */
export function AssignedPage({ to, overview, onOpen, onPick }: { to: string; overview: Overview; onOpen: (id: string) => void; onPick: (to: string) => void }) {
  const [days, setDays] = useState(7)
  const [rows, setRows] = useState<Assignment[] | null>(null)
  const refresh = useCallback(async () => setRows(await api().assignments(to === 'anyone' ? null : to, days)), [to, days])
  useEffect(() => {
    void refresh()
    return api().onChange(() => void refresh())
  }, [refresh])
  const who = (id: string) => nameOf(overview, id)
  return (
    <div className="page assigned-page">
      <div className="choices">
        {[...everyone(overview).map((s) => s.id), 'anyone'].map((id) => (
          <button key={id} data-ui className={to === id ? 'chosen' : ''} onClick={() => onPick(id)}>
            {id === 'anyone' ? 'Anyone' : id === overview.me.id ? 'You' : who(id)}
          </button>
        ))}
      </div>
      <div className="choices">
        {DAYS.map((d) => (
          <button key={d} data-ui className={days === d ? 'chosen' : ''} onClick={() => setDays(d)}>
            {d === 1 ? 'Today' : `${d} days`}
          </button>
        ))}
      </div>
      <section className="card">
        {rows?.length === 0 && <p className="muted">Nothing in that time.</p>}
        {rows && <AssignmentRows rows={rows} onOpen={onOpen} who={to === 'anyone' ? who : undefined} />}
      </section>
    </div>
  )
}
