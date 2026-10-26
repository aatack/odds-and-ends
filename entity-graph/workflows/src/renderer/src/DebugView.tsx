import { useEffect, useState } from 'react'
import type { Inspection } from '../../shared/types'
import { time } from './Chat'
import { api } from './useWorkflows'

/**
 * An item as egdb holds it: who made it and when, its values (a note's is just
 * its `text`), each with when it was set and by whom, and its links, each with
 * who wrote it and when.
 */
export function DebugView({ id }: { id: string }) {
  const [it, setIt] = useState<Inspection | null>(null)
  const [error, setError] = useState<string | null>(null)
  useEffect(() => {
    const load = () => void api().inspect(id).then(setIt, (e: Error) => setError(e.message))
    load()
    return api().onChange(load)
  }, [id])
  if (error) return <div className="error">{error}</div>
  if (!it) return <p className="muted">Reading…</p>
  return (
    <div className="page debug-view">
      <section className="card">
        <table className="params">
          <tbody>
            <tr>
              <td className="muted">id</td>
              <td className="mono">{it.id}</td>
            </tr>
            <tr>
              <td className="muted">created</td>
              <td>
                {it.createdAt !== null ? time(it.createdAt) : '—'} by {it.createdBy ?? '—'}
              </td>
            </tr>
          </tbody>
        </table>
      </section>
      <section className="card">
        <h2>Values</h2>
        <table className="params">
          <tbody>
            {Object.entries(it.values).map(([k, v]) => (
              <tr key={k}>
                <td className="muted mono">{k}</td>
                <td className="mono">{typeof v === 'string' ? v : JSON.stringify(v, null, 2)}</td>
                <td className="muted">{it.set[k] ? `${time(it.set[k].at)} ${it.set[k].by}` : ''}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>
      <section className="card">
        <h2>Links</h2>
        {!it.links.length && <p className="muted">None.</p>}
        <table className="params">
          <tbody>
            {it.links.map((l, i) => (
              <tr key={i}>
                <td className="muted mono">
                  {l.direction === 'outbound' ? `${l.type} →` : `← ${l.type}`}
                </td>
                <td className="mono">{l.other}</td>
                <td className="muted">
                  {l.at !== null ? time(l.at) : ''} {l.by ?? ''}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>
    </div>
  )
}
