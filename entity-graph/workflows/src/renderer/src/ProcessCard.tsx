import { Square } from 'lucide-react'
import { useEffect, useState } from 'react'
import type { ItemEvent, ProcessStatus } from '../../shared/types'
import { minutes } from './Chat'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * A process: how it stands, Stop while it runs, and the last `lines` lines it wrote, read from the backend each
 * second while it runs (once it has ended, what it ended with).
 */
export function ProcessCard({ event: e, act, lines: n }: { event: ItemEvent; act: Act; lines: number }) {
  const [out, setOut] = useState<{ status: ProcessStatus | null; lines: string[] } | null>(null)
  useEffect(() => {
    let live = true
    const read = () => void api().processOutput(e.id, n).then((o) => live && setOut(o))
    read()
    if (e.resolved) return () => void (live = false)
    const t = setInterval(read, 1000)
    return () => {
      live = false
      clearInterval(t)
    }
  }, [e.id, e.resolved, n])
  const s = out?.status
  const lines = out?.lines.length ? out.lines : Array.isArray(e.data?.output) ? (e.data.output as string[]).slice(-n) : []
  const state = !e.resolved
    ? s?.running
      ? `running${s.pid ? ` · pid ${s.pid}` : ''} · ${minutes(Date.now() - s.startedAt)}`
      : 'starting…'
    : (e.message ?? e.summary)
  return (
    <div className="event-card process-card">
      <div className="script-line">
        <code className="process-command">{String(e.params.command)}</code>
        {!e.resolved && (
          <button data-ui className="script-play" title="Stop it" aria-label="Stop it" onClick={() => void act(() => api().cancel(e.id))}>
            <Square className="icon" />
          </button>
        )}
      </div>
      <div className="muted script-runs">{state}</div>
      {lines.length > 0 && <pre className="output process-output">{lines.join('\n')}</pre>}
    </div>
  )
}
