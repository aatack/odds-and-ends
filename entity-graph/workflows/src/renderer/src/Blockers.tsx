import { X } from 'lucide-react'
import { useEffect, useState } from 'react'
import type { Catalog, ItemDetail, ItemEvent } from '../../shared/types'
import { minutes } from './Chat'
import { Form, withHints } from './forms'
import { TypeIcon } from './icons'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

/** The item's open blockers, a row each, with how long each has held it. Above the chat input. */
export function Blockers({ detail, catalog, act }: { detail: ItemDetail; catalog: Catalog; act: Act }) {
  // The ages count up while the panel is open.
  const [now, setNow] = useState(Date.now())
  useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 15_000)
    return () => clearInterval(t)
  }, [])
  const open = detail.item.blockers
  if (!open.length) return null
  return (
    <section className="blockers">
      {open.map((b) => (
        <Row key={b.id} b={b} detail={detail} catalog={catalog} act={act} now={now} />
      ))}
    </section>
  )
}

function Row({ b, detail, catalog, act, now }: { b: ItemEvent; detail: ItemDetail; catalog: Catalog; act: Act; now: number }) {
  const running = b.type === 'bot' && detail.item.worker !== null
  const status =
    b.type === 'bot'
      ? running
        ? 'a bot is working'
        : 'waiting for a free bot'
      : b.type === 'approval'
        ? 'waiting for you'
        : b.nextCheckAt
          ? `next check ${shortTime(b.nextCheckAt)}`
          : ''
  return (
    <div className="blocker">
      <div className="blocker-line">
        <TypeIcon type={b.type} />
        <span className="blocker-text">{b.summary}</span>
        <span className="muted blocker-age" title={new Date(b.createdAt).toLocaleString()}>
          {status && `${status} · `}for {minutes(Math.max(0, now - b.createdAt))}
        </span>
        <button className="blocker-cancel" title="Cancel" aria-label="Cancel" onClick={() => act(() => api().cancel(b.id))}>
          <X className="icon" />
        </button>
      </div>
      {b.lastError && <div className="error-text blocker-error">{b.lastError}</div>}
      {b.type === 'approval' && <Approval b={b} catalog={catalog} act={act} />}
    </div>
  )
}

function Approval({ b, catalog, act }: { b: ItemEvent; catalog: Catalog; act: Act }) {
  const name = String(b.params.action)
  const action = catalog.actions.find((a) => a.name === name)
  const [inputs, setInputs] = useState<Record<string, unknown>>((b.params.inputs as Record<string, unknown>) ?? {})
  return (
    <div className="approval">
      {action ? (
        <Form schema={withHints(action.inputSchema)} value={inputs} setValue={(v) => setInputs(v as Record<string, unknown>)} />
      ) : (
        <p className="error-text">No action {name}.</p>
      )}
      <button data-ui className="primary" title={action?.description} onClick={() => act(() => api().approve(b.id, inputs))}>
        Run {name}
      </button>
    </div>
  )
}
