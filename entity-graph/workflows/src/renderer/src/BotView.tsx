import { ChevronDown, ChevronRight } from 'lucide-react'
import { useState } from 'react'
import type { Agent, EventDetail } from '../../shared/types'
import { minutes, time } from './Chat'
import { BOT_FORM, botStatus, type BotDraft } from './BotsPage'
import { Form } from './forms'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

const draftOf = (b: Agent): BotDraft => ({
  name: b.name,
  instructions: b.instructions,
  provider: b.provider,
  model: b.model,
  debounceMinutes: b.debounceMinutes,
  folder: b.folder,
})

/**
 * The top of a bot's page: where it is, its settings (folded away until wanted;
 * saving them wakes it), and its last runs, a line each. Its own chat is below
 * this, as any task's is.
 */
export function BotView({ detail, bot, act }: { detail: EventDetail; bot: Agent | null; act: Act }) {
  const [draft, setDraft] = useState<BotDraft | null>(bot && draftOf(bot))
  const [open, setOpen] = useState(false)
  if (!bot || !draft) return <p className="muted">This bot was deleted.</p>
  const changed = JSON.stringify(draft) !== JSON.stringify(draftOf(bot))
  return (
    <div className="bot-top">
      <div className="bot-line">
        <button data-ui className="link" onClick={() => setOpen(!open)} aria-expanded={open}>
          {open ? <ChevronDown className="icon" /> : <ChevronRight className="icon" />} Settings
        </button>
        <span className="muted">
          {bot.model} · waits {bot.debounceMinutes}m · {botStatus(bot)}
        </span>
        <button data-ui disabled={!!bot.runId || !bot.assigned.length} onClick={() => void act(() => api().wakeBot(bot.id))} title="Run now, whatever the debounce says">
          Wake now
        </button>
      </div>
      {open && (
        <section className="card">
          <Form schema={BOT_FORM} value={draft} setValue={(v) => setDraft({ ...draft, ...(v as BotDraft) })} />
          <div className="buttons">
            <button data-ui className="primary" disabled={!changed || !draft.name.trim()} onClick={() => void act(() => api().saveBot({ ...draft, id: bot.id }))}>
              Save
            </button>
            <button data-ui disabled={!changed} onClick={() => setDraft(draftOf(bot))}>
              Undo changes
            </button>
          </div>
        </section>
      )}
      {detail.runs.length > 0 && (
        <table className="runs">
          <tbody>
            {[...detail.runs]
              .reverse()
              .slice(0, 5)
              .map((r) => (
                <tr key={r.id} title={r.result ?? ''}>
                  <td className="muted">{time(r.startedAt)}</td>
                  <td>
                    {r.itemIds.length} task{r.itemIds.length === 1 ? '' : 's'}
                  </td>
                  <td>{r.endedAt ? minutes(r.endedAt - r.startedAt) : 'running'}</td>
                  <td>{r.costUsd !== null ? `$${r.costUsd.toFixed(2)}` : ''}</td>
                </tr>
              ))}
          </tbody>
        </table>
      )}
    </div>
  )
}
