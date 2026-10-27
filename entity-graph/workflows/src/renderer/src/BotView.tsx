import { ChevronDown, ChevronRight } from 'lucide-react'
import { useEffect, useState } from 'react'
import type { Agent } from '../../shared/types'
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

/** How its last run went, in a line. */
function lastRun(b: Agent): string {
  const r = b.lastRun
  if (!r) return 'Has not run yet'
  if (r.endedAt === null) return `Running since ${time(r.startedAt)}`
  return [
    `Last ran ${time(r.startedAt)} for ${minutes(r.endedAt - r.startedAt)}`,
    r.costUsd !== null ? `$${r.costUsd.toFixed(2)}` : null,
    r.error ? `failed: ${r.error}` : null,
  ]
    .filter(Boolean)
    .join(' · ')
}

/**
 * The top of a bot's page: where it is (when it wakes, counting down), its last
 * run, and its settings (folded away until wanted; saving them wakes it; Delete
 * is in there too, out of the way). Its own chat is below this, as any task's is.
 */
/** `mobile`: its line of state and Wake now stack, for a phone's width. */
export function BotView({ bot, act, mobile = false }: { bot: Agent | null; act: Act; mobile?: boolean }) {
  const [draft, setDraft] = useState<BotDraft | null>(bot && draftOf(bot))
  const [open, setOpen] = useState(false)
  // The countdown to its next wake moves on by the second.
  const [now, setNow] = useState(Date.now())
  useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 1000)
    return () => clearInterval(t)
  }, [])
  if (!bot || !draft) return <p className="muted">This bot was deleted.</p>
  const changed = JSON.stringify(draft) !== JSON.stringify(draftOf(bot))
  return (
    <div className={`bot-top ${mobile ? 'mobile' : ''}`}>
      <div className="bot-line">
        <button data-ui className="link" onClick={() => setOpen(!open)} aria-expanded={open}>
          {open ? <ChevronDown className="icon" /> : <ChevronRight className="icon" />} Settings
        </button>
        <span className="muted">
          {bot.model} · waits {bot.debounceMinutes}m · {botStatus(bot, now)}
        </span>
        <button data-ui disabled={!!bot.runId} onClick={() => void act(() => api().wakeBot(bot.id))} title="Run now, whatever the debounce says">
          Wake now
        </button>
      </div>
      <p className="muted bot-last-run" title={bot.lastRun?.output ?? undefined}>
        {lastRun(bot)}
      </p>
      {open && (
        <section className="card">
          <Form schema={BOT_FORM} value={draft} setValue={(v) => setDraft({ ...draft, ...(v as BotDraft) })} />
          <div className="buttons">
            <button data-ui className="primary" disabled={!changed || !draft.name.trim()} onClick={() => void act(() => api().saveBot({ ...draft, id: bot.id }))}>
              Save
            </button>
            <button data-ui disabled={!changed} onClick={() => setDraft(draftOf(bot))}>
              Revert
            </button>
            <button
              data-ui
              className="delete"
              onClick={() => window.confirm(`Delete @${bot.name}? Its tasks go back to their owners.`) && void act(() => api().cancel(bot.id))}
            >
              Delete
            </button>
          </div>
        </section>
      )}
    </div>
  )
}
