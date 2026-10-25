import { useCallback, useEffect, useState } from 'react'
import type { Agent, Assignment, EventDetail, Item } from '../../shared/types'
import { minutes, time } from './Chat'
import { BOT_FORM, botStatus, type BotDraft } from './BotsPage'
import { Form } from './forms'
import { ItemRows } from './ItemRows'
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
 * A bot, on its own page: its settings (saving adds a configure event with what
 * changed), what it has and watches, what it was handed lately, and its runs.
 */
export function BotView(props: { detail: EventDetail; bot: Agent | null; items: Item[]; act: Act; onOpen: (id: string) => void; onAssigned: (to: string) => void }) {
  const { detail, bot, items, act, onOpen, onAssigned } = props
  const [draft, setDraft] = useState<BotDraft | null>(bot && draftOf(bot))
  const [recent, setRecent] = useState<Assignment[]>([])
  const refresh = useCallback(async () => setRecent(await api().assignments(detail.event.id, 7)), [detail.event.id])
  useEffect(() => {
    void refresh()
    return api().onChange(() => void refresh())
  }, [refresh])
  if (!bot || !draft) return <p className="muted">This bot was deleted.</p>
  const byId = new Map(items.map((i) => [i.id, i]))
  const changed = JSON.stringify(draft) !== JSON.stringify(draftOf(bot))
  return (
    <div className="page bot-view">
      <p className="muted">{botStatus(bot)}</p>
      <section className="card">
        <h2>Has</h2>
        <ItemRows items={bot.assigned.flatMap((id) => byId.get(id) ?? [])} onOpen={onOpen} empty="Nothing: @ it in a task to hand it one." />
        <div className="buttons">
          <button data-ui disabled={!!bot.runId || !bot.assigned.length} onClick={() => void act(() => api().wakeBot(bot.id))} title="Run now, whatever the debounce says">
            Wake now
          </button>
          <button data-ui onClick={() => onAssigned(bot.id)}>
            Handed lately
          </button>
        </div>
      </section>
      <section className="card">
        <h2>Settings</h2>
        <Form schema={BOT_FORM} value={draft} setValue={(v) => setDraft({ ...draft, ...(v as BotDraft) })} />
        <div className="buttons">
          <button data-ui className="primary" disabled={!changed || !draft.name.trim()} onClick={() => void act(() => api().saveBot({ ...draft, id: bot.id }))}>
            Save
          </button>
          <button data-ui disabled={!changed} onClick={() => setDraft(draftOf(bot))}>
            Undo changes
          </button>
          <button
            data-ui
            className="text-error-600"
            title="What it has goes back to you"
            onClick={() => window.confirm(`Delete @${bot.name}? What it has goes back to you.`) && void act(() => api().deleteBot(bot.id))}
          >
            Delete
          </button>
        </div>
      </section>
      {recent.length > 0 && (
        <section className="card">
          <h2>Handed to it this week</h2>
          <AssignmentRows rows={recent} onOpen={onOpen} />
        </section>
      )}
      <section className="card">
        <h2>Runs</h2>
        {!detail.runs.length && <p className="muted">None yet.</p>}
        <div className="list">
          {[...detail.runs].reverse().slice(0, 30).map((r) => (
            <div key={r.id} className="row" title={r.result ?? ''}>
              <span className="row-title muted">{time(r.startedAt)}</span>
              <span className="muted row-status">
                {r.itemIds.length} task{r.itemIds.length === 1 ? '' : 's'}
                {r.endedAt ? ` · ${minutes(r.endedAt - r.startedAt)}` : ' · running'}
                {r.costUsd !== null ? ` · $${r.costUsd.toFixed(2)}` : ''}
              </span>
            </div>
          ))}
        </div>
      </section>
    </div>
  )
}

/** Assign events, a row each: when, the task, and whether it was handed over or taken off. */
export function AssignmentRows({ rows, onOpen, who }: { rows: Assignment[]; onOpen: (id: string) => void; who?: (id: string) => string }) {
  return (
    <div className="list">
      {rows.map((a, i) => (
        <div key={`${a.itemId}:${a.at}:${i}`} className="row" onClick={() => onOpen(a.itemId)}>
          <span className="row-title">{a.title}</span>
          <span className="muted row-status">
            {who ? `${who(a.assignee)} · ` : ''}
            {a.on ? 'handed over' : 'taken off'}
          </span>
          <span className="muted row-age" title={time(a.at)}>
            {minutes(Math.max(0, Date.now() - a.at))} ago
          </span>
        </div>
      ))}
    </div>
  )
}
