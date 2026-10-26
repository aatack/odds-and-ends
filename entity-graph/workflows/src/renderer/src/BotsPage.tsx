import { Bot } from 'lucide-react'
import { useState } from 'react'
import type { Agent } from '../../shared/types'
import { minutes } from './Chat'
import { Form } from './forms'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** A bot's settings, as WorkflowsAPI.saveBot takes them. */
export type BotDraft = { name: string; instructions: string; provider: string; model: string; debounceMinutes: number; folder?: string }

/** All a bot's settings, on its own page. */
export const BOT_FORM = {
  type: 'object',
  properties: {
    name: { type: 'string', title: 'Name', description: 'People @ it by this: letters, digits, dots, dashes and underscores.' },
    instructions: {
      type: 'string',
      title: 'Instructions',
      'x-multiline': true,
      description: 'What it is for and how it works: its system prompt, after the app’s own.',
    },
    model: { type: 'string', title: 'Model', description: 'opus, sonnet, haiku, or a full model id.' },
    debounceMinutes: {
      type: 'number',
      title: 'Debounce (minutes)',
      description: 'How long it waits after the last thing that happened to it before it wakes: a burst of work goes in one run.',
    },
    folder: { type: 'string', title: 'Folder', description: 'Where it works: a folder of its own beside the source, unless you give another.' },
    provider: { type: 'string', title: 'Provider', enum: ['claude'], description: 'Who runs it.' },
  },
  required: ['name'],
}

/** What a new bot needs: a name, and what it is for. The rest has defaults, shown (and changed) on its page. */
const NEW_BOT_FORM = {
  type: 'object',
  properties: { name: BOT_FORM.properties.name, instructions: BOT_FORM.properties.instructions },
  required: ['name'],
}

export const NEW_BOT: BotDraft = { name: '', instructions: '', provider: 'claude', model: 'opus', debounceMinutes: 1 }

/** Where a bot is: running, due to wake (and when), or idle; and how much it has. */
export function botStatus(b: Agent, now = Date.now()): string {
  const has = b.assigned.length ? `${b.assigned.length} task${b.assigned.length === 1 ? '' : 's'}` : 'nothing'
  if (b.runId) return `working · has ${has}`
  if (b.wakesAt !== null) return b.wakesAt <= now ? `waking · has ${has}` : `wakes in ${minutes(b.wakesAt - now)} · has ${has}`
  return `idle · has ${has}`
}

/**
 * The bots: each a name to @, its own instructions, model and debounce, and the
 * tasks it has. Each opens as its own page; a new one is made below.
 */
export function BotsPage({ bots, act, onOpen }: { bots: Agent[]; act: Act; onOpen: (id: string) => void }) {
  const [draft, setDraft] = useState<BotDraft>(NEW_BOT)
  const add = () =>
    act(async () => {
      const id = await api().saveBot(draft)
      setDraft(NEW_BOT)
      onOpen(id)
    })
  return (
    <div className="page bots-page">
      <section className="card">
        <div className="list">
          {bots.map((b) => (
            <div key={b.id} className="row" onClick={() => onOpen(b.id)}>
              <Bot className="icon" />
              <span className="row-title">@{b.name}</span>
              <span className="muted row-status">
                {b.model || 'default model'}
                {b.debounceMinutes ? ` · waits ${b.debounceMinutes}m` : ''}
              </span>
              <span className="muted">{botStatus(b)}</span>
            </div>
          ))}
          {!bots.length && <p className="muted">No bots yet.</p>}
        </div>
      </section>
      <section className="card add">
        <h2>New bot</h2>
        <Form schema={NEW_BOT_FORM} value={draft} setValue={(v) => setDraft({ ...draft, ...(v as BotDraft) })} />
        <p className="muted">It runs on opus, waits a minute after anything happens to it, and works in a folder of its own: change those on its page.</p>
        <button data-ui className="primary" disabled={!draft.name.trim()} onClick={() => void add()}>
          Make it
        </button>
      </section>
    </div>
  )
}
