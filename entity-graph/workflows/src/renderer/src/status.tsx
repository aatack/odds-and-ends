// How an item stands, where that says something: a bot working (and for how long, on what), starting soon or waiting
// out a usage limit; a script running or due. A pill shows it as a dot after its name (`StatusDot`); a card, and the
// assignees over a chat's input, as the dot and a line (`StatusLine`). Types with nothing to say have no status:
// a topic's own icon already says whether it is closed.

import { createContext, useContext } from 'react'
import type { Overview } from '../../shared/types'
import { minutes, useNow } from './time'

export type Tone = 'working' | 'soon' | 'paused' | 'waiting'
export type Status = { tone: Tone; text: string }

/** What a status is read from: the overview, given by the app. */
export const OverviewContext = createContext<Overview | null>(null)

/** Within this long of being due counts as starting soon. */
const SOON_MS = 60_000

const clock = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

/** An item's status now, if it has one to show. */
export function statusOf(id: string, o: Overview, now: number): Status | null {
  const bot = o.bots.find((b) => b.id === id)
  if (bot) {
    if (bot.deleted) return null
    if (bot.runId && bot.lastRun) {
      const on = o.items.find((i) => i.bot === bot.id)
      return { tone: 'working', text: `${minutes(now - bot.lastRun.startedAt)}${on ? ` on ${on.title}` : ''}` }
    }
    if (bot.pausedUntil && bot.pausedUntil > now) return { tone: 'paused', text: `Usage limit till ${clock(bot.pausedUntil)}` }
    if (bot.wakesAt !== null) return bot.wakesAt - now <= SOON_MS ? { tone: 'soon', text: 'Starting soon' } : { tone: 'waiting', text: `Starts in ${minutes(bot.wakesAt - now)}` }
    return null
  }
  const script = o.scripts.find((s) => s.id === id)
  if (script) {
    if (script.running) return { tone: 'working', text: 'Running' }
    if (script.wakesAt !== null) return script.wakesAt - now <= SOON_MS ? { tone: 'soon', text: 'Starting soon' } : { tone: 'waiting', text: `Runs in ${minutes(script.wakesAt - now)}` }
  }
  return null
}

/** The tiny dot after a pill's name, while there is something to say (hover for what). */
export function StatusDot({ id }: { id: string }) {
  const o = useContext(OverviewContext)
  const now = useNow()
  const s = o && statusOf(id, o, now)
  return s ? <span className={`status-dot ${s.tone}`} title={s.text} /> : null
}

/** The dot and a line saying it, ticking by the second (for a card, and the assignees over a chat's input). */
export function StatusLine({ id }: { id: string }) {
  const o = useContext(OverviewContext)
  const now = useNow()
  const s = o && statusOf(id, o, now)
  if (!s) return null
  return (
    <span className="status-line">
      <span className={`status-dot ${s.tone}`} />
      {s.text}
    </span>
  )
}
