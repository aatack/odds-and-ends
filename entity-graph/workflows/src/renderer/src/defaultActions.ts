// What the chat input offers with nothing typed: a curated list, in order, each built from the task as it is. Tab
// steps through them and Enter runs the one shown. Add to `DEFAULT_ACTIONS` to offer more.

import type { EventSpec, ItemDetail, Overview, PresetInfo } from '../../shared/types'
import { SLACK_LINK, slackWait } from '../../shared/slack'
import { api } from './useWorkflows'

/** One choice: how the input shows it, and what Enter does (true once done). */
export interface DefaultAction extends PresetInfo {
  run: () => Promise<unknown>
}

type Ctx = { detail: ItemDetail; overview: Overview }

/**
 * A snooze: a script has the task for `ms`, then gives it back to the person, who lets go of it now. It wakes every five
 * minutes (and when anything happens in the task) and checks the time: scripts don't sleep until a time.
 */
const snooze = (label: string, ms: number) => ({ detail, overview }: Ctx): DefaultAction[] => [
  {
    id: `snooze:${ms}`,
    name: `Snooze for ${label}`,
    hint: `this task comes back to you in ${label}`,
    icon: 'script',
    run: () =>
      takeOff(detail, overview, {
        type: 'script',
        params: {
          description: `Snooze for ${label}`,
          owns: [detail.item.id],
          cycle_minutes: 5,
          source: `if (context.now < ${JSON.stringify(new Date(Date.now() + ms).toISOString())}) return null\nreturn { messages: [{ text: 'Back from snooze' }], assign: context.tasks.map((t) => ({ item: t.id, to: [${JSON.stringify(overview.me.id)}] })), done: true }`,
        },
      }),
  },
]

/** Adds a script that owns the task, and takes the person off it: the script has it now. */
async function takeOff(detail: ItemDetail, overview: Overview, script: EventSpec): Promise<void> {
  await api().addEvents(detail.item.id, [script])
  if (detail.item.assignees.includes(overview.me.id)) await api().assign(detail.item.id, overview.me.id, false)
}

/** Assign to the bot that last worked on it: the last to write in it, else the one the person handed something to last. */
const assignToLastBot = ({ detail, overview }: Ctx): DefaultAction[] => {
  const wrote = detail.events.map((e) => e.createdBy).reverse().find((by) => by.startsWith('bot:'))?.slice(4)
  const id = wrote ?? overview.recentAssignees.find((a) => overview.bots.some((b) => b.id === a))
  const bot = overview.bots.find((b) => b.id === id && !b.deleted)
  if (!bot) return []
  return [{ id: `assign:${bot.id}`, name: `Assign to @${bot.name}`, hint: `hand this task to @${bot.name}`, icon: 'bot', run: () => api().send(detail.item.id, `assign:${bot.id}`, '') }]
}

/** Wait for Slack: one for each Slack message in the last 20 events (pulled in, or linked in a note). */
const waitForSlack = ({ detail, overview }: Ctx): DefaultAction[] => {
  const links: string[] = []
  for (const e of detail.events.slice(-20).reverse()) {
    const found =
      e.type === 'slack_message' && e.params.link
        ? [String(e.params.link)]
        : e.type === 'note'
          ? [...String(e.params.text ?? '').matchAll(new RegExp(SLACK_LINK, 'g'))].map((m) => m[0])
          : []
    for (const link of found) if (!links.includes(link)) links.push(link)
  }
  return links.map((link, i) => {
    const where = link.replace(/^https:\/\/([\w-]+)\.slack\.com\/archives\/([A-Z0-9]+)\/p(\d+).*$/, '$1 $2 · $3')
    return {
      id: `slack-wait:${link}`,
      name: `Wait for Slack${links.length > 1 ? ` (${i + 1})` : ''}`,
      hint: `this task comes back when there is something new after ${where}: checked every five minutes`,
      icon: 'slack_message',
      run: () =>
        takeOff(detail, overview, {
          type: 'script',
          params: { description: 'Wait for something new on Slack', owns: [detail.item.id], cycle_minutes: 5, source: slackWait(link, overview.me.id) },
        }),
    }
  })
}

/** The default actions, in the order they are offered. */
export const DEFAULT_ACTIONS: ((ctx: Ctx) => DefaultAction[])[] = [assignToLastBot, waitForSlack, snooze('an hour', 3_600_000), snooze('a day', 86_400_000)]

/** What the chat input offers for this task, with nothing typed. None in a bot's own chat. */
export function defaultActions(detail: ItemDetail, overview: Overview): DefaultAction[] {
  if (overview.bots.some((b) => b.id === detail.item.id)) return []
  return DEFAULT_ACTIONS.flatMap((f) => f({ detail, overview }))
}
