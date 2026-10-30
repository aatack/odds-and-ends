// What the chat input offers with nothing typed: a curated list, in order, each built from the task as it is. Tab
// steps through them and Enter runs the one shown. Add to `DEFAULT_ACTIONS` to offer more.

import type { ItemDetail, Overview, PresetInfo } from '../../shared/types'
import { SLACK_LINK } from '../../shared/slack'
import { api } from './useWorkflows'

/** One choice: how the input shows it, and what Enter does (true once done). */
export interface DefaultAction extends PresetInfo {
  run: () => Promise<unknown>
}

type Ctx = { detail: ItemDetail; overview: Overview }

/** The script with this name, if there is one: the default actions hand topics to scripts by name. */
const scriptCalled = (overview: Overview, name: string) => overview.scripts.find((s) => s.named && s.name.toLowerCase() === name.toLowerCase())

/** A snooze: hands the topic to the script called Snooze, with how long ("[id]() 1h"). Offered only if there is one. */
const snooze = (label: string, short: string) => ({ detail, overview }: Ctx): DefaultAction[] => {
  const s = scriptCalled(overview, 'Snooze')
  if (!s) return []
  return [{ id: `snooze:${short}`, name: `Snooze for ${label}`, hint: `@${s.name} gives this topic back in ${label}`, icon: 'script', run: () => api().send(detail.item.id, 'note', `[${s.id}]() ${short}`) }]
}

/** Assign to the bot that last worked on it: the last to write in it, else the one the person handed something to last. */
const assignToLastBot = ({ detail, overview }: Ctx): DefaultAction[] => {
  const wrote = detail.children.map((e) => e.createdBy).reverse().find((by) => by.startsWith('bot:'))?.slice(4)
  const id = wrote ?? overview.recentAssignees.find((a) => overview.bots.some((b) => b.id === a))
  const bot = overview.bots.find((b) => b.id === id && !b.deleted)
  if (!bot) return []
  return [{ id: `assign:${bot.id}`, name: `Assign to @${bot.name}`, hint: `hand this task to @${bot.name}`, icon: 'bot', run: () => api().send(detail.item.id, `assign:${bot.id}`, '') }]
}

/** Wait for Slack: hands the topic to the script called "Slack wait", with the message to watch; one for each recent Slack message. */
const waitForSlack = ({ detail, overview }: Ctx): DefaultAction[] => {
  const s = scriptCalled(overview, 'Slack wait')
  if (!s) return []
  const links: string[] = []
  for (const e of detail.children.slice(-20).reverse()) {
    const found =
      e.type === 'slack_message' && e.params.link
        ? [String(e.params.link)]
        : e.type === 'note'
          ? [...String(e.params.text ?? '').matchAll(new RegExp(SLACK_LINK, 'g'))].map((m) => m[0])
          : []
    for (const link of found) if (!links.includes(link)) links.push(link)
  }
  return links.map((link, i) => ({
    id: `slack-wait:${link}`,
    name: `Wait for Slack${links.length > 1 ? ` (${i + 1})` : ''}`,
    hint: `@${s.name} gives this topic back when something new appears after ${link}`,
    icon: 'slack_message',
    run: () => api().send(detail.item.id, 'note', `[${s.id}]() ${link}`),
  }))
}

/** The default actions, in the order they are offered. */
export const DEFAULT_ACTIONS: ((ctx: Ctx) => DefaultAction[])[] = [assignToLastBot, waitForSlack, snooze('an hour', '1h'), snooze('a day', '1d')]

/** What the chat input offers for this task, with nothing typed. None in a bot's own chat. */
export function defaultActions(detail: ItemDetail, overview: Overview): DefaultAction[] {
  if (overview.bots.some((b) => b.id === detail.item.id)) return []
  return DEFAULT_ACTIONS.flatMap((f) => f({ detail, overview }))
}
