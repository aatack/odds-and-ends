// The event types. What each takes and resolves with is the contract
// docs/events.md sets out; keep the two in step.

import { z } from 'zod'
import type { EventSpec } from '../shared/types'
import { checkPr, githubEventParams, startPr } from './github'
import type { EventType } from './integrations'
import { runScript } from './script'
import { checkSlack, slackEventParams, startSlack } from './slack'

const timeout = { timeout_minutes: z.number().optional().describe('Resolve as timed_out after this long.') }

export const message: EventType = {
  type: 'message',
  description: 'Something said in the item: by the human, a bot, or a source.',
  params: { text: z.string() },
  data: '{}',
  blocking: false,
}

export const action: EventType = {
  type: 'action',
  description: 'An action run on the item, with what it was given and what it answered.',
  params: { name: z.string(), inputs: z.record(z.unknown()), output: z.unknown().optional() },
  data: '{}',
  blocking: false,
}

export const closed: EventType = {
  type: 'closed',
  description: 'The item closed: done, or done outside the tool.',
  params: { how: z.enum(['closed', 'handled_outside']) },
  data: '{}',
  blocking: false,
}

export const approval: EventType = {
  type: 'approval',
  description:
    'An unsafe action for the human to approve, change, or turn down. While only approvals are open, the item is the human\'s turn.',
  params: {
    action: z.string().describe('The unsafe action, by name: see unsafe_actions.'),
    inputs: z.record(z.unknown()).describe('Its inputs, filled in as far as you can. The human may change them.'),
    reason: z.string().optional().describe('Why, in a line.'),
  },
  data: '{ ran: true, inputs, changed: string[], output } or { cancelled: true }',
  blocking: true,
  human: true,
  bots: true,
}

export const bot: EventType = {
  type: 'bot',
  description:
    "A bot to take the item up: it reads what is new in the item's own Claude session, and answers with a message, an approval, more events, or closing it.",
  params: { instructions: z.string().optional().describe('What to do, when there is no message from the human to go on.') },
  data: '{ runId, message?, costUsd?, output } or { error }',
  blocking: true,
  bots: true,
}

export const github: EventType = {
  type: 'github',
  description: 'Wait on a pull request, checked every few minutes.',
  params: githubEventParams,
  data: '{ outcome: "passed" | "failed" | "activity" | "merged" | "closed" | "not_found" | "timed_out", detail, failed? }',
  blocking: true,
  bots: true,
  start: (p, { engine }) => startPr(p, engine.exec),
  check: (p, { engine }) => checkPr(p, engine.exec),
}

export const slack: EventType = {
  type: 'slack',
  description: "Optionally post a message as the operator, then wait for somebody else's reply in its thread.",
  params: slackEventParams,
  data: '{ outcome: "replied" | "timed_out", channel, thread_ts, replies?, detail }',
  blocking: true,
  bots: true,
  expand: (p, author) => {
    if (!p.message || author === 'human') return [{ type: 'slack', params: p }]
    // Posting is unsafe: a bot's message becomes an approval, and the wait follows it.
    const { message: text, ...rest } = p
    const ref = `post-${Math.random().toString(36).slice(2, 10)}`
    return [
      { type: 'approval', ref, params: { action: 'slack_post', inputs: { workspace: p.workspace, channel: p.channel, text, thread_ts: p.thread_ts }, reason: 'Send this Slack message?' } },
      { type: 'after', params: { event: ref, then: [{ type: 'slack', params: { ...rest, thread_of: ref } }] } },
    ]
  },
  start: async (p, { engine, item }) => startSlack(p, engine, item.id),
  check: (p, { engine }) => checkSlack(p, engine),
}

export const items: EventType = {
  type: 'items',
  description: 'Wait for every one of a list of items to close.',
  params: { item_ids: z.array(z.string()).min(1), ...timeout },
  data: '{ outcome: "closed" | "timed_out", items: { id, title }[] }',
  blocking: true,
  bots: true,
  check: async (p, { engine }) => {
    const found = (p.item_ids as string[]).map((id) => engine.get(id))
    if (found.some((i) => !i || !i.closed)) return null
    return { outcome: 'closed', detail: `${found.length} item(s) closed.`, items: found.map((i) => ({ id: i!.id, title: i!.title })) }
  },
}

export const after: EventType = {
  type: 'after',
  description:
    'Wait for another event to resolve, then add events: `then` as given, or what `script` returns. This is how events chain, ' +
    'e.g. wait on CI, then hand to a bot if it failed and leave it with the human if it passed.',
  params: {
    event: z.string().describe("The event to wait for: an id, or the ref of an event added alongside this one."),
    then: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })).optional(),
    script: z
      .string()
      .optional()
      .describe(
        'A JavaScript function expression: async (input, actions) => events. input is { item: { id, title }, event: { type, params, data } }, ' +
          'the event waited for. actions.<name>(args) calls an action; a script a bot wrote gets no unsafe ones. ' +
          'Return an event ({ type, params }), a list of them, or null. console.log is kept.',
      ),
    description: z.string().optional().describe('What it does, in a line.'),
  },
  data: '{ added: string[] (ids), logs?: string[] } or { skipped: true } when the event was cancelled',
  blocking: true,
  bots: true,
  check: async (p, { engine, item, author }) => {
    const waited = engine.event(String(p.event))
    if (!waited) throw new Error(`No event ${p.event}.`)
    if (!waited.resolved) return null
    if (waited.cancelled) return { skipped: true, detail: 'The event it waited for was cancelled.' }
    let next = (p.then as EventSpec[] | undefined) ?? []
    let logs: string[] | undefined
    // A script that fails, or answers events that can't be added, won't do better next time: it resolves with the error.
    try {
      if (p.script) {
        const ran = await runScript(
          String(p.script),
          { item: { id: item.id, title: item.title }, event: { type: waited.type, params: waited.params, data: waited.data } },
          (name, args) => engine.callAction(name, args, { engine, author, actor: 'script', itemId: item.id, runId: null }),
        )
        next = ran.value === null ? [] : Array.isArray(ran.value) ? ran.value : [ran.value as EventSpec]
        logs = ran.logs
      }
      next = engine.prepare(next, author)
    } catch (e) {
      return { error: (e as Error).message, detail: (e as Error).message, ...(logs ? { logs } : {}) }
    }
    const added = engine.add(item.id, next, author === 'bot' ? 'script:bot' : 'script')
    return { added, ...(logs ? { logs } : {}), detail: p.description ? String(p.description) : undefined }
  },
}

export const EVENT_TYPES: EventType[] = [message, action, closed, approval, bot, github, slack, items, after]
