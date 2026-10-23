// The handoff types. What each takes and what it resolves with is the contract
// docs/handoffs.md sets out; keep the two in step.

import { z } from 'zod'
import type { HandoffSpec } from '../shared/types'
import { checkPr, githubHandoffParams, startPr } from './github'
import type { HandoffType } from './integrations'
import { runScript } from './script'
import { checkSlack, slackHandoffParams, startSlack } from './slack'

export const human: HandoffType = {
  type: 'human',
  description:
    'The human. Hand off here only for what nobody else can do: a decision, information only they have, or approval of an outward action.',
  params: {
    needed: z
      .string()
      .describe(
        'Exactly what is needed from the human, as one direct question or request they can act on at once. ' +
          'No preamble, no summary of the work, no options they did not ask for: the state already says where things are.',
      ),
    approval: z
      .object({
        action: z.string().describe('The outward action to run, by name.'),
        inputs: z.record(z.unknown()).describe("The action's inputs, filled in as far as you can. The human may change them."),
      })
      .optional()
      .describe('An outward action for the human to approve, change, or turn down.'),
  },
  resolution:
    '{ decision: "handed_off" | "approved" | "closed" | "handled_outside", note?: string, action?: { name, proposed, inputs, changed, output } }',
}

export const bot: HandoffType = {
  type: 'bot',
  description: 'A worker: claude -p with this MCP server. It works the item and returns the next handoffs.',
  params: { instructions: z.string().describe('What the bot is to do: the goal, and anything it needs to know that the state does not say.') },
  resolution: '{ state: string, output: string (what the bot said on the way, which nobody reads by default), runId, costUsd? }',
}

export const github: HandoffType = {
  type: 'github',
  description: 'Wait on a pull request, checked every few minutes.',
  params: githubHandoffParams,
  resolution: '{ outcome: "passed" | "failed" | "activity" | "merged" | "closed" | "timed_out", detail: string, failed?: string[] (the failed checks) }',
  start: (p, { engine }) => startPr(p, engine.exec),
  check: (p, { engine }) => checkPr(p, engine.exec),
}

export const slack: HandoffType = {
  type: 'slack',
  description: "Optionally post a message as the operator, then wait for somebody else's reply in its thread.",
  params: slackHandoffParams,
  resolution: '{ outcome: "replied" | "timed_out", channel, thread_ts, replies?: { user, text, ts }[], detail }',
  expand: (p, author) => {
    if (!p.message || author === 'human') return [{ type: 'slack', params: p }]
    // A bot's message is outward: a human approves it first, and the wait
    // then takes the thread from what was posted.
    const { message, ...rest } = p
    return [
      {
        type: 'human',
        params: {
          needed: 'Send this Slack message?',
          approval: { action: 'slack_post', inputs: { workspace: p.workspace, channel: p.channel, text: message, thread_ts: p.thread_ts } },
        },
      },
      { type: 'slack', params: rest },
    ]
  },
  start: (p, { engine, item, previous }) => startSlack(p, engine, item.id, previous?.resolution ?? null),
  check: (p, { engine }) => checkSlack(p, engine),
}

export const items: HandoffType = {
  type: 'items',
  description: 'Wait for every one of a list of items to close.',
  params: { item_ids: z.array(z.string()).min(1) },
  resolution: '{ outcome: "closed" | "timed_out", items: { id, title, state }[] (each final state) }',
  check: async (p, { engine }) => {
    const found = (p.item_ids as string[]).map((id) => engine.get(id))
    if (found.some((i) => !i || !i.closed)) return null
    return { outcome: 'closed', items: found.map((i) => ({ id: i!.id, title: i!.title, state: i!.state })) }
  },
}

export const script: HandoffType = {
  type: 'script',
  description:
    'Route by what came before. Runs at once, in a sandbox, and puts what it returns at the front of the queue. ' +
    'The usual use is after a wait: e.g. after a github handoff, hand to a bot if checks failed and to the human if they passed.',
  params: {
    source: z
      .string()
      .describe(
        'A JavaScript function expression: async (input, actions) => handoffs. ' +
          'input is { item: { id, title, state }, previous: { type, params, resolution } | null, history: the item\'s handoffs so far }. ' +
          'actions.<name>(args) calls any action and resolves to its result; an outward one only runs if a human wrote the script. ' +
          'Return a handoff ({ type, params }), a list of them, or null to carry on with the queue. console.log is kept.',
      ),
    description: z.string().optional().describe('What it does, in a line, for the history.'),
  },
  resolution: '{ next: handoffs it returned, logs: string[] }',
  run: async (p, { engine, item, previous, author }) => {
    const input = {
      item: { id: item.id, title: item.title, state: item.state },
      previous: previous && { type: previous.type, params: previous.params, resolution: previous.resolution },
      history: engine.handoffs(item.id).filter((h) => h.status === 'resolved').map((h) => ({ type: h.type, params: h.params, resolution: h.resolution })),
    }
    const { value, logs } = await runScript(String(p.source), input, (name, args) =>
      engine.callAction(name, args, { engine, author, actor: 'script', itemId: item.id, runId: null }),
    )
    const next: HandoffSpec[] = value === null ? [] : Array.isArray(value) ? value : [value as HandoffSpec]
    return { resolution: { next, logs }, next }
  },
}

export const HANDOFF_TYPES: HandoffType[] = [human, bot, github, slack, items, script]
