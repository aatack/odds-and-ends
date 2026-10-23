// Slack, as the operator, through the user tokens on the Integrations page.

import { z } from 'zod'
import type { Engine } from './engine'
import type { Action } from './integrations'
import type { SlackWorkspace } from './settings'

const workspace = z.string().optional().describe('The workspace: its name or team. May be left out when there is only one.')

export function slackWorkspace(engine: Engine, which: unknown): SlackWorkspace {
  const all = engine.settings.slackWorkspaces()
  if (!all.length) throw new Error('No Slack workspace is set up: add one on the Integrations page.')
  if (which === undefined || which === '') {
    if (all.length > 1) throw new Error(`Name the workspace: one of ${all.map((w) => w.name).join(', ')}.`)
    return all[0]
  }
  const found = all.find((w) => [w.id, w.name, w.team].includes(String(which)))
  if (!found) throw new Error(`No Slack workspace ${which}: one of ${all.map((w) => w.name).join(', ')}.`)
  return found
}

/** Calls one Slack Web API method, failing with Slack's own error. */
export async function slackCall(
  engine: Engine,
  which: unknown,
  method: string,
  args: Record<string, unknown>,
  post = false,
): Promise<Record<string, unknown>> {
  const w = slackWorkspace(engine, which)
  const query = new URLSearchParams(Object.entries(args).filter(([, v]) => v !== undefined).map(([k, v]) => [k, String(v)] as [string, string]))
  const res = await engine.fetch(post ? `https://slack.com/api/${method}` : `https://slack.com/api/${method}?${query}`, {
    method: post ? 'POST' : 'GET',
    headers: { Authorization: `Bearer ${w.token}`, ...(post ? { 'Content-Type': 'application/json; charset=utf-8' } : {}) },
    ...(post ? { body: JSON.stringify(args) } : {}),
  })
  const body = (await res.json()) as Record<string, unknown>
  if (!body.ok) throw new Error(`Slack ${method}: ${body.error ?? res.status}`)
  return body
}

type Message = { user?: string; text?: string; ts: string; thread_ts?: string; reply_count?: number }
const brief = (m: Message) => ({ user: m.user, text: m.text, ts: m.ts, thread_ts: m.thread_ts, replies: m.reply_count })

export const slackActions: Action[] = [
  {
    name: 'slack_workspaces',
    unsafe: false,
    description: 'The Slack workspaces set up, and who the operator is in each.',
    input: {},
    run: async (_a, { engine }) => engine.settings.slackWorkspaces().map(({ token: _, ...w }) => w),
  },
  {
    name: 'slack_channel_history',
    unsafe: false,
    description: 'The latest messages in a channel or DM, newest first.',
    input: { workspace, channel: z.string().describe('The channel id, e.g. C0123.'), limit: z.number().int().default(30) },
    run: async (a, { engine }) =>
      ((await slackCall(engine, a.workspace, 'conversations.history', { channel: a.channel, limit: a.limit })).messages as Message[]).map(brief),
  },
  {
    name: 'slack_thread',
    unsafe: false,
    description: 'A thread: its first message and every reply.',
    input: { workspace, channel: z.string(), ts: z.string().describe("The thread's first message's ts.") },
    run: async (a, { engine }) =>
      ((await slackCall(engine, a.workspace, 'conversations.replies', { channel: a.channel, ts: a.ts })).messages as Message[]).map(brief),
  },
  {
    name: 'slack_search',
    unsafe: false,
    description: "Search messages with Slack's search syntax, e.g. \"from:@sam in:#eng deploy\". Needs the search:read scope.",
    input: { workspace, query: z.string(), count: z.number().int().default(20) },
    run: async (a, { engine }) => {
      const r = await slackCall(engine, a.workspace, 'search.messages', { query: a.query, count: a.count })
      return ((r.messages as { matches?: (Message & { channel?: { id: string; name: string }; permalink?: string })[] }).matches ?? []).map(
        (m) => ({ ...brief(m), channel: m.channel, permalink: m.permalink }),
      )
    },
  },
  {
    name: 'slack_post',
    unsafe: true,
    description: 'Post a message as the operator, in a channel or a thread. Answers its channel and ts.',
    input: {
      workspace,
      channel: z.string().describe('The channel id.'),
      text: z.string().describe('The message, in Slack mrkdwn.'),
      thread_ts: z.string().optional().describe('Reply in this thread.'),
    },
    run: async (a, { engine }) => {
      const r = await slackCall(engine, a.workspace, 'chat.postMessage', { channel: a.channel, text: a.text, thread_ts: a.thread_ts }, true)
      return { channel: r.channel, ts: r.ts }
    },
  },
]

export const slackEventParams = {
  workspace,
  channel: z.string().describe('The channel id.'),
  message: z
    .string()
    .optional()
    .describe('Posted as the operator as the wait starts. One a bot sets gets an approval before it instead.'),
  thread_ts: z.string().optional().describe('The thread to wait in. Left out, it is the message just posted.'),
  thread_of: z.string().optional().describe('An approval event whose slack_post started the thread to wait in.'),
  timeout_minutes: z.number().optional().describe('Resolve as timed_out after this long.'),
}

/** Posts the message if there is one, and remembers what the thread already holds. */
export async function startSlack(p: Record<string, unknown>, engine: Engine, itemId: string) {
  let thread = p.thread_ts as string | undefined
  if (p.message) {
    const posted = await slackCall(engine, p.workspace, 'chat.postMessage', { channel: p.channel, text: p.message, thread_ts: thread }, true)
    thread ??= posted.ts as string
    engine.mark(itemId, 'action_called', { actor: 'human', data: { action: 'slack_post', ok: true, output: { channel: posted.channel, ts: posted.ts } } })
  }
  // An approved slack_post started the thread.
  if (!thread && p.thread_of) thread = ((engine.event(String(p.thread_of))?.data?.output ?? {}) as { ts?: string }).ts
  if (!thread) throw new Error('A Slack wait needs a message to post, a thread_ts, or an approved slack_post to follow.')
  const messages = (await slackCall(engine, p.workspace, 'conversations.replies', { channel: p.channel, ts: thread })).messages as Message[]
  return { params: { ...p, message: undefined, thread_ts: thread, since: messages.at(-1)?.ts ?? thread } }
}

/** Resolved by the first reply from anybody but the operator. */
export async function checkSlack(p: Record<string, unknown>, engine: Engine): Promise<Record<string, unknown> | null> {
  const me = slackWorkspace(engine, p.workspace).userId
  const messages = (await slackCall(engine, p.workspace, 'conversations.replies', { channel: p.channel, ts: p.thread_ts })).messages as Message[]
  const replies = messages.filter((m) => Number(m.ts) > Number(p.since) && m.user !== me)
  return replies.length
    ? { outcome: 'replied', channel: p.channel, thread_ts: p.thread_ts, replies: replies.map(brief), detail: `${replies.length} reply(s) on Slack: ${replies.map((r) => r.text).join(' / ').slice(0, 200)}` }
    : null
}
