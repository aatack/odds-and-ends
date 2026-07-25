import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { requireEnv } from '../env'
import { postForm } from './http'

// Slack, through the Web API. There is deliberately one notion of "where a
// message is": a conversation id. A DM, a group DM, a private channel and a
// public channel are all conversations and all read and written the same way, so
// there is no tool-per-kind here and nothing to choose between. Threads are the
// one place Slack does differ, and even that is folded in — a reference to a
// message carries the thread it sits in, so replying to it lands in the thread
// without being asked to.

const API = 'https://slack.com/api'

interface SlackResponse {
  ok: boolean
  error?: string
  warning?: string
}

/** One Web API call. Every method takes a form body, so one shape covers them all. */
async function slack<T extends SlackResponse>(
  method: string,
  params: Record<string, string | number | boolean | undefined>,
): Promise<T> {
  const token = requireEnv('SLACK_TOKEN', 'SLACK_USER_TOKEN', 'SLACK_BOT_TOKEN')
  const res = await postForm<T>(`${API}/${method}`, params, {
    Authorization: `Bearer ${token}`,
  })
  if (!res.ok) throw new Error(`Slack ${method} failed: ${res.error ?? 'unknown error'}`)
  return res
}

/**
 * Where something is in Slack. The channel is the only part always present:
 * a message adds its own timestamp, and a message in a thread adds the thread's.
 */
export interface SlackRef {
  channel: string
  ts?: string
  threadTs?: string
}

const ARCHIVE = /\/archives\/([A-Z0-9]+)(?:\/p(\d{10})(\d{6,}))?/i
const CHANNEL_AND_TS = /^([^:/\s]+)[:/](\d{10}\.\d{6,})$/

/**
 * Read a reference to a conversation or a message. Accepts what Slack's own
 * "Copy link" gives you, the raw `<channel>:<timestamp>` pair, or a bare
 * conversation id / `#name`.
 */
export function parseRef(reference: string): SlackRef {
  const ref = reference.trim()
  if (/^https?:\/\//i.test(ref)) {
    let url: URL
    try {
      url = new URL(ref)
    } catch {
      throw new Error(`"${reference}" is not a valid URL`)
    }
    const match = ARCHIVE.exec(url.pathname)
    if (!match) {
      throw new Error(
        `"${reference}" isn't a Slack link — expected one like https://…/archives/C0123ABCD/p1712345678000100`,
      )
    }
    const threadTs = url.searchParams.get('thread_ts') ?? undefined
    return {
      channel: match[1],
      ...(match[2] ? { ts: `${match[2]}.${match[3]}` } : {}),
      ...(threadTs ? { threadTs } : {}),
    }
  }
  const pair = CHANNEL_AND_TS.exec(ref)
  if (pair) return { channel: pair[1], ts: pair[2] }
  return { channel: ref }
}

interface SlackMessage {
  ts: string
  thread_ts?: string
  user?: string
  bot_id?: string
  username?: string
  text?: string
  reply_count?: number
  subtype?: string
}

const slim = (m: SlackMessage): Record<string, unknown> => ({
  ts: m.ts,
  user: m.user ?? m.bot_id ?? null,
  text: m.text ?? '',
})

/**
 * The messages around `ts`. `conversations.replies` is the uniform door: given a
 * thread parent it returns the thread, given a reply it returns the thread it is
 * in, and given a message that was never threaded it returns just that message.
 * `conversations.history` is the fallback for the workspaces where it isn't.
 */
async function messagesAt(channel: string, ts: string): Promise<SlackMessage[]> {
  try {
    const replies = await slack<SlackResponse & { messages?: SlackMessage[] }>(
      'conversations.replies',
      { channel, ts, limit: 200, inclusive: true },
    )
    if (replies.messages?.length) return replies.messages
  } catch {
    // Not a thread, or this workspace disagrees — fall through to the history.
  }
  const history = await slack<SlackResponse & { messages?: SlackMessage[] }>(
    'conversations.history',
    { channel, latest: ts, oldest: ts, inclusive: true, limit: 1 },
  )
  return history.messages ?? []
}

/** A canonical link to a message. Best-effort: not worth failing a read over. */
const permalinkOf = (channel: string, ts: string): Promise<string | null> =>
  slack<SlackResponse & { permalink?: string }>('chat.getPermalink', {
    channel,
    message_ts: ts,
  })
    .then((r) => r.permalink ?? null)
    .catch(() => null)

const conversation = z
  .string()
  .min(1)
  .describe('Conversation id (C…/D…/G…), #channel, user id, or a Slack link')

export const SLACK_TOOLS: ToolDef[] = [
  {
    id: 'slack.readMessage',
    name: 'Read a Slack message',
    description:
      'The text of one message — in a DM, a group, a channel, or a thread, all the same. Give the link from Slack’s “Copy link”, or `<conversation id>:<timestamp>`.',
    safety: 'dangerous',
    args: z.object({
      message: conversation.describe(
        'Slack message link, or C0123ABCD:1712345678.000100',
      ),
      includeThread: z
        .boolean()
        .default(false)
        .describe('Also return the replies in its thread'),
    }),
    handler: async ({ message, includeThread }) => {
      const ref = parseRef(message)
      if (!ref.ts) {
        throw new Error(
          'That names a conversation, not a message — copy the link to a specific message',
        )
      }
      // A reply's own timestamp is enough to find the thread it belongs to, so
      // the two cases don't need telling apart.
      const messages = await messagesAt(ref.channel, ref.threadTs ?? ref.ts)
      const target = messages.find((m) => m.ts === ref.ts)
      if (!target) throw new Error(`No message at ${ref.ts} in ${ref.channel}`)
      return {
        channel: ref.channel,
        ts: target.ts,
        threadTs: target.thread_ts ?? null,
        user: target.user ?? target.bot_id ?? null,
        userName: target.username ?? null,
        text: target.text ?? '',
        replyCount: target.reply_count ?? 0,
        permalink: await permalinkOf(ref.channel, target.ts),
        ...(includeThread ? { thread: messages.map(slim) } : {}),
      }
    },
  },

  {
    id: 'slack.sendMessage',
    name: 'Send a Slack message',
    description:
      'Post to any conversation — a DM, a group, a channel. Point it at a *message* link instead and the reply lands in that message’s thread.',
    safety: 'dangerous',
    args: z.object({
      channel: conversation,
      text: z.string().min(1).describe('The message, in Slack mrkdwn'),
      threadTs: z
        .string()
        .optional()
        .describe('Reply under this thread; taken from the link when it names a message'),
    }),
    handler: async ({ channel, text, threadTs }) => {
      const ref = parseRef(channel)
      // A link to a message is a reply gesture: the thread it belongs to, or
      // failing that the message itself, becomes the thread to reply under.
      const thread = threadTs ?? ref.threadTs ?? ref.ts
      const posted = await slack<SlackResponse & { channel?: string; ts?: string }>(
        'chat.postMessage',
        { channel: ref.channel, text, thread_ts: thread },
      )
      const at = posted.channel ?? ref.channel
      return {
        channel: at,
        ts: posted.ts ?? null,
        threadTs: thread ?? null,
        permalink: posted.ts ? await permalinkOf(at, posted.ts) : null,
      }
    },
  },
]
