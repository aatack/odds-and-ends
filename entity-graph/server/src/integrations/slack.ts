import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { optionalEnv, requireEnv } from '../env'
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
  /** On `missing_scope`: exactly what the call wanted, and what the token has. */
  needed?: string
  provided?: string
}

/**
 * What went wrong, in the terms the fix is described in. Slack names the scope
 * it wanted right there in the response, and a bare "missing_scope" throws that
 * away — along with the two things that are nearly always the actual cause:
 * scopes added under *Bot* rather than *User*, and scopes added but not yet
 * installed, since a scope only takes effect when the app is reinstalled.
 */
export function slackError(method: string, res: SlackResponse): string {
  const why = res.error ?? 'unknown error'
  if (why !== 'missing_scope' || !res.needed) return `Slack ${method} failed: ${why}`
  return [
    `Slack ${method} needs the "${res.needed}" scope.`,
    'Add it under OAuth & Permissions → **User** Token Scopes (not Bot),',
    'then reinstall the app to the workspace — a new scope does nothing until you do,',
    'and reinstalling issues a new token, so copy it into server/.env.',
    `\nThe token currently has: ${res.provided || '(nothing)'}`,
  ].join(' ')
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
  if (!res.ok) throw new Error(slackError(method, res))
  return res
}

/** Slack's own ceiling on a page; asking for more is capped silently. */
const PAGE = 200

interface Paged extends SlackResponse {
  response_metadata?: { next_cursor?: string }
  channels?: unknown[]
  messages?: unknown[]
}

/**
 * Walk a cursor-paged method until `wanted` items are in hand, or Slack runs
 * out. Slack pages by cursor and these tools page by offset, so the walking
 * belongs here rather than being pushed onto whoever is asking.
 */
async function collect<T>(
  method: string,
  params: Record<string, string | number | boolean | undefined>,
  key: 'channels' | 'messages',
  wanted: number,
): Promise<T[]> {
  const out: T[] = []
  let cursor: string | undefined
  do {
    const res = await slack<Paged>(method, {
      ...params,
      limit: Math.min(PAGE, wanted - out.length),
      cursor,
    })
    const batch = (res[key] as T[] | undefined) ?? []
    // A cursor that keeps coming back with nothing behind it would spin forever.
    if (batch.length === 0) break
    out.push(...batch)
    cursor = res.response_metadata?.next_cursor || undefined
  } while (cursor && out.length < wanted)
  return out
}

/**
 * One window of a paged list, in the same shape the GitHub tools use: ask for one
 * past the end, and the extra is what says there's more.
 */
async function windowOf<T>(
  method: string,
  params: Record<string, string | number | boolean | undefined>,
  key: 'channels' | 'messages',
  offset: number,
  limit: number,
): Promise<{ items: T[]; hasMore: boolean }> {
  const end = offset + limit
  const all = await collect<T>(method, params, key, end + 1)
  return { items: all.slice(offset, end), hasMore: all.length > end }
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

/** One message as these tools hand it back, wherever it was read from. */
const summary = (m: SlackMessage): Record<string, unknown> => ({
  ts: m.ts,
  user: m.user ?? m.bot_id ?? null,
  userName: m.username ?? null,
  text: m.text ?? '',
  threadTs: m.thread_ts ?? null,
  replyCount: m.reply_count ?? 0,
})

/**
 * Who the token belongs to. Asked once and remembered: it is the same answer
 * every time, and "not mine" is a filter on every feed.
 */
let identity: Promise<{ id: string; handle: string }> | null = null

function whoAmI(): Promise<{ id: string; handle: string }> {
  identity ??= slack<SlackResponse & { user?: string; user_id?: string }>('auth.test', {})
    .then((r) => ({ id: r.user_id ?? '', handle: r.user ?? '' }))
    .catch((e) => {
      identity = null
      throw e
    })
  return identity
}

/** Display names by user id, remembered — a workspace's people rarely change. */
const displayNames = new Map<string, string>()

async function displayName(id: string): Promise<string> {
  const known = displayNames.get(id)
  if (known) return known
  const res = await slack<
    SlackResponse & {
      user?: { name?: string; profile?: { display_name?: string; real_name?: string } }
    }
  >('users.info', { user: id }).catch(() => null)
  const profile = res?.user?.profile
  const name = profile?.display_name || profile?.real_name || res?.user?.name || id
  displayNames.set(id, name)
  return name
}

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

// --- The recent-messages feed ----------------------------------------------

/** A date `n` days back, as `search` wants it. */
const daysAgo = (n: number): string =>
  new Date(Date.now() - n * 86_400_000).toISOString().slice(0, 10)

/**
 * The query behind the feed. Search takes no bare "everything" — the query is
 * required — but it doesn't have to contain any *text*: a query of modifiers
 * alone is a filter over the lot, and `sort: timestamp` is then what turns "all
 * of it" into "the most recent of it".
 *
 * The date bound is doing two jobs. It keeps the search shallow, and it is a
 * *positive* term, which a query of nothing but `-from:` would lack.
 */
export const recentQuery = (since: string, excludeHandle: string | null): string =>
  excludeHandle ? `after:${since} -from:@${excludeHandle}` : `after:${since}`

interface SearchMatch {
  channel?: { id?: string; name?: string }
  user?: string
  username?: string
  ts?: string
  text?: string
  permalink?: string
}

// --- Conversations ----------------------------------------------------------

interface Conversation {
  id: string
  name?: string
  user?: string
  is_im?: boolean
  is_mpim?: boolean
  is_private?: boolean
  is_archived?: boolean
  topic?: { value?: string }
  purpose?: { value?: string }
}

type ConversationKind = 'dm' | 'group' | 'private' | 'channel'

const kindOf = (c: Conversation): ConversationKind =>
  c.is_im ? 'dm' : c.is_mpim ? 'group' : c.is_private ? 'private' : 'channel'

/**
 * What to call a conversation. Channels and group DMs name themselves; a DM
 * doesn't, and comes back as nothing but the other person's user id — which is
 * useless in a list you are meant to pick from, so it costs one lookup.
 */
async function nameOf(c: Conversation): Promise<string> {
  if (c.is_im) return c.user ? `@${await displayName(c.user)}` : c.id
  if (c.is_mpim) return c.name ?? c.id
  return c.name ? `#${c.name}` : c.id
}

const ALL_KINDS = 'public_channel,private_channel,mpim,im'

// --- What the feed leaves out ----------------------------------------------

/**
 * Search sees every public channel in the workspace, joined or not, and knows
 * nothing about muting — so a feed built on it is noisier than the sidebar it is
 * standing in for. Both are fixed here rather than in the query: there is no
 * `is:member` modifier, and no search modifier for mute at all.
 *
 * Remembered briefly. The feed is the sort of thing you poll, and re-deriving
 * both sets every few seconds would be by far the most expensive part of an
 * otherwise single-call tool.
 */
const FILTER_TTL_MS = 5 * 60 * 1000

export interface FeedFilters {
  /** Conversations you are in. DMs and private channels are always among them. */
  joined: Set<string>
  muted: Set<string>
}

let cachedFilters: { at: number; value: FeedFilters } | null = null

/** A comma-separated list of ids, however it was written. */
export const idSet = (raw: string | undefined): Set<string> =>
  new Set((raw ?? '').split(',').map((s) => s.trim()).filter(Boolean))

/**
 * Whether a match survives. A match Slack didn't say the whereabouts of is kept
 * — discarding something for want of a channel id would be the wrong way round,
 * since the filters exist to remove *known* noise.
 */
export function keepInFeed(
  channelId: string | null | undefined,
  filters: FeedFilters | null,
  keep: { unjoined: boolean; muted: boolean },
): boolean {
  if (!filters || !channelId) return true
  if (!keep.unjoined && !filters.joined.has(channelId)) return false
  if (!keep.muted && filters.muted.has(channelId)) return false
  return true
}

/**
 * Muted conversations. There is no documented way to ask: mute is a user
 * preference, and `users.prefs.get` — what the Slack clients themselves call —
 * is not part of the public API. So this is best-effort by construction. When
 * the call is refused nothing counts as muted, and `SLACK_MUTED` is the way to
 * say so by hand, which works whether or not the endpoint does.
 */
async function mutedConversations(): Promise<Set<string>> {
  const byHand = idSet(optionalEnv('SLACK_MUTED'))
  const prefs = await slack<SlackResponse & { prefs?: { muted_channels?: string } }>(
    'users.prefs.get',
    {},
  ).catch(() => null)
  for (const id of idSet(prefs?.prefs?.muted_channels)) byHand.add(id)
  return byHand
}

async function feedFilters(): Promise<FeedFilters> {
  const now = Date.now()
  if (cachedFilters && now - cachedFilters.at < FILTER_TTL_MS) return cachedFilters.value
  const [joined, muted] = await Promise.all([
    // Not caught: failing here means a missing `*:read` scope, and quietly
    // returning an unfiltered feed would look like the filter simply not working.
    collect<Conversation>(
      'users.conversations',
      { types: ALL_KINDS, exclude_archived: true },
      'channels',
      1000,
    ).then((all) => new Set(all.map((c) => c.id))),
    mutedConversations(),
  ])
  const value = { joined, muted }
  cachedFilters = { at: now, value }
  return value
}

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
        ...(includeThread ? { thread: messages.map(summary) } : {}),
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

  {
    id: 'slack.recentMessages',
    name: 'Recent Slack messages',
    description:
      'The last few messages from anywhere you can see — DMs, groups, channels — newest first. As close to a notifications feed as the Web API offers: one search, sorted by time rather than relevance, with channels you aren’t in and conversations you’ve muted left out.',
    safety: 'dangerous',
    args: z.object({
      limit: z.number().int().min(1).max(100).default(10).describe('How many to return'),
      // A day count rather than a date, so the whole tool has a default and asks
      // nothing: "the last ten messages" should be one keystroke, not a form.
      days: z.number().int().min(1).max(90).default(2).describe('How far back to look'),
      includeMine: z.boolean().default(false).describe('Keep your own messages in'),
      includeUnjoined: z
        .boolean()
        .default(false)
        .describe('Keep public channels you are not a member of'),
      includeMuted: z.boolean().default(false).describe('Keep muted conversations'),
    }),
    handler: async ({ limit, days, includeMine, includeUnjoined, includeMuted }) => {
      // Two days rather than one by default: `after:` is day-granular, and
      // whether it counts the day it names is not worth depending on.
      const from = daysAgo(days)
      const handle = includeMine ? null : (await whoAmI()).handle
      const filters = includeUnjoined && includeMuted ? null : await feedFilters()

      // Both filters run after the search, so some of what comes back is thrown
      // away — ask for more than is wanted, or a busy hour in channels you don't
      // follow could swallow the lot. `scanned` against `count` is the ratio.
      const wanted = Math.min(100, filters ? limit * 4 : limit)
      const found = await slack<SlackResponse & { messages?: { matches?: SearchMatch[] } }>(
        'search.messages',
        {
          query: recentQuery(from, handle || null),
          sort: 'timestamp',
          sort_dir: 'desc',
          count: wanted,
        },
      )
      const matches = found.messages?.matches ?? []
      const kept = matches.filter((m) =>
        keepInFeed(m.channel?.id, filters, { unjoined: includeUnjoined, muted: includeMuted }),
      )

      return {
        since: from,
        scanned: matches.length,
        count: Math.min(kept.length, limit),
        messages: kept.slice(0, limit).map((m) => ({
          channel: m.channel?.id ?? null,
          channelName: m.channel?.name ? `#${m.channel.name}` : null,
          ts: m.ts ?? null,
          user: m.user ?? null,
          userName: m.username ?? null,
          text: m.text ?? '',
          permalink: m.permalink ?? null,
        })),
      }
    },
  },

  {
    id: 'slack.listChannels',
    name: 'List Slack conversations',
    description:
      'Everywhere you are — DMs, group DMs, private and public channels, one list. This is where a conversation id for the other tools comes from.',
    safety: 'dangerous',
    args: z.object({
      types: z
        .string()
        .default(ALL_KINDS)
        .describe(`Comma-separated, from ${ALL_KINDS}`),
      offset: z.number().int().min(0).default(0).describe('How many to skip'),
      limit: z.number().int().min(1).max(200).default(50).describe('How many to return'),
      includeArchived: z.boolean().default(false).describe('Keep archived channels in'),
    }),
    handler: async ({ types, offset, limit, includeArchived }) => {
      const { items, hasMore } = await windowOf<Conversation>(
        'users.conversations',
        { types, exclude_archived: !includeArchived },
        'channels',
        offset,
        limit,
      )
      // Only the window being returned is named, so a big workspace costs a
      // handful of lookups rather than one per conversation you own.
      const named = await Promise.all(
        items.map(async (c) => ({
          id: c.id,
          kind: kindOf(c),
          name: await nameOf(c),
          topic: c.topic?.value || null,
          archived: !!c.is_archived,
        })),
      )
      return { offset, limit, hasMore, count: named.length, conversations: named }
    },
  },

  {
    id: 'slack.getChannelMessages',
    name: 'Get Slack conversation messages',
    description:
      'Messages in one conversation, newest first, paged with `offset`. Top-level messages only: a reply count marks the ones with a thread under them, which “Read a Slack message” will open.',
    safety: 'dangerous',
    args: z.object({
      channel: conversation.describe(
        'Conversation id from “List Slack conversations”, or a Slack link',
      ),
      offset: z.number().int().min(0).default(0).describe('How many to skip'),
      limit: z.number().int().min(1).max(200).default(20).describe('How many to return'),
    }),
    handler: async ({ channel, offset, limit }) => {
      const ref = parseRef(channel)
      const { items, hasMore } = await windowOf<SlackMessage>(
        'conversations.history',
        { channel: ref.channel },
        'messages',
        offset,
        limit,
      )
      return {
        channel: ref.channel,
        offset,
        limit,
        hasMore,
        count: items.length,
        messages: items.map(summary),
      }
    },
  },
]
