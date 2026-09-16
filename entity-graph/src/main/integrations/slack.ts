import { z } from 'zod'
import type { ToolDef } from '../../core/pensive/index'
import { optionalEnv, requireEnv } from './env'
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
    'and reinstalling issues a new token, so copy it into `.env`.',
    `\nThe token currently has: ${res.provided || '(nothing)'}`,
  ].join(' ')
}

/**
 * One Web API call, as whoever the token belongs to. Every method takes a form
 * body, so one shape covers them all.
 *
 * The token is an argument rather than read from `.env` here because there is
 * more than one holder of one: the tools below are the app's own hands and use
 * what is in `.env`, while a `slackEvents` node on the sources page carries its
 * own, which is the token the *node* was configured with and nobody else's.
 */
export async function slackCall<T extends SlackResponse>(
  token: string,
  method: string,
  params: Record<string, string | number | boolean | undefined>,
): Promise<T> {
  const res = await postForm<T>(`${API}/${method}`, params, {
    Authorization: `Bearer ${token}`,
  })
  if (!res.ok) throw new Error(slackError(method, res))
  return res
}

/** One Web API call as the app itself — the token in `.env`. */
async function slack<T extends SlackResponse>(
  method: string,
  params: Record<string, string | number | boolean | undefined>,
): Promise<T> {
  return slackCall<T>(requireEnv('SLACK_TOKEN', 'SLACK_USER_TOKEN', 'SLACK_BOT_TOKEN'), method, params)
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

/** A user id (`U…`, or `W…` on an enterprise grid) or an app's (`B…`). */
const PERSON_OR_BOT = /^[UWB][A-Z0-9]+$/
const MENTION = /^<@([UW][A-Z0-9]+)(?:\|[^>]*)?>$/i

/**
 * Read a reference to somebody. The plain id is what the other tools hand back,
 * but a message's *text* writes a mention as `<@U0123ABCD>`, so that is the form
 * most likely to be copied out of one.
 *
 * A handle is refused rather than attempted: Slack has no method that takes one,
 * so there is nothing to fall back to and a guess would come back as
 * `user_not_found` with no hint as to why.
 */
export function parseUserId(reference: string): string {
  const ref = reference.trim()
  const mention = MENTION.exec(ref)
  const id = (mention ? mention[1] : ref.replace(/^@/, '')).toUpperCase()
  if (PERSON_OR_BOT.test(id)) return id
  throw new Error(
    `"${reference}" is not a Slack user id. Expected a U…, W… or B… — the \`user\` ` +
      'every other Slack tool hands back, or a <@U0123ABCD> mention out of a message. ' +
      'A handle or a name cannot be looked up: the Web API takes neither.',
  )
}

export interface SlackMessage {
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

/**
 * Somebody in the workspace. Slack keeps a person's name in three places — the
 * handle they were given, their real name, and whatever they typed as a display
 * name — and which of those are filled in varies per account, so nothing here
 * picks one on the reader's behalf.
 */
interface SlackUser {
  id?: string
  name?: string
  real_name?: string
  is_bot?: boolean
  deleted?: boolean
  profile?: { display_name?: string; real_name?: string }
}

/** People by user id, remembered — a workspace's people rarely change. */
const people = new Map<string, SlackUser>()

/** One `users.info`, memoised. Raises what Slack said; the callers decide. */
async function fetchUser(id: string): Promise<SlackUser> {
  const known = people.get(id)
  if (known) return known
  const res = await slack<SlackResponse & { user?: SlackUser }>('users.info', { user: id })
  const user = res.user ?? { id }
  people.set(id, user)
  return user
}

/**
 * What to call somebody: the name they chose for themselves, then their real
 * name, then the handle. The id is the last resort, and is at least unambiguous.
 */
const personName = (user: SlackUser, id: string): string =>
  user.profile?.display_name || user.profile?.real_name || user.real_name || user.name || id

/**
 * A name for a user id, or the id back if there isn't one to be had. This is the
 * decorative use — labelling a DM in a list — so a token without `users:read`
 * still gets its conversations rather than an error.
 */
const displayName = (id: string): Promise<string> =>
  fetchUser(id).then(
    (user) => personName(user, id),
    () => id,
  )

/** Whose id it was, in one shape whether that turned out to be a person or an app. */
export interface SlackIdentity {
  id: string
  name: string
  handle: string | null
  realName: string | null
  isBot: boolean
  deleted: boolean
}

/**
 * Who an id belongs to. Two methods, because the messages these tools hand back
 * name their author with either — a person's `user`, or an app's `bot_id`, which
 * `users.info` has never heard of. Which one it is is legible from the id, and
 * the answer is the same shape either way, so nobody asking has to know.
 */
async function identityOf(id: string): Promise<SlackIdentity> {
  if (id.startsWith('B')) {
    const res = await slack<
      SlackResponse & { bot?: { id?: string; name?: string; deleted?: boolean } }
    >('bots.info', { bot: id })
    const bot = res.bot ?? {}
    return {
      id: bot.id ?? id,
      name: bot.name || id,
      handle: null,
      realName: null,
      isBot: true,
      deleted: !!bot.deleted,
    }
  }
  const person = await fetchUser(id)
  return {
    id: person.id ?? id,
    name: personName(person, id),
    handle: person.name ? `@${person.name}` : null,
    realName: person.profile?.real_name || person.real_name || null,
    isBot: !!person.is_bot,
    deleted: !!person.deleted,
  }
}

/**
 * The messages around `ts`. `conversations.replies` is the uniform door: given a
 * thread parent it returns the thread, given a reply it returns the thread it is
 * in, and given a message that was never threaded it returns just that message.
 * `conversations.history` is the fallback for the workspaces where it isn't.
 */
export async function messagesAround(
  token: string,
  channel: string,
  ts: string,
): Promise<SlackMessage[]> {
  try {
    const replies = await slackCall<SlackResponse & { messages?: SlackMessage[] }>(
      token,
      'conversations.replies',
      { channel, ts, limit: 200, inclusive: true },
    )
    if (replies.messages?.length) return replies.messages
  } catch {
    // Not a thread, or this workspace disagrees — fall through to the history.
  }
  const history = await slackCall<SlackResponse & { messages?: SlackMessage[] }>(
    token,
    'conversations.history',
    { channel, latest: ts, oldest: ts, inclusive: true, limit: 1 },
  )
  return history.messages ?? []
}

/** One message, by where it is. Null when there is nothing there to read. */
export async function messageAt(
  token: string,
  channel: string,
  ts: string,
): Promise<SlackMessage | null> {
  const around = await messagesAround(token, channel, ts)
  return around.find((m) => m.ts === ts) ?? null
}

const messagesAt = (channel: string, ts: string): Promise<SlackMessage[]> =>
  messagesAround(requireEnv('SLACK_TOKEN', 'SLACK_USER_TOKEN', 'SLACK_BOT_TOKEN'), channel, ts)

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

/**
 * One hit from `search.messages`. It is not a message: it carries no
 * `thread_ts`, so whether it is a reply is legible only from its `permalink`,
 * which ends `?thread_ts=…` when it is one.
 */
export interface SearchMatch {
  channel?: {
    id?: string
    name?: string
    is_channel?: boolean
    is_group?: boolean
    is_im?: boolean
    is_mpim?: boolean
    is_private?: boolean
  }
  user?: string
  username?: string
  ts?: string
  text?: string
  permalink?: string
}

/** Slack's ceilings on `search.messages`: 100 per page, 100 pages. */
export const SEARCH_COUNT = 100
export const SEARCH_PAGES = 100

/**
 * One page of a search, newest first. Paged by number rather than by cursor —
 * `search.messages` is the one method that is — and the page count comes back
 * with it, so a caller walking backwards in time knows where the end is.
 */
export async function searchPage(
  token: string,
  query: string,
  page = 1,
  count = SEARCH_COUNT,
): Promise<{ matches: SearchMatch[]; pages: number; total: number }> {
  const found = await slackCall<
    SlackResponse & {
      messages?: {
        matches?: SearchMatch[]
        total?: number
        pagination?: { page_count?: number; total_count?: number }
      }
    }
  >(token, 'search.messages', {
    query,
    sort: 'timestamp',
    sort_dir: 'desc',
    count,
    page,
  })
  const matches = found.messages?.matches ?? []
  return {
    matches,
    pages: found.messages?.pagination?.page_count ?? page,
    // What the query matched in total, as against what this page holds. The
    // difference between "the search found nothing" and "the search found
    // plenty and we threw it away" is the first question to ask of a quiet feed.
    total: found.messages?.total ?? found.messages?.pagination?.total_count ?? matches.length,
  }
}

/**
 * Who a token belongs to, and where its workspace lives. The `url` is what makes
 * a permalink constructible rather than a call of its own: Slack's own form is
 * `<url>archives/<channel>/p<ts without its dot>`.
 */
export interface SlackWorkspace {
  id: string
  handle: string
  url: string
}

export async function whoHolds(token: string): Promise<SlackWorkspace> {
  const res = await slackCall<SlackResponse & { user?: string; user_id?: string; url?: string }>(
    token,
    'auth.test',
    {},
  )
  return { id: res.user_id ?? '', handle: res.user ?? '', url: res.url ?? '' }
}

/** One conversation's own description — its name and which kind it is. */
export async function conversationInfo(token: string, channel: string): Promise<Conversation> {
  const res = await slackCall<SlackResponse & { channel?: Conversation }>(
    token,
    'conversations.info',
    { channel },
  )
  return res.channel ?? { id: channel }
}

// --- Conversations ----------------------------------------------------------

export interface Conversation {
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

export type ConversationKind = 'dm' | 'group' | 'private' | 'channel'

/** Which kind of conversation something is, from whatever flags it came with. */
export const kindOf = (c: {
  is_im?: boolean
  is_mpim?: boolean
  is_private?: boolean
}): ConversationKind =>
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
      // away, and a busy hour in channels you don't follow could swallow a whole
      // page of it. So this pages: it asks for more until it has the window it
      // was after or Slack runs out, rather than returning short because the
      // first hundred happened to be noise. `scanned` against `count` is the
      // ratio it cost.
      const token = requireEnv('SLACK_TOKEN', 'SLACK_USER_TOKEN', 'SLACK_BOT_TOKEN')
      const query = recentQuery(from, handle || null)
      const matches: SearchMatch[] = []
      const kept: SearchMatch[] = []
      for (let page = 1; page <= SEARCH_PAGES && kept.length < limit; page++) {
        const found = await searchPage(token, query, page, filters ? SEARCH_COUNT : limit)
        if (!found.matches.length) break
        matches.push(...found.matches)
        kept.push(
          ...found.matches.filter((m) =>
            keepInFeed(m.channel?.id, filters, { unjoined: includeUnjoined, muted: includeMuted }),
          ),
        )
        if (page >= found.pages) break
      }

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

  {
    id: 'slack.getUser',
    name: 'Get a Slack user',
    description:
      'Who a user id belongs to. Every other tool here names a message’s author as a bare `U0123ABCD`, and this is the one thing that turns one into a name. Takes the id, or a `<@U0123ABCD>` mention lifted out of a message’s text; a bot id (`B…`) names the app instead.',
    safety: 'dangerous',
    args: z.object({
      user: z.string().min(1).describe('User id (U…/W…), a bot id (B…), or a <@U…> mention'),
    }),
    handler: async ({ user }) => identityOf(parseUserId(user)),
  },
]
