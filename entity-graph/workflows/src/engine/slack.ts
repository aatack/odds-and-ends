// Slack, as the operator, through the user tokens on the Integrations page.
//
// What Slack answers is cached for a while, in the backend's memory (never the
// store): names for a day, channels for an hour, messages for a few minutes.
// The slack_message and slack_channel events keep only where a message or
// channel is; their cards and views read the rest through the cache, as they
// are shown. To wait for a reply, a script checks with these actions.

import { z } from 'zod'
import type { SlackMessage, SlackView } from '../shared/types'
import type { Engine } from './engine'
import type { Action } from './integrations'
import { parseSlackLink } from '../shared/slack'
import { mentioned, mrkdwnToMarkdown } from './mrkdwn'
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

// ---- the cache ----

const MINUTE = 60_000
const TTL = { names: 24 * 60 * MINUTE, channels: 60 * MINUTE, messages: 5 * MINUTE }
/** Nothing is kept longer than this, whatever it is. */
const MAX_AGE = 24 * 60 * MINUTE

/** Each engine's cache: by workspace and key, what Slack answered and when. */
const caches = new WeakMap<Engine, Map<string, { value: unknown; at: number }>>()
const cacheOf = (engine: Engine) => caches.get(engine) ?? caches.set(engine, new Map()).get(engine)!

/** What `key` was last fetched as, if it is younger than `ttl`; else `load` it, and keep that. */
async function cached<T>(engine: Engine, w: SlackWorkspace, key: string, ttl: number, load: () => Promise<T>): Promise<T> {
  const cache = cacheOf(engine)
  const k = `${w.team}\n${key}`
  const hit = cache.get(k)
  if (hit && engine.now() - hit.at < ttl) return hit.value as T
  const value = await load()
  for (const [key, e] of cache) if (engine.now() - e.at >= MAX_AGE) cache.delete(key)
  cache.set(k, { value, at: engine.now() })
  return value
}

export function slackCacheStats(engine: Engine): { entries: number; oldest: number | null } {
  const ats = [...cacheOf(engine).values()].map((e) => e.at)
  return { entries: ats.length, oldest: ats.length ? Math.min(...ats) : null }
}

export function clearSlackCache(engine: Engine): void {
  cacheOf(engine).clear()
}

// ---- messages, as the app shows them ----

/** A message as Slack answers it, as far as it is read here. */
type Raw = {
  ts: string
  user?: string
  username?: string
  bot_profile?: { name?: string }
  text?: string
  thread_ts?: string
  reply_count?: number
  files?: { name?: string; title?: string }[]
  channel?: { id: string; name?: string }
  permalink?: string
}

async function userName(engine: Engine, w: SlackWorkspace, id: string): Promise<string | undefined> {
  try {
    return await cached(engine, w, `user:${id}`, TTL.names, async () => {
      const u = (await slackCall(engine, w.id, 'users.info', { user: id })).user as {
        name?: string
        real_name?: string
        profile?: { display_name?: string; real_name?: string }
      }
      return u.profile?.display_name || u.profile?.real_name || u.real_name || u.name || id
    })
  } catch {
    // Without users:read, a mention keeps its id.
    return undefined
  }
}

async function channelInfo(engine: Engine, w: SlackWorkspace, id: string): Promise<{ id: string; name: string; is_im?: boolean; user?: string }> {
  return cached(engine, w, `channel:${id}`, TTL.channels, async () => {
    const c = (await slackCall(engine, w.id, 'conversations.info', { channel: id })).channel as { id: string; name?: string; is_im?: boolean; user?: string }
    return { id: c.id, name: c.name ?? c.id, is_im: c.is_im, user: c.user }
  })
}

/** A channel's name to show: #name, or for a DM, the other person's. */
async function channelName(engine: Engine, w: SlackWorkspace, id: string): Promise<string> {
  try {
    const c = await channelInfo(engine, w, id)
    if (c.is_im && c.user) return `DM with ${(await userName(engine, w, c.user)) ?? c.user}`
    return `#${c.name}`
  } catch {
    return id
  }
}

const permalink = (w: SlackWorkspace, channel: string, m: Raw) =>
  w.url
    ? `${w.url.replace(/\/?$/, '/')}archives/${channel}/p${m.ts.replace('.', '')}${m.thread_ts && m.thread_ts !== m.ts ? `?thread_ts=${m.thread_ts}&cid=${channel}` : ''}`
    : undefined

/** Messages as the app shows them: names for ids, markdown for mrkdwn. */
export async function render(engine: Engine, w: SlackWorkspace, channel: string | null, raw: Raw[]): Promise<SlackMessage[]> {
  const users = new Set<string>()
  const channels = new Set<string>()
  for (const m of raw) {
    if (m.user) users.add(m.user)
    const said = mentioned(m.text ?? '')
    said.users.forEach((u) => users.add(u))
    said.channels.forEach((c) => channels.add(c))
  }
  const names = new Map<string, string | undefined>()
  await Promise.all([...users].map(async (u) => names.set(u, await userName(engine, w, u))))
  const chans = new Map<string, string>()
  await Promise.all([...channels].map(async (c) => chans.set(c, (await channelName(engine, w, c)).replace(/^#/, ''))))
  return raw.map((m) => {
    const where = channel ?? m.channel?.id ?? ''
    const files = (m.files ?? []).map((f) => f.title || f.name || 'a file')
    const link = m.permalink ?? permalink(w, where, m)
    return {
      ts: m.ts,
      ...(m.thread_ts ? { thread_ts: m.thread_ts } : {}),
      ...(m.user ? { user: m.user } : {}),
      author: (m.user && names.get(m.user)) || m.username || m.bot_profile?.name || m.user || 'Slack',
      text: mrkdwnToMarkdown(m.text ?? '', { user: (id) => names.get(id), channel: (id) => chans.get(id) }),
      ...(m.reply_count ? { replies: m.reply_count } : {}),
      ...(files.length ? { files } : {}),
      ...(link ? { permalink: link } : {}),
    }
  })
}

/** A thread: its first message and every reply, oldest first. */
async function threadOf(engine: Engine, w: SlackWorkspace, channel: string, ts: string): Promise<Raw[]> {
  return cached(engine, w, `thread:${channel}:${ts}`, TTL.messages, async () => {
    const out: Raw[] = []
    let cursor: string | undefined
    do {
      const r = await slackCall(engine, w.id, 'conversations.replies', { channel, ts, limit: 200, cursor })
      out.push(...(r.messages as Raw[]))
      cursor = (r.response_metadata as { next_cursor?: string } | undefined)?.next_cursor || undefined
    } while (cursor && out.length < 1000)
    return out
  })
}

/** A channel's messages between two times, oldest first: at most `limit` of them, the newest if there are more. */
async function historyOf(
  engine: Engine,
  w: SlackWorkspace,
  channel: string,
  range: { oldest?: string; latest?: string; inclusive?: boolean; limit: number },
): Promise<Raw[]> {
  const key = `history:${channel}:${range.oldest ?? ''}:${range.latest ?? ''}:${range.inclusive ? 1 : 0}:${range.limit}`
  return cached(engine, w, key, TTL.messages, async () => {
    const out: Raw[] = []
    let cursor: string | undefined
    do {
      const r = await slackCall(engine, w.id, 'conversations.history', {
        channel,
        oldest: range.oldest,
        latest: range.latest,
        inclusive: range.inclusive ? true : undefined,
        limit: Math.min(200, range.limit - out.length),
        cursor,
      })
      out.push(...(r.messages as Raw[]))
      cursor = (r.response_metadata as { next_cursor?: string } | undefined)?.next_cursor || undefined
    } while (cursor && out.length < range.limit)
    return out.reverse()
  })
}

/** A time as Slack's ts: a ts as it is, else an ISO time, a date, or milliseconds. */
export function toTs(when: string | number): string {
  const s = String(when).trim()
  if (/^\d{9,10}(\.\d+)?$/.test(s)) return s.includes('.') ? s : `${s}.000000`
  const ms = /^\d{12,}$/.test(s) ? Number(s) : Date.parse(s)
  if (Number.isNaN(ms)) throw new Error(`Not a time: ${s}. Give an ISO time (2026-10-23T09:00), a date, or a Slack ts.`)
  return (ms / 1000).toFixed(6)
}

const day = (ts: string, offset: number) => new Date((Number(ts) + offset * 86400) * 1000).toISOString().slice(0, 10)

/** The workspace a message's link is in: the one named, the one at its host, or the only one. */
async function linkWorkspace(engine: Engine, which: unknown, host: string): Promise<SlackWorkspace> {
  if (which) return slackWorkspace(engine, which)
  const all = engine.settings.slackWorkspaces()
  // A workspace added before addresses were kept learns its own.
  for (const w of all.filter((w) => !w.url)) {
    try {
      const url = (await slackCall(engine, w.id, 'auth.test', {})).url
      if (typeof url === 'string') engine.settings.setSlackUrl(w.id, (w.url = url))
    } catch {
      // Then it is matched only as the only workspace.
    }
  }
  return all.find((w) => w.url && new URL(w.url).host === host) ?? slackWorkspace(engine, undefined)
}

/** Where one message is, from its link or its channel and ts. */
export async function locate(
  engine: Engine,
  p: { workspace?: unknown; link?: unknown; channel?: unknown; ts?: unknown; thread_ts?: unknown },
): Promise<{ w: SlackWorkspace; channel: string; ts: string; thread_ts?: string }> {
  if (p.link) {
    const at = parseSlackLink(String(p.link))
    if (!at) throw new Error(`Not a Slack message link: ${p.link}`)
    return { w: await linkWorkspace(engine, p.workspace, at.host), channel: at.channel, ts: at.ts, ...(at.thread_ts ? { thread_ts: at.thread_ts } : {}) }
  }
  if (!p.channel || !p.ts) throw new Error('Give a message link, or its channel and ts.')
  return { w: slackWorkspace(engine, p.workspace), channel: String(p.channel), ts: String(p.ts), ...(p.thread_ts ? { thread_ts: String(p.thread_ts) } : {}) }
}

/** One message and the thread it is in (or starts), oldest first. */
export async function messageAndThread(
  engine: Engine,
  p: { workspace?: unknown; link?: unknown; channel?: unknown; ts?: unknown; thread_ts?: unknown },
): Promise<{ w: SlackWorkspace; channel: string; channelName: string; message: SlackMessage; thread: SlackMessage[] }> {
  const at = await locate(engine, p)
  // Replies to a message that starts no thread answer just that message.
  const raw = await threadOf(engine, at.w, at.channel, at.thread_ts ?? at.ts)
  const thread = await render(engine, at.w, at.channel, raw)
  const message = thread.find((m) => m.ts === at.ts)
  if (!message) throw new Error(`No message ${at.ts} in ${at.channel}.`)
  return { w: at.w, channel: at.channel, channelName: await channelName(engine, at.w, at.channel), message, thread }
}

type Channel = { id: string; name: string; private: boolean; topic?: string; members?: number; user?: string }

/** Every channel the operator is in, and with `dms`, their DMs. */
async function channelsOf(engine: Engine, w: SlackWorkspace, dms: boolean): Promise<Channel[]> {
  const types = dms ? 'public_channel,private_channel,im,mpim' : 'public_channel,private_channel'
  return cached(engine, w, `channels:${types}`, TTL.channels, async () => {
    const out: Channel[] = []
    let cursor: string | undefined
    do {
      const r = await slackCall(engine, w.id, 'users.conversations', { types, exclude_archived: true, limit: 200, cursor })
      for (const c of r.channels as { id: string; name?: string; is_private?: boolean; topic?: { value?: string }; num_members?: number; user?: string }[])
        out.push({ id: c.id, name: c.name ?? c.id, private: !!c.is_private, topic: c.topic?.value || undefined, members: c.num_members, user: c.user })
      cursor = (r.response_metadata as { next_cursor?: string } | undefined)?.next_cursor || undefined
    } while (cursor && out.length < 2000)
    return out
  })
}

/** A channel's id, from its id or its #name (one the operator is in). */
async function channelId(engine: Engine, w: SlackWorkspace, channel: string): Promise<string> {
  if (/^[CGD][A-Z0-9]+$/.test(channel)) return channel
  const found = (await channelsOf(engine, w, false)).find((c) => c.name === channel.replace(/^#/, ''))
  if (!found) throw new Error(`No channel ${channel} that the operator is in.`)
  return found.id
}

// ---- the actions ----

export const slackActions: Action[] = [
  {
    name: 'slack_workspaces',
    unsafe: false,
    description: 'The Slack workspaces set up, and who the operator is in each.',
    input: {},
    run: async (_a, { engine }) => engine.settings.slackWorkspaces().map(({ token: _, ...w }) => w),
  },
  {
    name: 'slack_channels',
    unsafe: false,
    description: 'The channels the operator is in (and the DMs, with `dms`), with their ids. Filter by name with `query`. Needs channels:read and groups:read.',
    input: {
      workspace,
      query: z.string().optional().describe('Only channels whose name contains this.'),
      dms: z.boolean().default(false).describe('Include DMs and group DMs (needs im:read and mpim:read).'),
    },
    run: async (a, { engine }) => {
      const all = await channelsOf(engine, slackWorkspace(engine, a.workspace), !!a.dms)
      const q = String(a.query ?? '').toLowerCase().replace(/^#/, '')
      return q ? all.filter((c) => c.name.toLowerCase().includes(q)) : all
    },
  },
  {
    name: 'slack_message',
    unsafe: false,
    description: 'One message, from its link (or its channel and ts), with the thread it is in or starts. Text is markdown, with names for ids.',
    input: {
      workspace,
      link: z.string().optional().describe("The message's link, as Slack's Copy link gives it."),
      channel: z.string().optional(),
      ts: z.string().optional(),
      thread_ts: z.string().optional().describe('The thread it is in, if it is a reply and there is no link.'),
    },
    run: async (a, { engine }) => {
      const { channel, channelName: name, message, thread } = await messageAndThread(engine, a)
      return { channel, channel_name: name, message, thread: thread.length > 1 ? thread : [] }
    },
  },
  {
    name: 'slack_messages',
    unsafe: false,
    description:
      'Messages across the whole workspace between two times, oldest first, through search (needs search:read). ' +
      'Narrow it with `query` in Slack search syntax, e.g. "in:#eng from:@sam".',
    input: {
      workspace,
      after: z.string().describe('From this time: an ISO time, a date, or a Slack ts.'),
      before: z.string().optional().describe('Up to this time. Left out, now.'),
      query: z.string().optional(),
      limit: z.number().int().default(100),
    },
    run: async (a, { engine }) => {
      const w = slackWorkspace(engine, a.workspace)
      const from = toTs(String(a.after))
      const to = toTs(String(a.before ?? engine.now()))
      // Search knows days only, and its after: and before: leave the day out.
      const query = `${a.query ?? ''} after:${day(from, -1)} before:${day(to, 1)}`.trim()
      const limit = Number(a.limit)
      const raw = await cached(engine, w, `search:${query}:${limit}`, TTL.messages, async () => {
        const out: Raw[] = []
        for (let page = 1; out.length < limit && page <= 10; page++) {
          const r = await slackCall(engine, w.id, 'search.messages', { query, sort: 'timestamp', sort_dir: 'asc', count: Math.min(100, limit), page })
          const m = r.messages as { matches?: Raw[]; paging?: { pages?: number } }
          out.push(...(m.matches ?? []))
          if (page >= (m.paging?.pages ?? 1)) break
        }
        return out
      })
      const within = raw.filter((m) => Number(m.ts) >= Number(from) && Number(m.ts) <= Number(to)).slice(0, limit)
      const shown = await render(engine, w, null, within)
      return shown.map((m, i) => ({ ...m, channel: within[i].channel }))
    },
  },
  {
    name: 'slack_messages_around',
    unsafe: false,
    description: 'The messages just before and after one, in its channel, or in its thread if it is in one; oldest first.',
    input: {
      workspace,
      link: z.string().optional().describe("The message's link."),
      channel: z.string().optional(),
      ts: z.string().optional().describe('The time to look around: a ts, or an ISO time.'),
      thread_ts: z.string().optional().describe('Look in this thread rather than the channel.'),
      before: z.number().int().default(10),
      after: z.number().int().default(10),
    },
    run: async (a, { engine }) => {
      const at = await locate(engine, { ...a, ts: a.ts === undefined ? undefined : toTs(String(a.ts)) })
      const [b, n] = [Number(a.before), Number(a.after)]
      let raw: Raw[]
      if (at.thread_ts) {
        const thread = await threadOf(engine, at.w, at.channel, at.thread_ts)
        const i = Math.max(0, thread.findIndex((m) => Number(m.ts) >= Number(at.ts)))
        raw = thread.slice(Math.max(0, i - b), i + n + 1)
      } else {
        const [older, newer] = await Promise.all([
          historyOf(engine, at.w, at.channel, { latest: at.ts, inclusive: true, limit: b + 1 }),
          historyOf(engine, at.w, at.channel, { oldest: at.ts, limit: 1000 }),
        ])
        raw = [...older, ...newer.slice(0, n)]
      }
      return render(engine, at.w, at.channel, raw)
    },
  },
  {
    name: 'slack_channel_history',
    unsafe: false,
    description: 'Messages in a channel or DM, oldest first: the latest `limit`, or those between `oldest` and `latest`. Replies are left out: see slack_message.',
    input: {
      workspace,
      channel: z.string().describe('The channel id, e.g. C0123.'),
      oldest: z.string().optional().describe('From this time: an ISO time, a date, or a ts.'),
      latest: z.string().optional().describe('Up to this time.'),
      limit: z.number().int().default(30),
    },
    run: async (a, { engine }) => {
      const w = slackWorkspace(engine, a.workspace)
      const raw = await historyOf(engine, w, String(a.channel), {
        oldest: a.oldest ? toTs(String(a.oldest)) : undefined,
        latest: a.latest ? toTs(String(a.latest)) : undefined,
        limit: Number(a.limit),
      })
      return render(engine, w, String(a.channel), raw)
    },
  },
  {
    name: 'slack_thread',
    unsafe: false,
    description: 'A thread: its first message and every reply.',
    input: { workspace, channel: z.string(), ts: z.string().describe("The thread's first message's ts.") },
    run: async (a, { engine }) => {
      const w = slackWorkspace(engine, a.workspace)
      return render(engine, w, String(a.channel), await threadOf(engine, w, String(a.channel), String(a.ts)))
    },
  },
  {
    name: 'slack_search',
    unsafe: false,
    description: "Search messages with Slack's search syntax, e.g. \"from:@sam in:#eng deploy\". Needs the search:read scope.",
    input: { workspace, query: z.string(), count: z.number().int().default(20) },
    run: async (a, { engine }) => {
      const w = slackWorkspace(engine, a.workspace)
      const r = await slackCall(engine, w.id, 'search.messages', { query: a.query, count: a.count })
      const matches = (r.messages as { matches?: Raw[] }).matches ?? []
      const shown = await render(engine, w, null, matches)
      return shown.map((m, i) => ({ ...m, channel: matches[i].channel }))
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

// ---- the slack_message and slack_channel events ----

export const slackMessageParams = {
  workspace,
  link: z.string().optional().describe("The message's link, as Slack's Copy link gives it. Or give channel and ts."),
  channel: z.string().optional(),
  ts: z.string().optional(),
  thread_ts: z.string().optional().describe('The thread it is in, if it is a reply.'),
}

export const slackChannelParams = {
  workspace,
  channel: z.string().describe('The channel: its id, or #name.'),
  oldest: z.string().describe('From this time: an ISO time, a date, or a Slack ts.'),
  latest: z.string().optional().describe('Up to this time. Left out, up to now.'),
}

/** What a Slack event's view shows: through the cache, as the actions read it. */
export async function slackView(engine: Engine, event: { type: string; params: Record<string, unknown> }, thread?: string): Promise<SlackView> {
  const p = event.params
  if (event.type === 'slack_message') {
    const { w, channel, channelName: name, message, thread: messages } = await messageAndThread(engine, p)
    if (thread) return { workspace: w.name, channel, channelName: name, messages: messages.filter((m) => m.ts !== thread) }
    return { workspace: w.name, channel, channelName: name, messages, focus: message.ts }
  }
  if (event.type === 'slack_channel') {
    const w = slackWorkspace(engine, p.workspace)
    const channel = await channelId(engine, w, String(p.channel))
    const name = await channelName(engine, w, channel)
    if (thread) return { workspace: w.name, channel, channelName: name, messages: (await render(engine, w, channel, await threadOf(engine, w, channel, thread))).slice(1) }
    const raw = await historyOf(engine, w, channel, { oldest: toTs(String(p.oldest)), latest: p.latest ? toTs(String(p.latest)) : undefined, limit: 1000 })
    return { workspace: w.name, channel, channelName: name, messages: await render(engine, w, channel, raw) }
  }
  throw new Error(`A ${event.type} event has no Slack messages.`)
}
