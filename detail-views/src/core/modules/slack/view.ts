import type { Lens, ModuleView } from '../../present.ts'
import type { ItemType } from '../../types.ts'
import { slackWrites } from './api.ts'
import { emoji, emojify } from './emoji.ts'
import { slackToMarkdown } from './markdown.ts'

const minute = 60_000
const day = 24 * 60 * minute

export const slackIds = {
  root: 'slack',
  conversation: (channel: string) => `slack:conv:${channel}`,
  message: (channel: string, ts: string) => `slack:msg:${channel}:${ts}`,
  user: (user: string) => `slack:user:${user}`,
  /** When the watch last looked, and how much it found. */
  watch: 'slack:watch',
}

/** The channel and ts in a message id. */
export function parseMessageId(id: string): { channel: string; ts: string } | null {
  const match = /^slack:msg:([^:]+):(\d+\.\d+)$/.exec(id)
  return match ? { channel: match[1], ts: match[2] } : null
}

/** Messages that are not somebody saying something. */
export const quiet = new Set(['channel_join', 'channel_leave', 'group_join', 'group_leave', 'bot_add', 'bot_remove'])

/** An image attached to a message. */
export interface ImageData {
  id: string
  name: string
  full: string
  thumb: string
  width?: number
  height?: number
}

export interface ConversationData {
  channel: string
  kind: 'channel' | 'private' | 'im' | 'mpim'
  name?: string
  user?: string
}

export interface MessageData {
  channel: string
  ts: string
  threadTs?: string
  user?: string
  botName?: string
  subtype?: string
  text: string
  replyCount?: number
  latestReply?: string
  reactions?: { name: string; count: number; users: string[] }[]
  images?: ImageData[]
}

export function maxTs(...values: (string | undefined)[]): string | undefined {
  let best: string | undefined
  for (const value of values) if (value && (!best || Number(value) > Number(best))) best = value
  return best
}

/** A Slack ts (`1712345678.123456`) as Unix ms: when a message's events happened. */
export function tsMillis(ts: string): number {
  return Math.floor(Number(ts) * 1000)
}

/** The user's name, or their id until it has loaded. */
function userName(lens: Lens, user: string): string {
  const name = lens.read(slackIds.user(user))?.data.name
  return typeof name === 'string' && name ? name : user
}

/** My DM with a user, if the app has one: the conversation list notes it on the user. */
function dmWith(lens: Lens, user: string): string | null {
  const dm = lens.read(slackIds.user(user))?.data.dm
  return typeof dm === 'string' ? dm : null
}

function conversationTitle(lens: Lens, data: ConversationData): string {
  if (data.kind === 'im') return data.user ? userName(lens, data.user) : data.channel
  if (data.kind === 'mpim') {
    const names = (data.name ?? '').replace(/^mpdm-/, '').replace(/-\d+$/, '').split('--')
    return names.join(', ')
  }
  return `#${data.name ?? data.channel}`
}

/** Mentions, channel links and links rendered as text. */
function render(lens: Lens, text: string): string {
  return text
    .replace(/<@([UW][A-Z0-9]+)(?:\|[^>]*)?>/g, (_, user: string) => `@${userName(lens, user)}`)
    .replace(/<#[A-Z0-9]+\|([^>]*)>/g, '#$1')
    .replace(/<!(here|channel|everyone)[^>]*>/g, '@$1')
    .replace(/<!subteam\^[A-Z0-9]+\|([^>]*)>/g, '$1')
    .replace(/<([^>|]+)\|([^>]+)>/g, '$2')
    .replace(/<([^>]+)>/g, '$1')
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&amp;/g, '&')
    .replace(/:[a-z0-9_+-]+(?:::skin-tone-\d)?:/g, (match) => emojify(match))
}

function markdown(lens: Lens, text: string): string {
  return slackToMarkdown(text, {
    user: (id) => ({ name: userName(lens, id), target: dmWith(lens, id) }),
    channel: (id) => (lens.read(slackIds.conversation(id))?.data.channel ? slackIds.conversation(id) : null),
  })
}

const types: Record<string, ItemType> = {
  'slack:conv:': 'slack.conversation',
  'slack:msg:': 'slack.message',
  'slack:user:': 'slack.user',
}

export const slackView: ModuleView = {
  id: 'slack',
  name: 'Slack',
  root: slackIds.root,

  typeOf(id) {
    if (id === slackIds.root) return 'slack.home'
    if (id === slackIds.watch) return 'slack.watch'
    for (const [prefix, type] of Object.entries(types)) if (id.startsWith(prefix)) return type
    return null
  },

  owns: (type) => type.startsWith('slack.'),

  // Only the workspace's lists load on their own. Messages come from the
  // watch (`Slack.poll`) and its first batch back through history; anything
  // older is asked for (`older`). A message seen only by its id (linked from a
  // task, say) loads itself, once.
  foreign(_id, type) {
    switch (type) {
      case 'slack.home':
        return { children: 60 * minute }
      case 'slack.message':
        return { self: Infinity }
      case 'slack.user':
        return { self: 7 * day }
      default:
        return null
    }
  },

  newestFirst: (type) => type === 'slack.conversation',

  // Further back is always possible until a load reached the start. A message
  // goes further back only as a thread: its whole thread.
  older: (entity) =>
    !entity.data.complete &&
    (entity.type === 'slack.home' || entity.type === 'slack.conversation' || (entity.type === 'slack.message' && Boolean(entity.data.replyCount))),

  compose(entity, lens) {
    if (lens.read(slackIds.root)?.data.connected === false) return 'slack-token'
    if (!slackWrites) return null
    return entity.type === 'slack.conversation' || entity.type === 'slack.message' ? 'slack' : null
  },

  // Most recently changed first: the newest event on a conversation, which is
  // the link to its newest message. Loads and undated values sit at 0, so
  // fetching a conversation again never moves it.
  order(entity, children) {
    if (entity.type !== 'slack.home') return children
    return [...children].sort((a, b) => b.updatedAt - a.updatedAt)
  },

  // `from` is where what is cached starts: everything after it is here. For a
  // conversation that is the older of its own cursor and the workspace's,
  // since every conversation is cached back to the workspace's. `complete`
  // says nothing is older.
  present(entity, lens) {
    const cursor = (data: Record<string, unknown> | undefined) =>
      typeof data?.['history.oldest'] === 'string' ? (data['history.oldest'] as string) : undefined
    if (entity.type === 'slack.home') {
      const watch = lens.read(slackIds.watch)?.data
      return {
        ...entity,
        data: {
          ...entity.data,
          from: cursor(entity.data) ?? null,
          complete: Boolean(entity.data['history.complete']),
          polledAt: watch?.polledAt ?? null,
          found: watch?.found ?? null,
        },
      }
    }
    if (entity.type === 'slack.conversation') {
      const data = { channel: entity.id.slice('slack:conv:'.length), ...entity.data } as unknown as ConversationData
      const global = cursor(lens.read(slackIds.root)?.data)
      const own = cursor(entity.data)
      const from = own && global ? (Number(own) < Number(global) ? own : global) : (own ?? global ?? null)
      return {
        ...entity,
        data: { ...entity.data, title: conversationTitle(lens, data), from, complete: Boolean(entity.data['history.complete']) },
      }
    }
    if (entity.type !== 'slack.message') return entity
    const data = entity.data as unknown as MessageData
    const self = lens.read(slackIds.root)?.data.self
    const text = String(data.text ?? '')
    return {
      ...entity,
      data: {
        ...data,
        author: data.user ? userName(lens, data.user) : (data.botName ?? 'bot'),
        text: render(lens, text),
        markdown: markdown(lens, text),
        authorTarget: data.user ? dmWith(lens, data.user) : null,
        authorKey: data.user ?? data.botName ?? 'bot',
        images: (data.images ?? []).map((image) => ({
          name: image.name,
          thumb: `thumb/${entity.id}/${image.id}`,
          full: `full/${entity.id}/${image.id}`,
          width: image.width,
          height: image.height,
        })),
        quiet: Boolean(data.subtype && quiet.has(data.subtype)),
        // Where the message is, for when it is shown away from its conversation.
        conversation: data.channel ? slackIds.conversation(data.channel) : null,
        where: data.channel ? conversationTitle(lens, { channel: data.channel, ...lens.read(slackIds.conversation(data.channel))?.data } as ConversationData) : null,
        complete: Boolean(entity.data['history.complete']),
        reactions: (data.reactions ?? []).map((reaction) => ({
          emoji: emoji(reaction.name),
          count: reaction.count,
          mine: reaction.users.includes(String(self ?? '')),
        })),
      },
    }
  },
}
