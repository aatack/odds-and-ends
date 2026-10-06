import type { Lens, ModuleView } from '../../present.ts'
import type { Entity, ItemType } from '../../types.ts'
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
  lastRead?: string
  latestTs?: string
  unread?: number
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
    for (const [prefix, type] of Object.entries(types)) if (id.startsWith(prefix)) return type
    return null
  },

  owns: (type) => type.startsWith('slack.'),

  foreign(_id, type) {
    switch (type) {
      case 'slack.home':
        return { children: 2 * minute }
      case 'slack.conversation':
        return { self: 5 * minute, children: 30_000 }
      case 'slack.message':
        return { self: day, children: 30_000 }
      case 'slack.user':
        return { self: 7 * day }
      default:
        return null
    }
  },

  newestFirst: (type) => type === 'slack.conversation',

  compose(entity, lens) {
    if (lens.read(slackIds.root)?.data.connected === false) return 'slack-token'
    if (!slackWrites) return null
    return entity.type === 'slack.conversation' || entity.type === 'slack.message' ? 'slack' : null
  },

  order(entity, children) {
    if (entity.type !== 'slack.home') return children
    const key = (child: Entity) => {
      const data = child.data as unknown as ConversationData
      return Number(maxTs(data.latestTs, data.lastRead) ?? 0)
    }
    const unread = (child: Entity) => ((child.data as unknown as ConversationData).unread ?? 0) > 0
    return [...children].sort((a, b) => Number(unread(b)) - Number(unread(a)) || key(b) - key(a))
  },

  present(entity, lens) {
    if (entity.type === 'slack.conversation') {
      const data = { channel: entity.id.slice('slack:conv:'.length), ...entity.data } as unknown as ConversationData
      return { ...entity, data: { ...entity.data, title: conversationTitle(lens, data) } }
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
        reactions: (data.reactions ?? []).map((reaction) => ({
          emoji: emoji(reaction.name),
          count: reaction.count,
          mine: reaction.users.includes(String(self ?? '')),
        })),
      },
    }
  },
}
