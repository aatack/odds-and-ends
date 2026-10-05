import type { Store } from '../../store.ts'
import type { Entity } from '../../types.ts'
import type { Module, ModuleContext } from '../module.ts'
import { SlackApi } from './api.ts'

const hour = 60 * 60 * 1000
const day = 24 * hour

/** How long anything fetched from Slack lives in the cache. */
export const ttl = { conversation: day, message: day, user: 7 * day }

const ids = {
  root: 'slack',
  conversation: (channel: string) => `slack:conv:${channel}`,
  message: (channel: string, ts: string) => `slack:msg:${channel}:${ts}`,
  user: (user: string) => `slack:user:${user}`,
}

/** Messages that are not somebody saying something. */
const quiet = new Set(['channel_join', 'channel_leave', 'group_join', 'group_leave', 'bot_add', 'bot_remove'])

interface RawConversation {
  id: string
  name?: string
  is_im?: boolean
  is_mpim?: boolean
  is_private?: boolean
  is_user_deleted?: boolean
  user?: string
}

interface RawMessage {
  ts: string
  thread_ts?: string
  user?: string
  bot_id?: string
  username?: string
  bot_profile?: { name?: string }
  subtype?: string
  text?: string
  reply_count?: number
  latest_reply?: string
  files?: { name?: string }[]
}

interface ConversationInfo {
  last_read?: string
  unread_count_display?: number
  latest?: { ts?: string } | string
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
}

function maxTs(...values: (string | undefined)[]): string | undefined {
  let best: string | undefined
  for (const value of values) if (value && (!best || Number(value) > Number(best))) best = value
  return best
}

function messageData(channel: string, raw: RawMessage): MessageData {
  const files = (raw.files ?? []).map((file) => file.name).filter(Boolean)
  return {
    channel,
    ts: raw.ts,
    threadTs: raw.thread_ts,
    user: raw.user,
    botName: raw.bot_profile?.name ?? raw.username,
    subtype: raw.subtype,
    text: [raw.text ?? '', ...files.map((name) => `[${name}]`)].filter(Boolean).join('\n'),
    replyCount: raw.reply_count,
    latestReply: raw.latest_reply,
  }
}

export class Slack implements Module {
  readonly id = 'slack'
  readonly name = 'Slack'
  readonly root = { id: ids.root, type: 'slack.home' }

  private readonly store: Store
  private readonly context: ModuleContext
  private api: SlackApi | null = null
  private readonly lookups = new Set<string>()
  private counting: Promise<void> | null = null

  constructor(context: ModuleContext) {
    this.context = context
    this.store = context.store
    const token = this.store.getSetting('slack.token')
    if (token) this.api = new SlackApi(token, context.fetch)
  }

  owns(entity: Entity): boolean {
    return entity.type.startsWith('slack.')
  }

  compose(entity: Entity) {
    if (!this.api) return 'slack-token' as const
    return entity.type === 'slack.conversation' || entity.type === 'slack.message' ? ('slack' as const) : null
  }

  staleAfter(id: string): number {
    return id === ids.root ? 2 * 60_000 : 30_000
  }

  async refresh(id: string): Promise<void> {
    if (!this.api) return
    const entity = this.store.get(id)
    if (!entity) return
    if (entity.type === 'slack.home') await this.refreshHome(this.api)
    else if (entity.type === 'slack.conversation') await this.refreshConversation(this.api, entity.data as unknown as ConversationData)
    else if (entity.type === 'slack.message') await this.refreshThread(this.api, entity.data as unknown as MessageData)
  }

  async submit(entity: Entity, text: string): Promise<void> {
    if (!this.api) return this.setToken(text.trim())
    if (entity.type === 'slack.conversation') {
      const { channel } = entity.data as unknown as ConversationData
      await this.api.call('chat.postMessage', { channel, text })
    } else if (entity.type === 'slack.message') {
      const { channel, ts, threadTs } = entity.data as unknown as MessageData
      await this.api.call('chat.postMessage', { channel, text, thread_ts: threadTs ?? ts })
    } else return
    await this.refresh(entity.id)
  }

  /** Checks the token before keeping it, so a typo is caught at once. */
  async setToken(token: string): Promise<void> {
    const api = new SlackApi(token, this.context.fetch)
    const auth = await api.call<{ user_id: string; url: string }>('auth.test')
    this.store.setSetting('slack.token', token)
    this.store.setSetting('slack.self', auth.user_id)
    this.store.setSetting('slack.url', auth.url)
    this.api = api
    this.context.setError(ids.root, null)
    await this.refresh(ids.root)
  }

  /** Marks a conversation read up to its newest message, in Slack too. */
  async markRead(id: string): Promise<void> {
    const entity = this.store.get(id)
    if (!this.api || entity?.type !== 'slack.conversation') return
    const data = entity.data as unknown as ConversationData
    const newest = maxTs(data.latestTs, ...this.store.children(id).map((child) => (child.data as unknown as MessageData).ts))
    if (!newest) return
    await this.api.call('conversations.mark', { channel: data.channel, ts: newest })
    this.store.patch(id, { lastRead: newest, unread: 0 })
  }

  order(entity: Entity, children: Entity[]): Entity[] {
    if (entity.type !== 'slack.home') return children
    const key = (child: Entity) => {
      const data = child.data as unknown as ConversationData
      return Number(maxTs(data.latestTs, data.lastRead) ?? 0)
    }
    const unread = (child: Entity) => ((child.data as unknown as ConversationData).unread ?? 0) > 0
    return [...children].sort((a, b) => Number(unread(b)) - Number(unread(a)) || key(b) - key(a))
  }

  present(entity: Entity): Entity {
    if (entity.type === 'slack.conversation') {
      return { ...entity, data: { ...entity.data, title: this.conversationTitle(entity.data as unknown as ConversationData) } }
    }
    if (entity.type === 'slack.message') {
      const data = entity.data as unknown as MessageData
      return {
        ...entity,
        data: {
          ...data,
          author: data.user ? this.userName(data.user) : (data.botName ?? 'bot'),
          text: this.render(data.text),
          quiet: Boolean(data.subtype && quiet.has(data.subtype)),
        },
      }
    }
    return entity
  }

  private conversationTitle(data: ConversationData): string {
    if (data.kind === 'im') return data.user ? this.userName(data.user) : data.channel
    if (data.kind === 'mpim') {
      const names = (data.name ?? '').replace(/^mpdm-/, '').replace(/-\d+$/, '').split('--')
      return names.join(', ')
    }
    return `#${data.name ?? data.channel}`
  }

  /** Mentions, channel links and links rendered as text. */
  private render(text: string): string {
    return text
      .replace(/<@([UW][A-Z0-9]+)(?:\|[^>]*)?>/g, (_, user: string) => `@${this.userName(user)}`)
      .replace(/<#[A-Z0-9]+\|([^>]*)>/g, '#$1')
      .replace(/<!(here|channel|everyone)[^>]*>/g, '@$1')
      .replace(/<!subteam\^[A-Z0-9]+\|([^>]*)>/g, '$1')
      .replace(/<([^>|]+)\|([^>]+)>/g, '$2')
      .replace(/<([^>]+)>/g, '$1')
      .replace(/&lt;/g, '<')
      .replace(/&gt;/g, '>')
      .replace(/&amp;/g, '&')
  }

  /** Cached name, or the id while it is looked up in the background. */
  private userName(user: string): string {
    const cached = this.store.get(ids.user(user))
    if (cached) return String(cached.data.name)
    this.lookUp(user)
    return user
  }

  private lookUp(user: string): void {
    if (!this.api || this.lookups.has(user)) return
    this.lookups.add(user)
    const api = this.api
    void (async () => {
      try {
        const { user: raw } = await api.call<{
          user: { name: string; real_name?: string; profile?: { display_name?: string; real_name?: string } }
        }>('users.info', { user })
        const name = raw.profile?.display_name || raw.profile?.real_name || raw.real_name || raw.name
        this.store.put(ids.user(user), 'slack.user', { name }, { ttl: ttl.user })
      } catch {
        // Left as the id; tried again after a restart.
      }
    })()
  }

  private async refreshHome(api: SlackApi): Promise<void> {
    const raw = await api.paginate<RawConversation>('users.conversations', 'channels', {
      types: 'public_channel,private_channel,mpim,im',
      exclude_archived: true,
      limit: 200,
    })
    const live = raw.filter((conversation) => !conversation.is_user_deleted)
    this.store.transaction(() => {
      for (const conversation of live) {
        const id = ids.conversation(conversation.id)
        const previous = (this.store.get(id)?.data ?? {}) as Partial<ConversationData>
        const kind = conversation.is_im ? 'im' : conversation.is_mpim ? 'mpim' : conversation.is_private ? 'private' : 'channel'
        this.store.put(
          id,
          'slack.conversation',
          { ...previous, channel: conversation.id, kind, name: conversation.name, user: conversation.user },
          { ttl: ttl.conversation },
        )
      }
      this.store.setCachedChildren(
        ids.root,
        live.map((conversation) => ({ id: ids.conversation(conversation.id), rank: 0 })),
        { ttl: ttl.conversation },
      )
    })
    this.counting ??= this.countUnread(api).finally(() => {
      this.counting = null
    })
  }

  /**
   * Slack has no public call for every unread count at once, so this walks
   * the conversations one at a time, most likely to matter first, and each
   * answer lands in the view as it arrives.
   */
  private async countUnread(api: SlackApi): Promise<void> {
    const home = this.store.get(ids.root)
    if (!home) return
    for (const conversation of this.order(home, this.store.children(ids.root))) {
      try {
        await this.countOne(api, conversation.data as unknown as ConversationData)
      } catch (error) {
        this.context.setError(conversation.id, error instanceof Error ? error.message : String(error))
      }
    }
  }

  private async countOne(api: SlackApi, data: ConversationData): Promise<void> {
    const { channel } = await api.call<{ channel: ConversationInfo }>('conversations.info', { channel: data.channel })
    const lastRead = channel.last_read
    const latest = typeof channel.latest === 'object' ? channel.latest?.ts : undefined
    let unread = channel.unread_count_display
    let newest: string | undefined
    if (unread === undefined && lastRead) {
      const self = this.store.getSetting('slack.self')
      const { messages } = await api.call<{ messages: RawMessage[] }>('conversations.history', {
        channel: data.channel,
        oldest: lastRead,
        limit: 100,
      })
      const counted = messages.filter((message) => message.user !== self && !(message.subtype && quiet.has(message.subtype)))
      unread = counted.length
      newest = messages[0]?.ts
    }
    this.store.patch(
      ids.conversation(data.channel),
      { lastRead, unread: unread ?? 0, latestTs: maxTs(data.latestTs, latest, newest) },
      { ttl: ttl.conversation },
    )
  }

  private async refreshConversation(api: SlackApi, data: ConversationData): Promise<void> {
    const { messages } = await api.call<{ messages: RawMessage[] }>('conversations.history', {
      channel: data.channel,
      limit: 100,
    })
    this.storeMessages(ids.conversation(data.channel), data.channel, messages)
    this.store.patch(ids.conversation(data.channel), { latestTs: maxTs(data.latestTs, messages[0]?.ts) }, { ttl: ttl.conversation })
  }

  private async refreshThread(api: SlackApi, data: MessageData): Promise<void> {
    const messages = await api.paginate<RawMessage>('conversations.replies', 'messages', {
      channel: data.channel,
      ts: data.threadTs ?? data.ts,
      limit: 200,
    })
    const root = data.threadTs ?? data.ts
    this.storeMessages(
      ids.message(data.channel, data.ts),
      data.channel,
      messages.filter((message) => message.ts !== root),
    )
    const parent = messages.find((message) => message.ts === root)
    if (parent && parent.ts === data.ts) {
      this.store.put(ids.message(data.channel, data.ts), 'slack.message', { ...messageData(data.channel, parent) }, { ttl: ttl.message })
    }
  }

  private storeMessages(parent: string, channel: string, messages: RawMessage[]): void {
    this.store.transaction(() => {
      for (const message of messages) {
        this.store.put(ids.message(channel, message.ts), 'slack.message', { ...messageData(channel, message) }, { ttl: ttl.message })
      }
      this.store.setCachedChildren(
        parent,
        messages.map((message) => ({ id: ids.message(channel, message.ts), rank: Number(message.ts) })),
        { ttl: ttl.message },
      )
    })
  }
}
