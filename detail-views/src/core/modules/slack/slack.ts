import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { loadedKey } from '../../types.ts'
import type { Entity, ItemType, LoadPart } from '../../types.ts'
import type { Module, ModuleContext } from '../module.ts'
import { SlackApi } from './api.ts'
import {
  maxTs,
  parseMessageId,
  quiet,
  slackIds as ids,
  slackView,
  tsMillis,
  type ConversationData,
  type ImageData,
  type MessageData,
} from './view.ts'

const author = 'slack'

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
  edited?: { ts?: string }
  reply_count?: number
  latest_reply?: string
  files?: RawFile[]
  reactions?: { name: string; count: number; users?: string[] }[]
}

interface RawFile {
  id: string
  name?: string
  mimetype?: string
  url_private?: string
  thumb_360?: string
  thumb_480?: string
  thumb_360_w?: number
  thumb_360_h?: number
  thumb_480_w?: number
  thumb_480_h?: number
}

interface ConversationInfo extends RawConversation {
  last_read?: string
  unread_count_display?: number
  latest?: { ts?: string } | string
}

function kindOf(conversation: RawConversation): ConversationData['kind'] {
  return conversation.is_im ? 'im' : conversation.is_mpim ? 'mpim' : conversation.is_private ? 'private' : 'channel'
}

/** A conversation's name and shape: no date to it, so timestamp 0, where anything of mine overrides it. */
function conversationEvents(conversation: RawConversation): AppEvent[] {
  const id = ids.conversation(conversation.id)
  const events = values(
    id,
    { type: 'slack.conversation', channel: conversation.id, kind: kindOf(conversation), name: conversation.name ?? null, user: conversation.user ?? null },
    0,
    author,
  )
  // Noted on the user, so a mention of them can open our DM.
  if (conversation.is_im && conversation.user) events.push(value(ids.user(conversation.user), 'dm', id, 0, author))
  return events
}

/**
 * A message as events. What was said is written at the time it was said (or
 * last edited), so a later edit of mine wins over it; what changes without a
 * date (reactions, the thread under it) at 0. Marked loaded: this is all of it.
 */
function messageEvents(channel: string, raw: RawMessage, now: number): AppEvent[] {
  const id = ids.message(channel, raw.ts)
  const isImage = (file: RawFile) => Boolean(file.mimetype?.startsWith('image/') && file.url_private)
  const files = (raw.files ?? []).filter((file) => !isImage(file)).map((file) => file.name).filter(Boolean)
  const images = (raw.files ?? []).filter(isImage).map(
    (file): ImageData => ({
      id: file.id,
      name: file.name ?? file.id,
      full: file.url_private!,
      thumb: file.thumb_480 ?? file.thumb_360 ?? file.url_private!,
      width: file.thumb_480_w ?? file.thumb_360_w,
      height: file.thumb_480_h ?? file.thumb_360_h,
    }),
  )
  const said = tsMillis(raw.edited?.ts ?? raw.ts)
  const data: Omit<MessageData, 'replyCount' | 'latestReply' | 'reactions'> = {
    channel,
    ts: raw.ts,
    threadTs: raw.thread_ts,
    user: raw.user,
    botName: raw.bot_profile?.name ?? raw.username,
    subtype: raw.subtype,
    text: [raw.text ?? '', ...files.map((name) => `[${name}]`)].filter(Boolean).join('\n'),
    images: images.length ? images : undefined,
  }
  return [
    value(id, 'type', 'slack.message', 0, author),
    ...values(id, Object.fromEntries(Object.entries(data).map(([key, v]) => [key, v ?? null])), said, author),
    ...values(
      id,
      {
        replyCount: raw.reply_count ?? null,
        latestReply: raw.latest_reply ?? null,
        reactions: raw.reactions?.map(({ name, count, users }) => ({ name, count, users: users ?? [] })) ?? null,
      },
      0,
      author,
    ),
    value(id, loadedKey('self'), now, 0, author),
  ]
}

export class Slack implements Module {
  readonly view = slackView

  private readonly context: ModuleContext
  private api: SlackApi | null = null
  private readonly downloads = new Map<string, Promise<{ mime: string; data: Uint8Array }>>()

  constructor(context: ModuleContext) {
    this.context = context
    const token = context.settings.get('slack.token')
    if (token) this.api = new SlackApi(token, context.fetch)
  }

  private get cache() {
    return this.context.cache
  }

  private data<T>(id: string): Partial<T> {
    return (this.context.lens.read(id)?.data ?? {}) as Partial<T>
  }

  async load(id: string, part: LoadPart, type: ItemType): Promise<void> {
    if (type === 'slack.home') return this.loadHome()
    const api = this.api
    if (!api) throw new Error('no Slack token')
    const channel = id.startsWith('slack:conv:') ? id.slice('slack:conv:'.length) : null
    const message = parseMessageId(id)
    if (type === 'slack.conversation' && channel) {
      return part === 'self' ? this.loadInfo(api, channel) : this.loadHistory(api, channel)
    }
    if (type === 'slack.message' && message) {
      return part === 'self' ? this.loadMessage(api, message.channel, message.ts) : this.loadThread(api, id)
    }
    if (type === 'slack.user') return this.loadUser(api, id.slice('slack:user:'.length))
  }

  async submit(entity: Entity, text: string): Promise<AppEvent[]> {
    if (!this.api) {
      await this.setToken(text.trim())
      return []
    }
    if (entity.type === 'slack.conversation') {
      const { channel } = entity.data as unknown as ConversationData
      await this.api.call('chat.postMessage', { channel, text })
    } else if (entity.type === 'slack.message') {
      const { channel, ts, threadTs } = entity.data as unknown as MessageData
      await this.api.call('chat.postMessage', { channel, text, thread_ts: threadTs ?? ts })
    } else return []
    await this.context.load(entity.id, 'children', true)
    return []
  }

  /** Checks the token before keeping it, so a typo is caught at once. */
  async setToken(token: string): Promise<void> {
    const api = new SlackApi(token, this.context.fetch)
    const auth = await api.call<{ user_id: string; url: string }>('auth.test')
    this.context.settings.set('slack.token', token)
    this.context.settings.set('slack.self', auth.user_id)
    this.context.settings.set('slack.url', auth.url)
    this.api = api
    await this.context.load(ids.root, 'children', true)
  }

  /**
   * An image attached to a message, from the cache or from Slack. `ref` is
   * `<size>/<message id>/<file id>`, as handed out by `present`.
   */
  async image(ref: string): Promise<{ mime: string; data: Uint8Array }> {
    const cached = this.context.blobs.get(`slack:image:${ref}`)
    if (cached) return cached
    const [size, ...rest] = ref.split('/')
    const fileId = rest.pop()
    const image = this.data<MessageData>(rest.join('/')).images?.find((candidate) => candidate.id === fileId)
    if (!image || !this.api) throw new Error('image not found')
    const pending = this.downloads.get(ref)
    if (pending) return pending
    const download = this.api
      .download(size === 'full' ? image.full : image.thumb, 'image/')
      .then((blob) => {
        this.context.blobs.put(`slack:image:${ref}`, blob)
        return blob
      })
      .finally(() => this.downloads.delete(ref))
    this.downloads.set(ref, download)
    return download
  }

  /** Marks a conversation read up to its newest message, in Slack too. */
  async markRead(id: string): Promise<AppEvent[]> {
    const conversation = this.context.lens.read(id)
    if (!this.api || conversation?.type !== 'slack.conversation') return []
    const data = conversation.data as unknown as ConversationData
    const newest = maxTs(
      data.latestTs,
      ...this.context.lens.children(id).map((child) => this.data<MessageData>(child).ts),
    )
    if (!newest) return []
    await this.api.call('conversations.mark', { channel: data.channel, ts: newest })
    this.cache.write(values(id, { lastRead: newest, unread: 0 }, 0, author))
    return []
  }

  /** Every conversation I am in. Their unread counts load one by one as each is shown. */
  private async loadHome(): Promise<void> {
    if (!this.api) {
      this.cache.write([value(ids.root, 'connected', false, 0, author)])
      return
    }
    const raw = await this.api.paginate<RawConversation>('users.conversations', 'channels', {
      types: 'public_channel,private_channel,mpim,im',
      exclude_archived: true,
      limit: 200,
    })
    const live = raw.filter((conversation) => !conversation.is_user_deleted)
    this.cache.write(
      [
        ...values(ids.root, { connected: true, self: this.context.settings.get('slack.self') }, 0, author),
        ...live.flatMap(conversationEvents),
        ...live.map((conversation) => link(ids.root, ids.conversation(conversation.id), 0, author)),
      ],
      { replaceLinksFrom: [ids.root] },
    )
  }

  /**
   * Where a conversation stands: what it is, how far I have read, and how
   * much is unread. Slack has no call for every unread count at once, so this
   * runs per conversation, as each is shown, behind anything urgent.
   */
  private async loadInfo(api: SlackApi, channelId: string): Promise<void> {
    const id = ids.conversation(channelId)
    const { channel } = await api.call<{ channel: ConversationInfo }>('conversations.info', { channel: channelId })
    const lastRead = channel.last_read
    const latest = typeof channel.latest === 'object' ? channel.latest?.ts : undefined
    let unread = channel.unread_count_display
    let newest: string | undefined
    if (unread === undefined && lastRead) {
      const self = this.context.settings.get('slack.self')
      const { messages } = await api.call<{ messages: RawMessage[] }>('conversations.history', {
        channel: channelId,
        oldest: lastRead,
        limit: 100,
      })
      unread = messages.filter((message) => message.user !== self && !(message.subtype && quiet.has(message.subtype))).length
      newest = messages[0]?.ts
    }
    const known = this.data<ConversationData>(id)
    this.cache.write([
      ...conversationEvents({ ...channel, id: channelId }),
      ...values(id, { lastRead: lastRead ?? null, unread: unread ?? 0, latestTs: maxTs(known.latestTs, latest, newest) ?? null }, 0, author),
    ])
  }

  /** The latest messages. Older ones already loaded stay, so scrolling back doesn't lose them. */
  private async loadHistory(api: SlackApi, channel: string): Promise<void> {
    const id = ids.conversation(channel)
    const { messages } = await api.urgent.call<{ messages: RawMessage[] }>('conversations.history', { channel, limit: 100 })
    const now = this.context.now()
    this.cache.write([
      ...messages.flatMap((message) => [
        ...messageEvents(channel, message, now),
        link(id, ids.message(channel, message.ts), tsMillis(message.ts), author),
      ]),
      value(id, 'latestTs', maxTs(this.data<ConversationData>(id).latestTs, messages[0]?.ts) ?? null, 0, author),
    ])
  }

  /** The replies under a message, which become its children. */
  private async loadThread(api: SlackApi, id: string): Promise<void> {
    const data = this.data<MessageData>(id)
    const { channel, ts } = parseMessageId(id)!
    const root = data.threadTs ?? ts
    const messages = await api.urgent.paginate<RawMessage>('conversations.replies', 'messages', { channel, ts: root, limit: 200 })
    const now = this.context.now()
    const replies = messages.filter((message) => message.ts !== root)
    const parent = messages.find((message) => message.ts === root && root === ts)
    this.cache.write(
      [
        ...(parent ? messageEvents(channel, parent, now) : []),
        ...replies.flatMap((message) => [
          ...messageEvents(channel, message, now),
          link(id, ids.message(channel, message.ts), tsMillis(message.ts), author),
        ]),
      ],
      { replaceLinksFrom: [id] },
    )
  }

  /** One message seen only by its id: linked from a task, say, after the cache was cleared. */
  private async loadMessage(api: SlackApi, channel: string, ts: string): Promise<void> {
    const { messages } = await api.urgent.call<{ messages: RawMessage[] }>('conversations.history', {
      channel,
      latest: ts,
      inclusive: true,
      limit: 1,
    })
    let found = messages.find((message) => message.ts === ts)
    if (!found) {
      const thread = await api.urgent.call<{ messages: RawMessage[] }>('conversations.replies', { channel, ts, limit: 1 })
      found = thread.messages.find((message) => message.ts === ts)
    }
    if (!found) throw new Error('message not found')
    this.cache.write(messageEvents(channel, found, this.context.now()))
  }

  private async loadUser(api: SlackApi, user: string): Promise<void> {
    // Urgent: a name is wanted wherever someone is mentioned, and should not wait behind unread counts.
    const { user: raw } = await api.urgent.call<{
      user: { name: string; real_name?: string; profile?: { display_name?: string; real_name?: string } }
    }>('users.info', { user })
    const name = raw.profile?.display_name || raw.profile?.real_name || raw.real_name || raw.name
    this.cache.write(values(ids.user(user), { type: 'slack.user', name }, 0, author))
  }
}
