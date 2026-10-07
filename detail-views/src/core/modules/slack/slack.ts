import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { loadedKey } from '../../types.ts'
import type { Entity, ItemType, LoadPart } from '../../types.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { SlackApi } from './api.ts'
import {
  maxTs,
  parseMessageId,
  slackIds as ids,
  slackView,
  tsMillis,
  type ConversationData,
  type ImageData,
  type MessageData,
} from './view.ts'

const author = 'slack'

/** How often the watch asks Slack for what is new. Search is Tier 2: 20 calls a minute. */
const pollEvery = 15_000
/**
 * How far before the newest message seen the watch looks again, in seconds:
 * search indexes a message a little after it is sent, so one sent just before
 * another can turn up after it.
 */
const overlap = 120
/** The most pages one search reads: Slack's own limit. Past this the gap is too big to fill from search. */
const maxPages = 100
/** How many messages one batch back through history brings in: the first on start, more on demand. */
const batch = 1000

interface SearchMatch {
  ts: string
  user?: string
  username?: string
  text?: string
  permalink?: string
  files?: RawFile[]
  channel: RawConversation
}

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

interface RawUser {
  id: string
  name: string
  real_name?: string
  profile?: { display_name?: string; real_name?: string }
}

function userName(user: RawUser): string {
  return user.profile?.display_name || user.profile?.real_name || user.real_name || user.name
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
 *
 * A message from search is `partial`: search leaves out reactions and replies,
 * so those are not written, rather than written as nothing.
 *
 * A message with replies is a thread. Its newest reply is written at that
 * reply's time, so the thread's own newest event, which orders it in the
 * workspace, is that reply. It is listed in the workspace only when its
 * conversation is (`listed`): unlinking a conversation drops its threads too.
 */
function messageEvents(
  channel: string,
  raw: RawMessage,
  now: number,
  options: { partial?: boolean; listed?: boolean } = {},
): AppEvent[] {
  const { partial = false, listed = false } = options
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
    ...(partial
      ? []
      : [
          ...values(
            id,
            {
              replyCount: raw.reply_count ?? null,
              reactions: raw.reactions?.map(({ name, count, users }) => ({ name, count, users: users ?? [] })) ?? null,
            },
            0,
            author,
          ),
          value(id, 'latestReply', raw.latest_reply ?? null, raw.latest_reply ? tsMillis(raw.latest_reply) : 0, author),
          ...(raw.reply_count && listed ? [link(ids.root, id, tsMillis(raw.ts), author)] : []),
        ]),
    value(id, loadedKey('self'), now, 0, author),
  ]
}

export class Slack implements Module {
  readonly view = slackView

  private readonly context: ModuleContext
  private api: SlackApi | null = null
  private polling = false
  /** Searches run one at a time: each moves cursors the next one reads. */
  private searching: Promise<unknown> = Promise.resolve()
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
    const message = parseMessageId(id)
    if (type === 'slack.message' && message && part === 'self') return this.loadMessage(api, message.channel, message.ts)
    if (type === 'slack.user') return this.loadUser(api, id.slice('slack:user:'.length))
  }

  /**
   * Loads further back, on demand: the workspace one batch further back
   * through every conversation, a conversation 100 messages further back, a
   * thread whole. Nothing else loads history except the first batch.
   */
  async older(id: string): Promise<void> {
    const api = this.api
    if (!api) throw new Error('no Slack token')
    const type = this.view.typeOf(id)
    if (type === 'slack.home') {
      await this.serially(() => this.searchBack(api))
      return
    }
    if (type === 'slack.conversation') return this.channelBack(api, id.slice('slack:conv:'.length))
    if (type === 'slack.message' && parseMessageId(id)) return this.loadThread(api, id)
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
    const newest = maxTs(...this.context.lens.children(id).map((child) => this.data<MessageData>(child).ts))
    if (!newest) return []
    await this.api.call('conversations.mark', { channel: data.channel, ts: newest })
    return []
  }

  // --- The watch and the history behind it ----------------------------------------

  /**
   * Keeps Slack current while the app runs, so no conversation is polled.
   * Returns the stop.
   */
  watch(): () => void {
    void this.poll()
    const timer = setInterval(() => void this.poll(), pollEvery)
    return () => clearInterval(timer)
  }

  private serially<T>(body: () => Promise<T>): Promise<T> {
    const run = this.searching.then(body, body)
    this.searching = run.catch(() => {})
    return run
  }

  private cursor(id: string, key: string): string | null {
    const found = this.data<Record<string, unknown>>(id)[key]
    return typeof found === 'string' ? found : null
  }

  /**
   * One search, newest first, collecting what `keep` accepts until `enough`
   * says stop or the results run out. Says whether it got to the end.
   */
  /** The conversations in the workspace now, as I see it (my own unlinks included). */
  private listed(): Set<string> {
    return new Set(this.context.lens.children(ids.root).filter((id) => id.startsWith('slack:conv:')))
  }

  /**
   * Hiding a chat is unlinking it from the workspace, as an owned event. A
   * conversation takes its threads with it, and no thread of it is listed
   * again (`listed`).
   */
  unlink(parent: string, child: string): AppEvent[] | null {
    if (parent !== ids.root) return null
    const now = this.context.now()
    const threads = child.startsWith('slack:conv:')
      ? this.context.lens.children(ids.root).filter((id) => id.startsWith(`slack:msg:${child.slice('slack:conv:'.length)}:`))
      : []
    return [child, ...threads].map((id) => link(ids.root, id, now, me, 1))
  }

  /**
   * When the watch last looked and how much it found, on an entity of its own
   * (`slack:watch`), so writing it every poll re-reads that and nothing else.
   */
  private watchEvents(added: number): AppEvent[] {
    return [...values(ids.watch, { type: 'slack.watch', polledAt: this.context.now(), found: added }, 0, author)]
  }

  private noteWatch(added: number): void {
    this.cache.write(this.watchEvents(added))
  }

  private async search(
    api: SlackApi,
    query: string,
    keep: (match: SearchMatch) => 'keep' | 'skip' | 'stop',
    enough: (found: SearchMatch[]) => boolean,
    from = 1,
  ): Promise<{ found: SearchMatch[]; ended: boolean; stopped: boolean; next: number }> {
    const found: SearchMatch[] = []
    for (let page = from; page <= maxPages; page++) {
      const { messages } = await api.urgent.call<{ messages: { matches: SearchMatch[]; paging: { pages: number } } }>(
        'search.messages',
        { query, sort: 'timestamp', sort_dir: 'desc', count: 100, page },
      )
      for (const match of messages.matches) {
        const verdict = keep(match)
        if (verdict === 'stop') return { found, ended: false, stopped: true, next: page }
        if (verdict === 'keep') found.push(match)
      }
      // Whole pages only, so the next batch can start on the page after.
      if (page >= messages.paging.pages) return { found, ended: true, stopped: false, next: page + 1 }
      if (enough(found)) return { found, ended: false, stopped: false, next: page + 1 }
    }
    return { found, ended: false, stopped: false, next: maxPages + 1 }
  }

  /**
   * One look at what is new: every message since the newest one seen
   * (`watch.at`, on the workspace), across all conversations at once. On a new
   * cache store there is no `watch.at`, so this is instead the first batch
   * back through history, which sets it.
   *
   * On start this is also the catch-up on what came while the app was closed.
   * If that is more than search will page through, what is cached is no longer
   * unbroken back to the global oldest cursor: that cursor moves up to where
   * the catch-up ended, and every conversation's own cursor is dropped.
   */
  async poll(): Promise<void> {
    const api = this.api
    if (!api || this.polling) return
    this.polling = true
    try {
      await this.serially(async () => {
        // The lists come first, so a thread knows whether its conversation is listed.
        // A no-op while they are fresh.
        await this.context.load(ids.root, 'children')
        const cursor = this.cursor(ids.root, 'watch.at')
        if (!cursor) return this.noteWatch(await this.searchBack(api))
        const since = Number(cursor) - overlap
        // `after:` takes a day and excludes it; two back covers any time zone.
        const after = new Date((since - 2 * 86_400) * 1000).toISOString().slice(0, 10)
        const { found, ended, stopped } = await this.search(
          api,
          `after:${after}`,
          (match) => (Number(match.ts) <= since ? 'stop' : 'keep'),
          () => false,
        )
        const { events, added } = this.searchEvents(found)
        events.push(...this.watchEvents(added))
        if (!ended && !stopped) {
          const oldest = found.reduce((least, match) => (Number(match.ts) < Number(least) ? match.ts : least), cursor)
          events.push(value(ids.root, 'history.oldest', oldest, 0, author), value(ids.root, 'history.query', null, 0, author))
          for (const id of this.context.lens.children(ids.root)) events.push(value(id, 'history.oldest', null, 0, author))
        }
        events.push(value(ids.root, 'watch.at', maxTs(cursor, ...found.map((match) => match.ts))!, 0, author), value(ids.root, 'watch.error', null, 0, author))
        this.cache.write(events)
      })
    } catch (error) {
      this.cache.write([value(ids.root, 'watch.error', error instanceof Error ? error.message : String(error), 0, author)])
    } finally {
      this.polling = false
    }
  }

  /**
   * One batch further back through every conversation at once: the
   * `batch` messages before the global oldest cursor (`history.oldest`, on the
   * workspace), or before now for the first. Everything from that cursor to
   * now is then cached, in every conversation, which is what lets a
   * conversation's own history start from it rather than from now.
   */
  /** Returns how many messages it brought in. */
  private async searchBack(api: SlackApi): Promise<number> {
    const oldest = this.cursor(ids.root, 'history.oldest')
    const start = oldest ?? (this.context.now() / 1000).toFixed(6)
    // Carry on with the last batch's search from the page after it, while
    // search will still page that far; otherwise start one from the cursor.
    // `before:` takes a day and excludes it; two on covers any time zone, and
    // what that brings in from after the cursor is skipped. New messages
    // pushing results down a page only bring back ones already seen.
    const resumed = oldest ? this.cursor(ids.root, 'history.query') : null
    const page = Number(this.data<Record<string, unknown>>(ids.root)['history.page']) || 1
    const fresh = !resumed || page > maxPages
    const query = fresh ? `before:${new Date((Number(start) + 2 * 86_400) * 1000).toISOString().slice(0, 10)}` : resumed
    const { found, ended, next } = await this.search(
      api,
      query,
      (match) => (Number(match.ts) < Number(start) ? 'keep' : 'skip'),
      (kept) => kept.length >= batch,
      fresh ? 1 : page,
    )
    const reached = found.reduce((least, match) => (Number(match.ts) < Number(least) ? match.ts : least), start)
    const { events, added } = this.searchEvents(found)
    events.push(
      value(ids.root, 'history.oldest', reached, 0, author),
      value(ids.root, 'history.query', query, 0, author),
      value(ids.root, 'history.page', next, 0, author),
    )
    if (ended) events.push(value(ids.root, 'history.complete', true, 0, author))
    if (!this.cursor(ids.root, 'watch.at')) events.push(value(ids.root, 'watch.at', maxTs(start, ...found.map((match) => match.ts))!, 0, author))
    this.cache.write(events)
    return added
  }

  /**
   * A conversation 100 messages further back. It starts from whichever is
   * older, its own cursor or the global one, since everything after the global
   * cursor is cached already.
   */
  private async channelBack(api: SlackApi, channel: string): Promise<void> {
    const id = ids.conversation(channel)
    const own = this.cursor(id, 'history.oldest')
    const global = this.cursor(ids.root, 'history.oldest')
    const starts = [own, global].filter((ts): ts is string => ts !== null)
    const start = starts.length ? starts.reduce((a, b) => (Number(a) < Number(b) ? a : b)) : undefined
    const { messages, has_more: more } = await api.urgent.call<{ messages: RawMessage[]; has_more?: boolean }>(
      'conversations.history',
      { channel, latest: start, limit: 100 },
    )
    const now = this.context.now()
    const reached = messages.reduce((least, message) => (Number(message.ts) < Number(least) ? message.ts : least), start ?? (now / 1000).toFixed(6))
    this.cache.write([
      ...messages.flatMap((message) => [
        ...messageEvents(channel, message, now, { listed: this.listed().has(id) }),
        link(id, ids.message(channel, message.ts), tsMillis(message.ts), author),
      ]),
      value(id, 'history.oldest', reached, 0, author),
      ...(more ? [] : [value(id, 'history.complete', true, 0, author)]),
    ])
  }

  /**
   * New messages from a search, as events. Ones already cached are left
   * alone, so a repeat counts nothing twice. A message's link is written at
   * its ts, which is what moves its conversation up the list.
   */
  private searchEvents(matches: SearchMatch[]): { events: AppEvent[]; added: number } {
    const { lens } = this.context
    const now = this.context.now()
    const events: AppEvent[] = []
    const conversations = new Map<string, Partial<ConversationData>>()
    const threads = new Map<string, { replyCount: number; latestReply?: string }>()

    const seen = new Set<string>()
    // Read once: the workspace's rollup is large. Conversations this batch adds join it.
    const listed = this.listed()
    for (const match of [...matches].sort((a, b) => Number(a.ts) - Number(b.ts))) {
      const channel = match.channel.id
      const id = ids.message(channel, match.ts)
      // New messages push results down a page, so one batch can meet a message twice.
      if (seen.has(id) || lens.read(id)?.data.ts) continue
      seen.add(id)
      const conversationId = ids.conversation(channel)
      let conversation = conversations.get(conversationId)
      if (!conversation) {
        conversation = this.data<ConversationData>(conversationId)
        conversations.set(conversationId, conversation)
        // A conversation not known yet: enough to show it; it loads whole when looked at.
        if (!conversation.channel) {
          events.push(
            ...values(
              conversationId,
              { type: 'slack.conversation', channel, kind: kindOf(match.channel), name: match.channel.is_im ? undefined : match.channel.name },
              0,
              author,
            ),
          )
          // Search also finds public channels I am not in. Only a DM, a group
          // DM or a private channel is surely mine, so only those join the list
          // here; a public one I join comes with the list's next load.
          const mine = match.channel.is_im || match.channel.is_mpim || match.channel.is_private
          if (mine) {
            events.push(link(ids.root, conversationId, 0, author))
            listed.add(conversationId)
          }
        }
      }

      const threadTs = match.permalink ? (new URL(match.permalink).searchParams.get('thread_ts') ?? undefined) : undefined
      const reply = Boolean(threadTs && threadTs !== match.ts)
      events.push(
        ...messageEvents(
          channel,
          { ts: match.ts, thread_ts: threadTs, user: match.user, username: match.username, text: match.text, files: match.files },
          now,
          { partial: true },
        ),
      )
      if (reply) {
        const parent = ids.message(channel, threadTs!)
        const known = this.data<MessageData>(parent)
        const thread = threads.get(parent) ?? { replyCount: known.replyCount ?? 0, latestReply: known.latestReply ?? undefined }
        threads.set(parent, { replyCount: thread.replyCount + 1, latestReply: maxTs(thread.latestReply, match.ts) })
        events.push(link(parent, id, tsMillis(match.ts), author))
        // The thread joins the workspace's list only if its conversation is there.
        // A parent not cached loads itself, once, when shown.
        if (listed.has(conversationId)) events.push(link(ids.root, parent, tsMillis(threadTs!), author))
      } else {
        events.push(link(conversationId, id, tsMillis(match.ts), author))
      }
    }

    for (const [id, thread] of threads) {
      events.push(
        value(id, 'replyCount', thread.replyCount, 0, author),
        value(id, 'latestReply', thread.latestReply ?? null, thread.latestReply ? tsMillis(thread.latestReply) : 0, author),
      )
    }
    return { events, added: seen.size }
  }

  // --- Loads ---------------------------------------------------------------------

  /**
   * The workspace's lists, each a call or a few: the conversations I am in
   * (which make the list), every public channel (so a mention of one has a
   * name) and every user (so a name never needs a call of its own). Nothing
   * here has a date, so all of it sits at 0 and none of it reorders the list.
   */
  private async loadHome(): Promise<void> {
    const api = this.api
    if (!api) {
      this.cache.write([value(ids.root, 'connected', false, 0, author)])
      return
    }
    const [mine, everyChannel, users] = await Promise.all([
      api.paginate<RawConversation>('users.conversations', 'channels', {
        types: 'public_channel,private_channel,mpim,im',
        exclude_archived: true,
        limit: 200,
      }),
      api.paginate<RawConversation>('conversations.list', 'channels', { types: 'public_channel', exclude_archived: true, limit: 1000 }),
      api.paginate<RawUser>('users.list', 'members', { limit: 200 }),
    ])
    const live = mine.filter((conversation) => !conversation.is_user_deleted)
    const now = this.context.now()
    this.cache.write(
      [
        ...values(ids.root, { connected: true, self: this.context.settings.get('slack.self') }, 0, author),
        ...everyChannel.flatMap(conversationEvents),
        ...users.flatMap((user) => [
          ...values(ids.user(user.id), { type: 'slack.user', name: userName(user) }, 0, author),
          value(ids.user(user.id), loadedKey('self'), now, 0, author),
        ]),
        ...live.flatMap(conversationEvents),
        ...live.map((conversation) => link(ids.root, ids.conversation(conversation.id), 0, author)),
      ],
      // Only the conversations: the threads listed here come from messages, not from this list.
      { replaceLinksFrom: [{ source: ids.root, within: 'slack:conv:' }] },
    )
  }

  /** A thread whole: the replies under a message, which become its children. On demand only. */
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
        ...(parent ? messageEvents(channel, parent, now, { listed: this.listed().has(ids.conversation(channel)) }) : []),
        ...replies.flatMap((message) => [
          ...messageEvents(channel, message, now),
          link(id, ids.message(channel, message.ts), tsMillis(message.ts), author),
        ]),
        value(id, 'history.complete', true, 0, author),
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
    this.cache.write(messageEvents(channel, found, this.context.now(), { listed: this.listed().has(ids.conversation(channel)) }))
  }

  private async loadUser(api: SlackApi, user: string): Promise<void> {
    // Only for someone `users.list` didn't name: from another workspace, say.
    const { user: raw } = await api.urgent.call<{ user: RawUser }>('users.info', { user })
    this.cache.write(values(ids.user(user), { type: 'slack.user', name: userName(raw) }, 0, author))
  }
}
