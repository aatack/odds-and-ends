import type { IntegrationStatus } from '@conswap/common/types'
import type { Context, Integration } from '../context.js'
import { readJson, readKey, writeJson, writeKey } from '../database.js'
import { addEvent } from '../lifecycle.js'
import { getTopic, updateTopic, upsertTopic } from '../topics.js'

interface SlackMessage {
  type?: string
  subtype?: string
  ts: string
  thread_ts?: string
  user?: string
  bot_id?: string
  username?: string
  text?: string
  reply_count?: number
  latest_reply?: string
  files?: { name?: string }[]
  attachments?: { fallback?: string; text?: string }[]
}

interface SlackChannel {
  id: string
  name?: string
  is_im?: boolean
  is_mpim?: boolean
  is_private?: boolean
  user?: string
}

/**
 * A token bucket. Slack's limits are per method and per workspace; one shared
 * bucket well under the lowest tier is simpler and never gets us throttled.
 */
class RateLimiter {
  private tokens: number
  private readonly capacity: number
  private readonly perSecond: number
  private last = Date.now()
  private blockedUntil = 0

  constructor(requestsPerMinute: number) {
    this.capacity = Math.max(requestsPerMinute, 1)
    this.tokens = this.capacity
    this.perSecond = this.capacity / 60
  }

  /** Slack asked us to back off; nothing goes out until then. */
  backOff(seconds: number): void {
    this.blockedUntil = Math.max(this.blockedUntil, Date.now() + seconds * 1000)
  }

  async take(): Promise<void> {
    for (;;) {
      const nowMs = Date.now()
      this.tokens = Math.min(this.capacity, this.tokens + ((nowMs - this.last) / 1000) * this.perSecond)
      this.last = nowMs
      const waitForBackOff = this.blockedUntil - nowMs
      if (waitForBackOff > 0) {
        await sleep(Math.min(waitForBackOff, 5000))
        continue
      }
      if (this.tokens >= 1) {
        this.tokens -= 1
        return
      }
      await sleep(Math.ceil(((1 - this.tokens) / this.perSecond) * 1000))
    }
  }
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

class SlackApi {
  constructor(
    private readonly token: string,
    private readonly limiter: RateLimiter,
  ) {}

  async call<T>(method: string, params: Record<string, string | number | boolean | undefined>): Promise<T> {
    const url = new URL(`https://slack.com/api/${method}`)
    for (const [key, value] of Object.entries(params)) {
      if (value !== undefined) url.searchParams.set(key, String(value))
    }
    for (let attempt = 0; attempt < 4; attempt += 1) {
      await this.limiter.take()
      const response = await fetch(url, { headers: { authorization: `Bearer ${this.token}` } })
      if (response.status === 429) {
        this.limiter.backOff(Number.parseInt(response.headers.get('retry-after') ?? '30', 10) || 30)
        continue
      }
      const body = (await response.json()) as { ok: boolean; error?: string } & T
      if (!body.ok) {
        if (body.error === 'ratelimited') {
          this.limiter.backOff(30)
          continue
        }
        throw new Error(`slack ${method}: ${body.error ?? 'failed'}`)
      }
      return body
    }
    throw new Error(`slack ${method}: gave up after being rate limited`)
  }

  async post(method: string, body: Record<string, unknown>): Promise<Record<string, unknown>> {
    await this.limiter.take()
    const response = await fetch(`https://slack.com/api/${method}`, {
      method: 'POST',
      headers: { authorization: `Bearer ${this.token}`, 'content-type': 'application/json; charset=utf-8' },
      body: JSON.stringify(body),
    })
    const parsed = (await response.json()) as { ok: boolean; error?: string }
    if (!parsed.ok) throw new Error(`slack ${method}: ${parsed.error ?? 'failed'}`)
    return parsed as Record<string, unknown>
  }
}

interface ChannelCursor {
  /** Oldest message timestamp we have already filed. */
  ts: string
  /** Set while we are still working backwards through a gap. */
  catchingUp?: boolean
}

export class SlackIntegration implements Integration {
  readonly name = 'slack'

  private api: SlackApi | null = null
  private timer: NodeJS.Timeout | null = null
  private running = false
  private state: IntegrationStatus['state'] = 'disabled'
  private detail = 'no SLACK_USER_TOKEN'
  private lastRunAt: string | null = null
  private selfId: string | null = null
  private teamDomain: string | null = null
  private channelIndex = 0

  constructor(private readonly context: Context) {
    const token = context.config.slack.token
    if (token) {
      this.api = new SlackApi(token, new RateLimiter(context.config.slack.requestsPerMinute))
      this.state = 'idle'
      this.detail = 'not started'
    }
  }

  status(): IntegrationStatus {
    return {
      name: this.name,
      enabled: this.api !== null,
      state: this.state,
      detail: this.detail,
      lastRunAt: this.lastRunAt,
    }
  }

  start(): void {
    if (!this.api || this.timer) return
    const interval = this.context.config.slack.pollSeconds * 1000
    this.timer = setInterval(() => void this.poll(), interval)
    void this.poll()
  }

  stop(): void {
    if (this.timer) clearInterval(this.timer)
    this.timer = null
  }

  /** Posts as me, and files the message straight away so the topic reads in order. */
  async send(topicId: string, text: string): Promise<void> {
    if (!this.api) throw new Error('Slack is not configured')
    const topic = getTopic(this.context.db, topicId)
    if (!topic) throw new Error('no such topic')
    const metadata = topic.metadata as { channelId?: string; threadTs?: string; ts?: string }
    const channelId = metadata.channelId ?? this.findChannel(topicId)
    if (!channelId) throw new Error('this topic is not attached to a Slack conversation')
    const threadTs = metadata.threadTs ?? (topic.type === 'slack_message' ? metadata.ts : undefined)
    const posted = (await this.api.post('chat.postMessage', {
      channel: channelId,
      text,
      ...(threadTs ? { thread_ts: threadTs } : {}),
    })) as { ts?: string }
    const ts = typeof posted.ts === 'string' ? posted.ts : String(Date.now() / 1000)
    addEvent(
      this.context,
      topicId,
      {
        id: `slack:msg:${channelId}:${ts}`,
        type: 'slack_message',
        text,
        metadata: {
          channelId,
          channelName: this.channelName(channelId),
          ts,
          threadTs,
          userName: 'me',
          fromMe: true,
          permalink: this.permalink(channelId, ts, threadTs),
        },
      },
      { silent: true },
    )
  }

  /** Walks up from a message topic to whatever channel it belongs to. */
  private findChannel(topicId: string): string | null {
    const row = this.context.db
      .prepare(
        `SELECT topics.metadata AS metadata FROM links
         JOIN topics ON topics.id = links.parent_id
         WHERE links.child_id = ? AND topics.type IN ('slack_channel', 'slack_message') LIMIT 1`,
      )
      .get(topicId) as { metadata: string } | undefined
    if (!row) return null
    try {
      const metadata = JSON.parse(row.metadata) as { channelId?: string }
      return metadata.channelId ?? null
    } catch {
      return null
    }
  }

  private async poll(): Promise<void> {
    if (!this.api || this.running) return
    this.running = true
    this.state = 'polling'
    try {
      if (!this.selfId) await this.identify()
      await this.refreshChannels()
      await this.readChannels()
      this.state = 'idle'
      this.detail = `${this.channels().length} conversations`
      this.lastRunAt = new Date().toISOString()
    } catch (error) {
      this.state = 'error'
      this.detail = String(error instanceof Error ? error.message : error)
      this.context.log('slack', 'poll failed', this.detail)
    } finally {
      this.running = false
    }
  }

  private async identify(): Promise<void> {
    const body = await (this.api as SlackApi).call<{ user_id: string; url: string; user: string }>('auth.test', {})
    this.selfId = body.user_id
    this.teamDomain = new URL(body.url).host
    writeKey(this.context.db, 'slack:self', body.user_id)
    writeKey(this.context.db, 'slack:host', this.teamDomain)
  }

  private channels(): SlackChannel[] {
    return readJson<SlackChannel[]>(this.context.db, 'slack:channels', [])
  }

  private async refreshChannels(): Promise<void> {
    const lastRefresh = Number.parseInt(readKey(this.context.db, 'slack:channels:at') ?? '0', 10)
    if (Date.now() - lastRefresh < 600_000) return
    const all: SlackChannel[] = []
    let cursor: string | undefined
    do {
      const body = await (this.api as SlackApi).call<{
        channels: SlackChannel[]
        response_metadata?: { next_cursor?: string }
      }>('users.conversations', {
        types: 'public_channel,private_channel,mpim,im',
        exclude_archived: true,
        limit: 200,
        cursor,
      })
      all.push(...body.channels)
      cursor = body.response_metadata?.next_cursor || undefined
    } while (cursor)
    writeJson(this.context.db, 'slack:channels', all)
    writeKey(this.context.db, 'slack:channels:at', String(Date.now()))
  }

  /**
   * Reads a slice of the conversation list each tick, oldest cursor first, so that
   * a busy workspace never spends the whole rate limit on one channel.
   */
  private async readChannels(): Promise<void> {
    const channels = this.channels()
    if (channels.length === 0) return
    const budget = Math.min(channels.length, 8)
    for (let taken = 0; taken < budget; taken += 1) {
      const channel = channels[this.channelIndex % channels.length] as SlackChannel
      this.channelIndex = (this.channelIndex + 1) % channels.length
      try {
        await this.readChannel(channel)
      } catch (error) {
        this.context.log('slack', `could not read ${channel.id}`, String(error))
      }
    }
  }

  private cursorKey(channelId: string): string {
    return `slack:cursor:${channelId}`
  }

  private async readChannel(channel: SlackChannel): Promise<void> {
    const key = this.cursorKey(channel.id)
    const stored = readJson<ChannelCursor | null>(this.context.db, key, null)
    // A conversation seen for the first time starts an hour ago, so that opening
    // the app does not file a year of history.
    const cursor: ChannelCursor = stored ?? { ts: String((Date.now() - 3_600_000) / 1000) }

    const body = await (this.api as SlackApi).call<{ messages: SlackMessage[]; has_more?: boolean }>(
      'conversations.history',
      { channel: channel.id, oldest: cursor.ts, limit: 100, inclusive: false },
    )
    const messages = [...body.messages].sort((left, right) => Number(left.ts) - Number(right.ts))
    let newest = cursor.ts
    for (const message of messages) {
      if (Number(message.ts) <= Number(cursor.ts)) continue
      this.file(channel, message)
      if (Number(message.ts) > Number(newest)) newest = message.ts
    }
    // `has_more` means there is still a gap: keep the cursor at the newest thing
    // we filed and come back for the rest next tick.
    writeJson(this.context.db, key, { ts: newest, catchingUp: body.has_more === true })

    for (const message of messages) {
      if ((message.reply_count ?? 0) > 0) await this.readThread(channel, message)
    }
  }

  private async readThread(channel: SlackChannel, parent: SlackMessage): Promise<void> {
    const key = `slack:thread:${channel.id}:${parent.ts}`
    const seen = readKey(this.context.db, key)
    if (seen && parent.latest_reply && Number(seen) >= Number(parent.latest_reply)) return
    const body = await (this.api as SlackApi).call<{ messages: SlackMessage[] }>('conversations.replies', {
      channel: channel.id,
      ts: parent.ts,
      oldest: seen ?? parent.ts,
      limit: 100,
    })
    let newest = seen ?? parent.ts
    for (const message of body.messages) {
      if (message.ts === parent.ts) continue
      if (Number(message.ts) <= Number(newest)) continue
      this.file(channel, message)
      newest = message.ts
    }
    writeKey(this.context.db, key, newest)
  }

  private channelName(channelId: string): string {
    const channel = this.channels().find((entry) => entry.id === channelId)
    if (!channel) return channelId
    if (channel.is_im) return `@${this.userName(channel.user ?? '')}`
    return `#${channel.name ?? channelId}`
  }

  private userName(userId: string): string {
    if (!userId) return 'someone'
    const cached = readKey(this.context.db, `slack:user:${userId}`)
    if (cached) return cached
    // Resolved lazily in the background so a poll is never held up by names.
    void this.resolveUser(userId)
    return userId
  }

  private async resolveUser(userId: string): Promise<void> {
    if (!this.api) return
    if (readKey(this.context.db, `slack:user:${userId}`)) return
    try {
      const body = await this.api.call<{ user: { name: string; profile?: { display_name?: string; real_name?: string } } }>(
        'users.info',
        { user: userId },
      )
      const name = body.user.profile?.display_name || body.user.profile?.real_name || body.user.name
      writeKey(this.context.db, `slack:user:${userId}`, name)
    } catch {
      writeKey(this.context.db, `slack:user:${userId}`, userId)
    }
  }

  private permalink(channelId: string, ts: string, threadTs?: string): string | undefined {
    if (!this.teamDomain) return undefined
    const path = `https://${this.teamDomain}/archives/${channelId}/p${ts.replace('.', '')}`
    return threadTs ? `${path}?thread_ts=${threadTs}&cid=${channelId}` : path
  }

  /**
   * One message becomes one topic. A reply hangs off the topic for the message it
   * is replying to, which is exactly what a thread is here.
   */
  private file(channel: SlackChannel, message: SlackMessage): void {
    if (message.subtype === 'channel_join' || message.subtype === 'channel_leave') return
    const channelTopicId = `slack:channel:${channel.id}`
    const channelName = this.channelName(channel.id)
    upsertTopic(this.context.db, {
      id: channelTopicId,
      type: 'slack_channel',
      text: channelName,
      metadata: { channelId: channel.id, channelName, isDirect: channel.is_im === true },
      open: false,
    })

    let parentId = channelTopicId
    if (message.thread_ts && message.thread_ts !== message.ts) {
      const threadParentId = `slack:msg:${channel.id}:${message.thread_ts}`
      const existing = getTopic(this.context.db, threadParentId)
      if (!existing) {
        upsertTopic(this.context.db, {
          id: threadParentId,
          type: 'slack_message',
          text: 'a thread',
          metadata: { channelId: channel.id, channelName, ts: message.thread_ts },
          open: false,
        })
        addEvent(this.context, channelTopicId, { id: threadParentId }, { silent: true })
      }
      parentId = threadParentId
    }

    const fromMe = message.user !== undefined && message.user === this.selfId
    const authorId = message.user ?? ''
    const author = fromMe ? 'me' : message.username ?? this.userName(authorId)
    addEvent(
      this.context,
      parentId,
      {
        id: `slack:msg:${channel.id}:${message.ts}`,
        type: 'slack_message',
        text: renderMessage(message),
        createdAt: new Date(Number(message.ts) * 1000).toISOString(),
        metadata: {
          channelId: channel.id,
          channelName,
          ts: message.ts,
          threadTs: message.thread_ts,
          user: authorId,
          userName: author,
          fromMe,
          permalink: this.permalink(channel.id, message.ts, message.thread_ts),
        },
      },
      { silent: fromMe },
    )

    // A channel only shows up as something to look at once it has something in it.
    const channelTopic = getTopic(this.context.db, channelTopicId)
    if (channelTopic && channelTopic.text !== channelName) {
      updateTopic(this.context.db, channelTopicId, { text: channelName })
    }
  }
}

function renderMessage(message: SlackMessage): string {
  const parts: string[] = []
  if (message.text) parts.push(message.text)
  for (const attachment of message.attachments ?? []) {
    const line = attachment.text ?? attachment.fallback
    if (line) parts.push(line)
  }
  for (const file of message.files ?? []) {
    if (file.name) parts.push(`[file] ${file.name}`)
  }
  return parts.join('\n\n').trim() || '(no text)'
}
