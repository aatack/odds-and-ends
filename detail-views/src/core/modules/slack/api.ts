type Params = Record<string, string | number | boolean | undefined>

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

/**
 * One token bucket for every method, well under Slack's lowest tier, plus
 * whatever back-off Slack asks for.
 */
class RateLimiter {
  private tokens: number
  private readonly capacity: number
  private readonly perMs: number
  private last = Date.now()
  private blockedUntil = 0

  constructor(perMinute: number) {
    this.capacity = perMinute
    this.tokens = Math.min(perMinute, 10)
    this.perMs = perMinute / 60_000
  }

  backOff(seconds: number): void {
    this.blockedUntil = Math.max(this.blockedUntil, Date.now() + seconds * 1000)
  }

  async take(): Promise<void> {
    for (;;) {
      const now = Date.now()
      this.tokens = Math.min(this.capacity, this.tokens + (now - this.last) * this.perMs)
      this.last = now
      if (this.blockedUntil > now) {
        await sleep(this.blockedUntil - now)
        continue
      }
      if (this.tokens >= 1) {
        this.tokens -= 1
        return
      }
      await sleep(Math.ceil((1 - this.tokens) / this.perMs))
    }
  }
}

/**
 * Everything the app may call while Slack is read-only. Anything else, which
 * includes every method that writes, is refused before it leaves the machine.
 */
const reads = new Set([
  'auth.test',
  'users.conversations',
  'users.info',
  'conversations.info',
  'conversations.history',
  'conversations.replies',
])

/** Off while the app is being developed, so nothing is sent by accident. */
export const slackWrites = false

export class SlackError extends Error {
  readonly code: string
  constructor(method: string, code: string) {
    super(`${method}: ${code}`)
    this.code = code
  }
}

export class SlackApi {
  private readonly token: string
  private readonly fetch: typeof fetch
  private readonly limiter = new RateLimiter(90)

  constructor(token: string, fetchImpl: typeof fetch) {
    this.token = token
    this.fetch = fetchImpl
  }

  async call<T>(method: string, params: Params = {}): Promise<T> {
    if (!slackWrites && !reads.has(method)) throw new SlackError(method, 'Slack is read-only')
    const body = new URLSearchParams()
    for (const [key, value] of Object.entries(params)) if (value !== undefined) body.set(key, String(value))
    for (let attempt = 0; attempt < 4; attempt += 1) {
      await this.limiter.take()
      const response = await this.fetch(`https://slack.com/api/${method}`, {
        method: 'POST',
        headers: { authorization: `Bearer ${this.token}`, 'content-type': 'application/x-www-form-urlencoded' },
        body,
      })
      if (response.status === 429) {
        this.limiter.backOff(Number.parseInt(response.headers.get('retry-after') ?? '30', 10) || 30)
        continue
      }
      const parsed = (await response.json()) as { ok: boolean; error?: string } & T
      if (parsed.ok) return parsed
      if (parsed.error === 'ratelimited') {
        this.limiter.backOff(30)
        continue
      }
      throw new SlackError(method, parsed.error ?? 'failed')
    }
    throw new SlackError(method, 'ratelimited')
  }

  /**
   * Downloads a private file. The token only ever goes to Slack's own hosts,
   * and an answer that isn't the kind of file asked for (Slack's sign-in
   * page, when a scope is missing) is refused.
   */
  async download(url: string, expect: string): Promise<{ mime: string; data: Uint8Array }> {
    const { protocol, hostname } = new URL(url)
    if (protocol !== 'https:' || !(hostname === 'slack.com' || hostname.endsWith('.slack.com') || hostname.endsWith('.slack-edge.com'))) {
      throw new SlackError('download', `refusing to send the token to ${hostname}`)
    }
    await this.limiter.take()
    const response = await this.fetch(url, { headers: { authorization: `Bearer ${this.token}` } })
    const mime = response.headers.get('content-type')?.split(';')[0] ?? ''
    if (!response.ok) throw new SlackError('download', String(response.status))
    if (!mime.startsWith(expect)) throw new SlackError('download', `got ${mime || 'nothing'}; is files:read granted?`)
    return { mime, data: new Uint8Array(await response.arrayBuffer()) }
  }

  /** Follows `response_metadata.next_cursor` to the end. */
  async paginate<T>(method: string, key: string, params: Params = {}): Promise<T[]> {
    const items: T[] = []
    let cursor: string | undefined
    do {
      const page = await this.call<Record<string, unknown> & { response_metadata?: { next_cursor?: string } }>(method, {
        ...params,
        cursor,
      })
      items.push(...((page[key] as T[] | undefined) ?? []))
      cursor = page.response_metadata?.next_cursor || undefined
    } while (cursor)
    return items
  }
}
