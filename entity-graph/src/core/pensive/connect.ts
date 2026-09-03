import type { AppEvent } from '../events'
import type { Pensive, ResourceRecord, ToolMeta } from './types'

/**
 * A pensive on another machine, reached over HTTP.
 *
 * The other half of a broadcast: a URL and a bearer token in, a pensive out. It
 * holds no store of its own — every call is forwarded — so a combiner can join a
 * local SQLite file to somebody else's laptop and neither end has to know.
 *
 * Tools are forwarded rather than rebuilt, which matters for the ones the remote
 * store defines itself: a tool written as a note over there is callable from
 * here, with the schema it published.
 */
export class ConnectPensive implements Pensive {
  private baseUrl: string
  private cache: ToolMeta[] = []

  constructor(
    readonly id: string,
    readonly label: string,
    baseUrl: string,
    private token: string,
  ) {
    this.baseUrl = baseUrl.trim().replace(/\/+$/, '')
  }

  private headers(json = false): Record<string, string> {
    return {
      ...(this.token ? { authorization: `Bearer ${this.token}` } : {}),
      ...(json ? { 'content-type': 'application/json' } : {}),
    }
  }

  /**
   * What a failed request means, since `fetch` will not say. Every way of not
   * getting a request out at all arrives as the same bare `TypeError`, so the
   * message names the URL that could not be reached rather than passing that on.
   */
  private unreachable(e: unknown): Error {
    const why = e instanceof Error ? e.message : String(e)
    return new Error(`Can't reach ${this.baseUrl} — ${why}`)
  }

  /** Fetch and keep the remote tool list. */
  async refresh(): Promise<void> {
    const res = await fetch(`${this.baseUrl}/tools`, { headers: this.headers() }).catch((e) => {
      throw this.unreachable(e)
    })
    if (!res.ok) throw new Error(`${this.baseUrl}/tools answered ${res.status}`)
    this.cache = (await res.json()) as ToolMeta[]
  }

  async listTools(): Promise<ToolMeta[]> {
    if (!this.cache.length) await this.refresh()
    return this.cache
  }

  async callTool(toolId: string, args: unknown): Promise<unknown> {
    const res = await fetch(`${this.baseUrl}/call`, {
      method: 'POST',
      headers: this.headers(true),
      body: JSON.stringify({ tool: toolId, args: args ?? {} }),
    }).catch((e) => {
      throw this.unreachable(e)
    })
    const text = await res.text()
    const body = (text ? JSON.parse(text) : {}) as
      | { status: 'success'; result: unknown }
      | { status: 'error'; message: string }
    if (body.status === 'success') return body.result
    throw new Error(body.message ?? `${this.baseUrl} refused "${toolId}" (${res.status})`)
  }

  // The five calls, each of which is one of the tools the other end publishes.
  // Written out rather than generated so the types are the interface's own.

  readEvents(entityIds?: string[]): Promise<AppEvent[]> {
    return this.callTool('readEvents', { entityIds }) as Promise<AppEvent[]>
  }

  async writeEvents(events: AppEvent[]): Promise<void> {
    await this.callTool('writeEvents', { events })
  }

  popEvents(windowMs: number): Promise<AppEvent[]> {
    return this.callTool('popEvents', { windowMs }) as Promise<AppEvent[]>
  }

  readResource(id: string): Promise<ResourceRecord | null> {
    return this.callTool('readResource', { id }) as Promise<ResourceRecord | null>
  }

  async writeResource(resource: ResourceRecord): Promise<void> {
    await this.callTool('writeResource', resource)
  }
}
