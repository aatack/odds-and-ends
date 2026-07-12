import { z } from 'zod'
import type { Source, ToolDef } from './types'

interface RemoteToolMeta {
  id: string
  name: string
  description: string
  safety: ToolDef['safety']
  args: Record<string, unknown>
}

/**
 * Proxies to another source exposed over HTTP. Full tool passthrough: `call`
 * forwards to the remote `POST /call`, and `readEvents` is just one of the
 * forwarded tools, so Combined/Frozen compose over a Remote.
 *
 * Tool discovery is async (the remote must be queried); call `refresh()` before
 * relying on `tools()`. `call` works without a prior refresh.
 */
export class RemoteSource implements Source {
  private baseUrl: string
  private cache: ToolDef[] = []

  constructor(
    public id: string,
    public label: string,
    baseUrl: string,
    private token?: string
  ) {
    this.baseUrl = baseUrl.replace(/\/+$/, '')
  }

  private headers(extra: Record<string, string> = {}): Record<string, string> {
    return this.token ? { authorization: `Bearer ${this.token}`, ...extra } : extra
  }

  /** Fetch and cache the remote tool registry. */
  async refresh(): Promise<void> {
    const res = await fetch(`${this.baseUrl}/tools`, { headers: this.headers() })
    if (!res.ok) throw new Error(`remote /tools failed: ${res.status}`)
    const list = (await res.json()) as RemoteToolMeta[]
    this.cache = list.map((t) => ({
      id: t.id,
      name: t.name,
      description: t.description,
      safety: t.safety,
      args: z.any(),
      jsonSchema: t.args,
      handler: (args: unknown) => this.call(t.id, args),
    }))
  }

  tools(): ToolDef[] {
    return this.cache
  }

  async call(toolId: string, args: unknown): Promise<unknown> {
    const res = await fetch(`${this.baseUrl}/call`, {
      method: 'POST',
      headers: this.headers({ 'content-type': 'application/json' }),
      body: JSON.stringify({ tool: toolId, args: args ?? {} }),
    })
    const body = (await res.json()) as
      | { status: 'success'; result: unknown }
      | { status: 'error'; message: string }
    if (body.status === 'success') return body.result
    throw new Error(body.message ?? `remote call "${toolId}" failed`)
  }
}
