import { createServer, type Server } from 'node:http'
import type { AppEvent, Entity, QueryResult } from '../src/core/types'

// An in-memory stand-in for a source, over the same HTTP contract the real server
// exposes: `POST /:sourceId/call` with `{ tool, args }`, answering
// `{ status, result }`. Enough of the tools for the client to be exercised against
// something that behaves like the store — including the ordering rules, which are
// the part of the client worth testing.
//
// A stand-in rather than the real server because the real one needs better-sqlite3,
// which on this machine is built for Electron's ABI at any given moment; a test that
// rebuilt it would break the desktop app it shares the install with.

const rollup = (id: string, events: AppEvent[]): Entity => {
  const sorted = [...events].sort((a, b) => a.timestamp - b.timestamp)
  const values: Record<string, unknown> = {}
  const outbound: string[] = []
  const inbound = new Map<string, boolean>()
  let createdAt = Infinity
  let editedAt = -Infinity

  for (const e of sorted) {
    createdAt = Math.min(createdAt, e.timestamp)
    editedAt = Math.max(editedAt, e.timestamp)
    if (e.type === 'value') {
      values[e.key] = e.value
      continue
    }
    if (e.sourceId === id) {
      const at = outbound.indexOf(e.destinationId)
      if (e.action === 0 && at === -1) outbound.push(e.destinationId)
      else if (e.action === 1 && at !== -1) outbound.splice(at, 1)
      else if (e.action === 2 && at > 0) {
        outbound.splice(at, 1)
        outbound.splice(at - 1, 0, e.destinationId)
      } else if (e.action === 3 && at !== -1 && at < outbound.length - 1) {
        outbound.splice(at, 1)
        outbound.splice(at + 1, 0, e.destinationId)
      }
    }
    if (e.destinationId === id) inbound.set(e.sourceId, e.action === 0)
  }

  return {
    id,
    createdAt: Number.isFinite(createdAt) ? createdAt : 0,
    editedAt: Number.isFinite(editedAt) ? editedAt : 0,
    createdBy: '',
    editedBy: '',
    values,
    outboundLinks: outbound,
    inboundLinks: [...inbound].filter(([, live]) => live).map(([from]) => from),
  }
}

export class MemorySource {
  events: AppEvent[] = []
  /** How many tool calls have been served — what "with no round trip" is measured by. */
  calls = 0

  private touching(id: string): AppEvent[] {
    return this.events.filter((e) =>
      e.type === 'value' ? e.entityId === id : e.sourceId === id || e.destinationId === id,
    )
  }

  entity(id: string): Entity {
    return rollup(id, this.touching(id))
  }

  /** Depth-first, cycle-guarded by the path — the same traversal the server does. */
  query(rootId: string, direction: 'out' | 'in' = 'out'): QueryResult[] {
    const out: QueryResult[] = []
    const walk = (id: string, depth: number, parentId: string | null, path: string[]): void => {
      const entity = this.entity(id)
      out.push({ entity, depth, parentId })
      const links = direction === 'in' ? entity.inboundLinks : entity.outboundLinks
      for (const child of links) {
        if (!path.includes(child)) walk(child, depth + 1, id, [...path, child])
      }
    }
    walk(rootId, 0, null, [rootId])
    return out
  }

  call(tool: string, args: any): unknown {
    this.calls++
    switch (tool) {
      case 'query':
        return { results: this.query(args.rootId, args.direction ?? 'out'), continuationStack: null }
      case 'readEntities':
        return Object.fromEntries((args.entityIds as string[]).map((id) => [id, this.entity(id)]))
      case 'writeValue':
        this.events.push({
          type: 'value',
          timestamp: args.timestamp ?? Date.now(),
          author: args.author ?? 'test',
          entityId: args.entityId,
          key: args.key,
          value: args.value ?? null,
        })
        return { ok: true }
      case 'writeLink':
        this.events.push({
          type: 'link',
          timestamp: args.timestamp ?? Date.now(),
          author: args.author ?? 'test',
          sourceId: args.sourceId,
          destinationId: args.destinationId,
          action: args.action ?? 0,
        })
        return { ok: true }
      case 'writeEvents':
        this.events.push(...(args.events as AppEvent[]))
        return { written: args.events.length }
      case 'popEvents': {
        // The real store takes the most recent event and anything within `windowMs`
        // of it, which is what makes one user action one undo step.
        if (!this.events.length) return []
        const latest = Math.max(...this.events.map((e) => e.timestamp))
        const window = args.windowMs ?? 100
        const taken = this.events.filter((e) => e.timestamp >= latest - window)
        this.events = this.events.filter((e) => e.timestamp < latest - window)
        return taken.sort((a, b) => a.timestamp - b.timestamp)
      }
      default:
        throw new Error(`No tool with id "${tool}"`)
    }
  }
}

export interface Harness {
  source: MemorySource
  baseUrl: string
  close: () => Promise<void>
}

/** Serve a {@link MemorySource} on a free port. */
export async function serve(source: MemorySource, token: string): Promise<Harness> {
  const server: Server = createServer((req, res) => {
    const send = (code: number, body: unknown): void => {
      res.writeHead(code, { 'content-type': 'application/json' })
      res.end(JSON.stringify(body))
    }
    if (req.headers.authorization !== `Bearer ${token}`) return send(401, { error: 'nope' })

    if (req.method === 'GET' && req.url?.endsWith('/tools')) {
      return send(
        200,
        ['query', 'readEntities', 'writeValue', 'writeLink', 'writeEvents', 'popEvents'].map(
          (id) => ({ id, name: id, description: '', safety: 'pure', args: {} }),
        ),
      )
    }

    let body = ''
    req.on('data', (chunk) => (body += chunk))
    req.on('end', () => {
      try {
        const { tool, args } = JSON.parse(body || '{}')
        send(200, { status: 'success', result: source.call(tool, args ?? {}) })
      } catch (e) {
        send(200, { status: 'error', message: e instanceof Error ? e.message : String(e) })
      }
    })
  })

  await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve))
  const port = (server.address() as { port: number }).port
  return {
    source,
    baseUrl: `http://127.0.0.1:${port}`,
    close: () => new Promise<void>((resolve) => server.close(() => resolve())),
  }
}
