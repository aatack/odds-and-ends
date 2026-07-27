import { createServer, type Server } from 'node:http'
import { rollupEntity } from '../../src/core/entity'
import type { AppEvent, Entity } from '../src/core/types'

// An in-memory stand-in for a source, over the same HTTP contract the real server
// exposes: `POST /:sourceId/call` with `{ tool, args }`, answering
// `{ status, result }`. Enough of the tools for the client to be exercised against
// something that behaves like the store — including the ordering rules, which are
// the part of the client worth testing.
//
// A stand-in rather than the real server because the real one needs better-sqlite3,
// which on this machine is built for Electron's ABI at any given moment; a test that
// rebuilt it would break the desktop app it shares the install with. The *rollup* is
// the real one, though: the whole point of the client keeping raw events is that it
// folds them exactly as the server would, so a stand-in that folded them its own way
// would be testing the wrong thing.

/** How many layers of children `scanEvents` reads ahead, as the real one does. */
const SCAN_DEPTH = 2

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
    return rollupEntity(id, this.touching(id))
  }

  /**
   * Events for a set of entities plus two layers of their children, as the real
   * `scanEvents` reads them — which is the client's only read of the store.
   */
  scan(entityIds: string[]): { entityIds: string[]; events: AppEvent[] } {
    const covered = new Set<string>()
    let frontier = [...new Set(entityIds)]
    for (const id of frontier) covered.add(id)
    for (let layer = 0; frontier.length && layer < SCAN_DEPTH; layer++) {
      const next = new Set<string>()
      for (const id of frontier) {
        for (const child of this.entity(id).outboundLinks) if (!covered.has(child)) next.add(child)
      }
      frontier = [...next]
      for (const id of frontier) covered.add(id)
    }
    const ids = new Set(covered)
    return {
      entityIds: [...covered],
      events: this.events.filter((e) =>
        e.type === 'value' ? ids.has(e.entityId) : ids.has(e.sourceId) || ids.has(e.destinationId),
      ),
    }
  }

  call(tool: string, args: any): unknown {
    this.calls++
    switch (tool) {
      case 'scanEvents':
        return this.scan(args.entityIds)
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
        ['scanEvents', 'readEntities', 'writeValue', 'writeLink', 'writeEvents', 'popEvents'].map(
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
