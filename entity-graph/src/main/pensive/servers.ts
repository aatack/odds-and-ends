import net from 'net'
import type { NodeStatus } from '../../core/client'
import { PensiveServer } from './http'
import type { GraphDb } from './graph'
import type { PensiveRegistry } from './registry'

// The listeners, kept in step with the drawing. A broadcast or MCP node exists
// ⇔ a server is up on its port, so adding one starts a server and deleting one
// stops it; pausing does neither, since a paused node answers 403 rather than
// nothing at all — "somebody switched this off" is worth being able to tell from
// "there is nothing here".

/** Grab a port nothing is using, for a node the user has just added. */
export function findFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const probe = net.createServer()
    probe.on('error', reject)
    probe.listen(0, '127.0.0.1', () => {
      const { port } = probe.address() as net.AddressInfo
      probe.close(() => resolve(port))
    })
  })
}

export class PensiveServers {
  private servers = new Map<string, PensiveServer>()

  constructor(
    private db: GraphDb,
    private registry: PensiveRegistry,
  ) {}

  /** Start, stop and re-port servers until they match the graph. */
  async sync(): Promise<void> {
    const wanted = new Map(
      this.db
        .nodes()
        .filter((n) => n.config.kind === 'broadcast' || n.config.kind === 'mcp')
        .map((n) => [n.id, n]),
    )

    // A node whose port is unchanged keeps the server it has: nothing about the
    // node is held there, since the pensive and the tokens are read per request.
    for (const [id, server] of [...this.servers]) {
      const node = wanted.get(id)
      const port =
        node && (node.config.kind === 'broadcast' || node.config.kind === 'mcp')
          ? node.config.port
          : null
      if (port === server.port) continue
      await server.stop()
      this.servers.delete(id)
    }

    for (const [id, node] of wanted) {
      if (this.servers.has(id)) continue
      if (node.config.kind !== 'broadcast' && node.config.kind !== 'mcp') continue
      const server = new PensiveServer({
        kind: node.config.kind,
        port: node.config.port,
        // Read per request rather than captured: pausing a node, revoking a
        // token or re-plugging an edge takes effect on the next call.
        node: () => this.db.node(id) ?? null,
        authorOf: (token) => this.db.authorOf(id, token),
        pensive: () => this.registry.tryGet(id),
      })
      this.servers.set(id, server)
      await server.start()
    }
  }

  /** Where a node answers and what is wrong with it, for the page to draw. */
  status(nodeId: string): NodeStatus {
    const server = this.servers.get(nodeId)
    const problem = this.registry.problem(nodeId)
    if (!server) return { url: null, localUrl: null, problem }
    // Both are loopback, and equal, while everything published is local-only:
    // a node reached from another machine is a kind that doesn't exist yet, and
    // the two fields are what will tell it apart from these when it does.
    return {
      url: server.listening ? server.url : null,
      localUrl: server.listening ? server.url : null,
      problem: server.problem ?? problem,
    }
  }

  /** The loopback URL of one node's server — what a tailnet mount proxies to. */
  localUrl(nodeId: string): string | null {
    const server = this.servers.get(nodeId)
    return server?.listening ? server.url : null
  }

  async stopAll(): Promise<void> {
    await Promise.all([...this.servers.values()].map((s) => s.stop()))
    this.servers.clear()
  }
}
