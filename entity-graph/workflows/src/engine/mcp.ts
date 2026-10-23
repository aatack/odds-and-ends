// The app's MCP server, in two scopes:
//
// - run: a worker's bot, at /mcp/<runId> on a free loopback port. It gets every
//   action but the unsafe ones, and may change only its own item.
// - operator: anything else the user connects, at /mcp on a fixed loopback
//   port, behind a bearer token. It acts as the human.
//
// Stateless: a fresh server and transport per request.

import { createServer, type IncomingMessage, type RequestListener, type Server } from 'node:http'
import type { AddressInfo } from 'node:net'
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import type { Catalog } from '../shared/types'
import type { Engine } from './engine'
import type { ActionContext } from './integrations'
import { objectSchema } from './schema'

export type Scope = { kind: 'run'; runId: string } | { kind: 'operator' }

const UNSAFE = ' UNSAFE: it changes things outside this app, so only a human may run it.'

export function makeServer(engine: Engine, scope: Scope): McpServer {
  const server = new McpServer({ name: 'workflows', version: '0.1.0' })
  const ctx = (): ActionContext => {
    if (scope.kind === 'operator') return { engine, author: 'human', actor: 'mcp', itemId: null, runId: null }
    const run = engine.activeRun(scope.runId)
    if (!run) throw new Error('This run has ended.')
    return { engine, author: 'bot', actor: 'bot', itemId: run.itemId, runId: scope.runId }
  }
  // Unsafe actions are not offered to a bot at all.
  const offered = scope.kind === 'run' ? engine.actions.filter((a) => !a.unsafe) : engine.actions
  for (const action of offered)
    server.registerTool(
      action.name,
      { description: action.description + (action.unsafe ? UNSAFE : ''), inputSchema: action.input },
      (async (args: Record<string, unknown>) => {
        try {
          const out = await engine.callAction(action.name, args, ctx())
          return { content: [{ type: 'text' as const, text: typeof out === 'string' ? out : JSON.stringify(out, null, 1) }] }
        } catch (e) {
          return { content: [{ type: 'text' as const, text: (e as Error).message }], isError: true }
        }
      }) as never,
    )
  return server
}

/** Every action, event type and preset, for the MCP page and the chat input. */
export function catalog(engine: Engine): Catalog {
  return {
    actions: engine.actions.map((a) => ({
      name: a.name,
      description: a.description,
      inputSchema: objectSchema(a.input),
      unsafe: a.unsafe,
    })),
    events: engine.eventTypes.map((t) => ({ type: t.type, description: t.description, params: objectSchema(t.params), data: t.data, blocking: t.blocking })),
    presets: engine.presets.map(({ id, name, hint, icon }) => ({ id, name, hint, icon })),
  }
}

async function readBody(req: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = []
  for await (const c of req) chunks.push(c as Buffer)
  const raw = Buffer.concat(chunks).toString('utf8')
  return raw ? JSON.parse(raw) : undefined
}

/** `scopeOf` answers the scope a request may use, or the status to refuse it with. */
function listener(engine: Engine, scopeOf: (req: IncomingMessage) => Scope | number): RequestListener {
  return async (req, res) => {
    const scope = scopeOf(req)
    if (typeof scope === 'number') {
      res.writeHead(scope).end()
      return
    }
    try {
      const server = makeServer(engine, scope)
      const transport = new StreamableHTTPServerTransport({ sessionIdGenerator: undefined })
      res.on('close', () => {
        void transport.close()
        void server.close()
      })
      await server.connect(transport)
      await transport.handleRequest(req, res, req.method === 'POST' ? await readBody(req) : undefined)
    } catch (e) {
      if (!res.headersSent) res.writeHead(500).end((e as Error).message)
    }
  }
}

const listen = (server: Server, port: number) =>
  new Promise<void>((resolve, reject) => {
    server.once('error', reject)
    server.listen(port, '127.0.0.1', () => {
      server.off('error', reject)
      resolve()
    })
  })

export interface Mcp {
  /** Where a run's bot reaches its scope. */
  runUrl: (runId: string) => string
  /** Where anything else reaches the operator scope, or why it can't. */
  operator: () => { url: string; error: string | null }
  /** Listens again, after the port or token changed. */
  restartOperator: () => Promise<void>
  close: () => void
}

/** Starts the bots' server on a free port, and the operator's on the configured one. */
export async function startMcp(engine: Engine): Promise<Mcp> {
  const settings = engine.settings
  const runs = createServer(
    listener(engine, (req) => {
      const m = /^\/mcp\/([\w-]+)$/.exec(req.url ?? '')
      return m && engine.activeRun(m[1]) ? { kind: 'run', runId: m[1] } : 404
    }),
  )
  await listen(runs, 0)
  const runPort = (runs.address() as AddressInfo).port

  let operator: Server | null = null
  let error: string | null = null
  const restartOperator = async () => {
    if (operator) await new Promise((r) => operator!.close(r))
    operator = createServer(
      listener(engine, (req) =>
        req.url !== '/mcp' ? 404 : req.headers.authorization === `Bearer ${settings.mcpToken()}` ? { kind: 'operator' } : 401,
      ),
    )
    try {
      await listen(operator, settings.mcpPort())
      error = null
    } catch (e) {
      operator = null
      error = `Could not listen on port ${settings.mcpPort()}: ${(e as Error).message}`
    }
  }
  await restartOperator()

  return {
    runUrl: (runId) => `http://127.0.0.1:${runPort}/mcp/${runId}`,
    operator: () => {
      const port = operator ? (operator.address() as AddressInfo).port : settings.mcpPort()
      return { url: `http://127.0.0.1:${port}/mcp`, error }
    },
    restartOperator,
    close: () => {
      runs.close()
      operator?.close()
    },
  }
}
