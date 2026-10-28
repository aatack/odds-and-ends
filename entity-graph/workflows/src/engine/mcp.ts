// The app's MCP server. Whoever connects is an agent: a bot's run, at
// /mcp/<runId> on a free loopback port, or anything else (Claude Code, say), at
// /mcp on a fixed loopback port behind a bearer token. Both get the same tools:
// every action but the unsafe ones, which no agent is ever given (they are the
// human's, through approvals). A tool that is about an item takes its id, so an
// agent may work on any item, its own included. The run scope differs only in
// tying each call to its run, for the trace.
//
// Stateless: a fresh server and transport per request.

import { createServer, type IncomingMessage, type RequestListener, type Server } from 'node:http'
import type { AddressInfo } from 'node:net'
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import type { Catalog } from '../shared/types'
import type { Engine } from './engine'
import type { ActionContext } from './integrations'
import { LEGACY_NAMES, LEGACY_PARAMS } from './actions'
import { PARADIGM } from './paradigm'
import { objectSchema } from './schema'

export type Scope = { kind: 'run'; runId: string } | { kind: 'operator' }


export function makeServer(engine: Engine, scope: Scope): McpServer {
  const server = new McpServer({ name: 'workflows', version: '0.1.0' }, { instructions: PARADIGM })
  const ctx = (): ActionContext => {
    if (scope.kind === 'operator') return { engine, author: 'bot', actor: 'mcp', itemId: null, runId: null }
    const run = engine.activeRun(scope.runId)
    if (!run) throw new Error('This run has ended.')
    // What a run does is recorded as its bot's, and its calls are traced in the bot (a run may span items).
    return { engine, author: 'bot', actor: `bot:${run.agentId}`, itemId: run.agentId, runId: scope.runId }
  }
  // The unsafe rule: no agent is given an unsafe action.
  const offered = engine.actions.filter((a) => !a.unsafe)
  // A tool's old parameters are taken too: what they name is then optional here, and required by callAction.
  const inputOf = (action: (typeof offered)[number]) => {
    const old = Object.entries(LEGACY_PARAMS[action.name] ?? {})
    if (!old.length) return action.input
    const input = { ...action.input }
    for (const [from, to] of old) {
      input[to] = input[to].optional()
      input[from] = input[to].describe(`The old name for ${to}.`)
    }
    return input
  }
  const register = (name: string, action: (typeof offered)[number]) =>
    server.registerTool(
      name,
      { description: action.description, inputSchema: inputOf(action) },
      (async (args: Record<string, unknown>) => {
        try {
          const out = await engine.callAction(name, args, ctx())
          return { content: [{ type: 'text' as const, text: typeof out === 'string' ? out : JSON.stringify(out, null, 1) }] }
        } catch (e) {
          return { content: [{ type: 'text' as const, text: (e as Error).message }], isError: true }
        }
      }) as never,
    )
  for (const action of offered) register(action.name, action)
  // The old names answer too, but are not listed.
  const hidden = new Set<string>()
  for (const [old, now] of Object.entries(LEGACY_NAMES)) {
    const action = offered.find((a) => a.name === now)
    if (!action) continue
    register(old, action)
    hidden.add(old)
  }
  const handlers = (server.server as unknown as { _requestHandlers: Map<string, (...a: unknown[]) => Promise<{ tools: { name: string }[] }>> })._requestHandlers
  const list = handlers.get('tools/list')
  if (list) handlers.set('tools/list', async (...a) => {
    const out = await list(...a)
    return { ...out, tools: out.tools.filter((t) => !hidden.has(t.name)) }
  })
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
    events: engine.itemTypes.map((t) => ({ type: t.type, description: t.description, params: objectSchema(t.params), data: t.data, blocking: t.blocking })),
    presets: engine.presets.map(({ id, name, hint, icon, whenEmpty }) => ({ id, name, hint, icon, whenEmpty })),
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
