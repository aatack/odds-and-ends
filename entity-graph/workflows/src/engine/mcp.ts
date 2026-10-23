// The app's MCP server, in two scopes:
//
// - run: what a worker's agent sees, at /mcp/<runId> on a free loopback port.
//   Every call is about that run's item, and the run ends with one of the
//   END YOUR RUN tools.
// - operator: for connecting anything else (Claude Code, Claude Desktop), at
//   /mcp on a fixed loopback port, behind a bearer token. Item tools name their
//   item, and nothing ends a run.
//
// Stateless: a fresh server and transport per request.

import { createServer, type IncomingMessage, type RequestListener, type Server } from 'node:http'
import type { AddressInfo } from 'node:net'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { InMemoryTransport } from '@modelcontextprotocol/sdk/inMemory.js'
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import { z } from 'zod'
import type { Blocker, ToolInfo } from '../shared/types'
import type { AgentOutcome, Engine } from './engine'
import type { Settings } from './settings'

export type Scope = { kind: 'run'; runId: string } | { kind: 'operator' }

const text = (s: string) => ({ content: [{ type: 'text' as const, text: s }] })
const fail = (e: unknown) => ({ content: [{ type: 'text' as const, text: (e as Error).message }], isError: true })

const state = z.string().describe('The current state of the item, in a few lines: where it is and what is left. Replaces the last one.')
const BLOCKERS = ['operator', 'agent', 'external', 'items', 'closed'] as const

export function makeServer(engine: Engine, scope: Scope): McpServer {
  const server = new McpServer({ name: 'workflows', version: '0.1.0' })
  const run = scope.kind === 'run'
  /** The item a call is about: the run's own, or the one it names. */
  const itemShape = (run ? {} : { item_id: z.string().describe('The id of the item.') }) as { item_id: z.ZodString }
  const itemOf = (args: { item_id?: string }) => {
    if (scope.kind === 'operator') {
      if (!engine.get(args.item_id!)) throw new Error(`No item ${args.item_id}.`)
      return args.item_id!
    }
    const r = engine.activeRun(scope.runId)
    if (!r) throw new Error('This run has ended.')
    return r.itemId
  }
  const actor = run ? 'agent' : 'mcp'

  const tool = <S extends z.ZodRawShape>(
    name: string,
    description: string,
    input: S,
    fn: (args: z.objectOutputType<S, z.ZodTypeAny>) => Promise<string> | string,
  ) =>
    server.registerTool(name, { description, inputSchema: input }, (async (args: z.objectOutputType<S, z.ZodTypeAny>) => {
      try {
        return text(await fn(args))
      } catch (e) {
        return fail(e)
      }
    }) as never)

  // ---- items ----

  if (!run)
    tool(
      'list_items',
      'The open items, and those closed in the last day, oldest blocker first. Optionally only those with one blocker.',
      { blocker: z.enum(BLOCKERS).optional() },
      ({ blocker }) =>
        JSON.stringify(
          engine
            .overview()
            .items.filter((i) => !blocker || i.blocker === (blocker as Blocker))
            .map(({ id, title, blocker, state, url }) => ({ id, title, blocker, state, url })),
          null,
          1,
        ),
    )
  tool(
    'get_item',
    run ? 'Your item: its state, the instruction, what woke it, and its full history.' : 'An item: its state, the instruction, and its full history.',
    itemShape,
    (a) => {
      const detail = engine.detail(itemOf(a))
      return JSON.stringify(detail && { ...detail, runs: detail.runs.map(({ result: _, ...r }) => r) }, null, 1)
    },
  )
  tool('set_state', 'Record the current state of the item. Do this whenever it changes.', { ...itemShape, state }, (a) => {
    engine.setState(itemOf(a), a.state, actor)
    return 'Recorded.'
  })
  tool('note', "Add a line to the item's history: something done or found worth keeping.", { ...itemShape, text: z.string() }, (a) => {
    engine.note(itemOf(a), a.text, actor)
    return 'Recorded.'
  })
  tool(
    'create_item',
    'Create an item. With an instruction it goes straight to an agent; without one, to the operator. Answers its id.',
    { title: z.string(), state: z.string().optional(), instruction: z.string().optional() },
    ({ title, state, instruction }) => {
      const parent = run ? itemOf({}) : null
      const id = engine.create(title, { state, instruction, actor, source: parent ? `item:${parent}` : 'mcp' })
      if (parent) engine.note(parent, `Created item ${id}: ${title}`, actor)
      return id ?? 'Not created.'
    },
  )

  // ---- reads ----

  for (const read of engine.registry.reads)
    tool(read.name, read.description, read.input, (args) => read.run(args as Record<string, unknown>, engine.exec))

  // ---- history, for looking back at how the work went ----

  tool(
    'recent_items',
    'Items touched in the last `days`: minutes spent on each blocker, agent runs and their cost, operator decisions, approvals the operator changed, and what was handled outside the tool.',
    { days: z.number().default(7) },
    ({ days }) => JSON.stringify(engine.recent(days), null, 1),
  )
  tool('item_history', "Any item's full history: every event, wait and run.", { item_id: z.string() }, ({ item_id }) =>
    JSON.stringify(engine.detail(item_id), null, 1),
  )
  tool('run_transcript', "One agent run's prompt and whole transcript.", { run_id: z.string() }, ({ run_id }) => {
    const t = engine.transcript(run_id)
    if (!t) throw new Error(`No run ${run_id}.`)
    return t.length > 200_000 ? `${t.slice(0, 200_000)}\n… (cut)` : t
  })

  if (scope.kind === 'run') addEndings(engine, scope.runId, tool)
  return server
}

/** The tools a run ends with: exactly one of them, once. */
function addEndings(
  engine: Engine,
  runId: string,
  tool: <S extends z.ZodRawShape>(name: string, d: string, i: S, fn: (a: z.objectOutputType<S, z.ZodTypeAny>) => Promise<string>) => void,
) {
  const end = (outcome: AgentOutcome) => engine.agentBlock(runId, outcome)
  tool('ask_operator', 'END YOUR RUN: the item needs a decision or information from the operator.', { state, question: z.string() }, (a) =>
    end({ kind: 'ask', ...a }),
  )
  const actions = engine.registry.actions
    .map((a) => `- ${a.id}: ${a.description} Inputs: ${a.fields.map((f) => f.name + (f.options ? ` (${f.options.join('|')})` : '')).join(', ')}`)
    .join('\n')
  tool(
    'request_approval',
    `END YOUR RUN: ask the operator to run an action other people will see. Fill in every input you can; the operator may change them. Actions:\n${actions}`,
    { state, action: z.string(), inputs: z.record(z.string()), reason: z.string().describe('Why, in a line or two.') },
    (a) => end({ kind: 'approval', ...a }),
  )
  const waits = engine.registry.waits.map((w) => `- ${w.id}: ${w.description} Params: ${Object.keys(w.input).join(', ')}`).join('\n')
  tool(
    'wait_for',
    `END YOUR RUN: the item waits for something outside the tool. It comes back to an agent when that happens, or to the operator after timeout_minutes. Kinds:\n${waits}`,
    { state, kind: z.string(), params: z.record(z.unknown()), timeout_minutes: z.number().optional() },
    ({ state, kind, params, timeout_minutes }) => end({ kind: 'wait', state, wait: kind, params, timeoutMinutes: timeout_minutes }),
  )
  tool(
    'wait_for_items',
    'END YOUR RUN: the item waits for other items to close. It comes back to an agent when they all have.',
    { state, item_ids: z.array(z.string()).min(1) },
    ({ state, item_ids }) => end({ kind: 'items', state, itemIds: item_ids }),
  )
  tool('close_item', 'END YOUR RUN: the item is done.', { state }, (a) => end({ kind: 'close', ...a }))
}

/** Every tool a scope offers, exactly as a client lists them. */
export async function describeTools(engine: Engine, scope: Scope): Promise<ToolInfo[]> {
  const server = makeServer(engine, scope)
  const [a, b] = InMemoryTransport.createLinkedPair()
  const client = new Client({ name: 'describe', version: '0' })
  await Promise.all([server.connect(a), client.connect(b)])
  const { tools } = await client.listTools()
  await client.close()
  await server.close()
  return tools.map((t) => ({ name: t.name, description: t.description ?? '', inputSchema: t.inputSchema }))
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
  /** Where a run's agent reaches its scope. */
  runUrl: (runId: string) => string
  /** Where anything else reaches the operator scope, or why it can't. */
  operator: () => { url: string; error: string | null }
  /** Listens again, after the port or token changed. */
  restartOperator: () => Promise<void>
  close: () => void
}

/** Starts the agents' server on a free port, and the operator's on the configured one. */
export async function startMcp(engine: Engine, settings: Settings): Promise<Mcp> {
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
