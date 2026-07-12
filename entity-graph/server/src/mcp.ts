import type { FastifyInstance } from 'fastify'
import { Server } from '@modelcontextprotocol/sdk/server/index.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js'
import { argsJsonSchema, type Source } from '../../src/core/source/index'
import { bearerToken } from './app'
import type { ConfigDb } from './config'
import type { Registry } from './registry'

function makeMcpServer(source: Source): Server {
  const server = new Server(
    { name: `entity-graph:${source.id}`, version: '0.1.0' },
    { capabilities: { tools: {} } }
  )

  server.setRequestHandler(ListToolsRequestSchema, async () => ({
    tools: source.tools().map((t) => ({
      name: t.id,
      description: `${t.name} — ${t.description}`,
      inputSchema: argsJsonSchema(t) as { type: 'object' },
    })),
  }))

  server.setRequestHandler(CallToolRequestSchema, async (req) => {
    const { name, arguments: args } = req.params
    try {
      const result = await source.call(name, args ?? {})
      return { content: [{ type: 'text', text: JSON.stringify(result) }] }
    } catch (e) {
      return {
        content: [{ type: 'text', text: e instanceof Error ? e.message : String(e) }],
        isError: true,
      }
    }
  })

  return server
}

interface McpDeps {
  db: ConfigDb
  registry: Registry
}

/**
 * POST /:sourceId/mcp — a stateless MCP endpoint per source. A fresh MCP server
 * + transport is created per request (no session state); each source tool is
 * exposed as an MCP tool delegating to `source.call`. GET/DELETE are unused.
 */
export function registerMcp(app: FastifyInstance, deps: McpDeps): void {
  app.post<{ Params: { sourceId: string } }>('/:sourceId/mcp', async (req, reply) => {
    const { sourceId } = req.params
    if (!deps.db.getSource(sourceId)) {
      return reply.code(404).send({ error: `source "${sourceId}" not found` })
    }
    const token = bearerToken(req)
    if (!token || !deps.db.verifyToken(sourceId, token)) {
      return reply.code(401).send({ error: 'invalid or missing source token' })
    }

    const source = await deps.registry.get(sourceId)
    const server = makeMcpServer(source)
    const transport = new StreamableHTTPServerTransport({ sessionIdGenerator: undefined })

    reply.hijack()
    reply.raw.on('close', () => {
      transport.close()
      server.close()
    })
    await server.connect(transport)
    await transport.handleRequest(req.raw, reply.raw, req.body)
  })

  for (const method of ['GET', 'DELETE'] as const) {
    app.route({
      method,
      url: '/:sourceId/mcp',
      handler: async (_req, reply) =>
        reply.code(405).send({ error: 'stateless MCP endpoint: use POST' }),
    })
  }
}
