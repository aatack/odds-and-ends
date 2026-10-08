import { randomBytes } from 'node:crypto'
import { createServer, type IncomingMessage } from 'node:http'
import type { Core } from '../core/core.ts'
import { handleMcpRequest } from '../core/mcp.ts'

/**
 * The app's MCP server (`src/core/mcp.ts`), for the Claude sessions it starts:
 * on 127.0.0.1 only, never published, with a token made fresh each run and
 * handed to each session in its `--mcp-config`. It reads, and links (`merge_context`).
 */

export const mcpPort = 47822

async function body(request: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = []
  for await (const chunk of request) chunks.push(chunk as Buffer)
  const text = Buffer.concat(chunks).toString('utf8')
  return text ? JSON.parse(text) : undefined
}

/** Starts the server and tells the core where it is. Returns the stop. */
export function startMcpServer(core: Core, port = mcpPort): () => void {
  const token = randomBytes(24).toString('base64url')
  const server = createServer(async (request, response) => {
    try {
      const url = new URL(request.url ?? '/', 'http://localhost')
      if (url.pathname !== '/mcp') {
        response.writeHead(404).end()
        return
      }
      if (request.headers.authorization !== `Bearer ${token}`) {
        response.writeHead(401, { 'content-type': 'application/json' }).end(JSON.stringify({ error: 'not signed in' }))
        return
      }
      await handleMcpRequest(core, request, response, request.method === 'POST' ? await body(request) : undefined)
    } catch (error) {
      if (!response.headersSent) response.writeHead(500, { 'content-type': 'application/json' })
      response.end(JSON.stringify({ error: error instanceof Error ? error.message : String(error) }))
    }
  })
  server.on('error', (error) => {
    // Another copy of the app has the port: sessions started here go without it.
    console.error('MCP server:', error)
    core.setMcp(null)
  })
  server.listen(port, '127.0.0.1', () => {
    const address = server.address()
    const listening = typeof address === 'object' && address ? address.port : port
    core.setMcp({ url: `http://127.0.0.1:${listening}/mcp`, token })
  })
  return () => {
    core.setMcp(null)
    server.close()
    server.closeAllConnections()
  }
}
