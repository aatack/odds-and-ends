import { createServer, type IncomingMessage, type Server, type ServerResponse } from 'http'
import { AttributedPensive, PausedError, type Pensive } from '../../core/pensive/index'
import { formatError } from './format'
import { handleMcpRequest } from './mcpServer'

// How a pensive leaves this machine: one small HTTP server per broadcast or MCP
// node, run by the app and answering for exactly one pensive.
//
// There is no admin surface and no source ids to route by — a server serves the
// one pensive its node is plugged into, so the path prefix is ignored and
// `/tools` and `/x/tools` mean the same thing. (That is not laxity: the phone
// client appends a source id to its base URL, and a URL that already worked
// keeps working.)
//
// Everything is authenticated by a bearer token issued on the node itself, and a
// token *is* an identity: writes that arrive with it are recorded as the person
// it was issued to, which is what {@link AttributedPensive} is for.

/** Bodies can carry a pasted image, so the cap is generous rather than tight. */
const MAX_BODY = 64 * 1024 * 1024

const bearer = (req: IncomingMessage): string | null => {
  const header = req.headers['authorization']
  if (!header || Array.isArray(header)) return null
  const match = /^Bearer\s+(.+)$/i.exec(header)
  return match ? match[1] : null
}

/** The last segment of the path, so a prefix in front of it is ignored. */
const route = (url: string): string => (url.split('?')[0].split('/').filter(Boolean).pop() ?? '')

function readBody(req: IncomingMessage): Promise<string> {
  return new Promise((resolve, reject) => {
    let size = 0
    const chunks: Buffer[] = []
    req.on('data', (chunk: Buffer) => {
      size += chunk.length
      if (size > MAX_BODY) {
        reject(new Error('request body is too large'))
        req.destroy()
        return
      }
      chunks.push(chunk)
    })
    req.on('error', reject)
    req.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')))
  })
}

function send(res: ServerResponse, status: number, body: unknown): void {
  const text = JSON.stringify(body ?? null)
  res.writeHead(status, {
    'content-type': 'application/json',
    'content-length': Buffer.byteLength(text),
    // A browser client served from anywhere but here — the phone app — makes
    // every call cross-origin, and a JSON POST carrying an `Authorization`
    // header is preflighted. A wildcard is safe because the credential is a
    // token the client sends deliberately and never a cookie: a hostile page
    // gains nothing it did not already have, which is why credentials are not
    // allowed.
    'access-control-allow-origin': '*',
    vary: 'origin',
  })
  res.end(text)
}

export interface PensiveServerOptions {
  kind: 'broadcast' | 'mcp'
  port: number
  /** The node as it now stands, or null if it has been deleted under us. */
  node: () => { label: string; paused: boolean } | null
  /** Who a token says a write is by, or null when it is not one of ours. */
  authorOf: (token: string) => string | null
  /** What to serve, or the reason there is nothing to. */
  pensive: () => Promise<{ pensive: Pensive } | { problem: string }>
}

/**
 * One node, listening. Nothing is cached here: the pensive and the tokens are
 * read per request, so pausing a node, revoking a token or re-plugging an edge
 * takes effect on the next call rather than on a restart.
 */
export class PensiveServer {
  private server: Server | null = null
  /** Why it isn't listening — a port already taken, most likely. */
  problem: string | null = null

  constructor(private opts: PensiveServerOptions) {}

  get port(): number {
    return this.opts.port
  }

  get url(): string {
    return `http://127.0.0.1:${this.opts.port}`
  }

  get listening(): boolean {
    return !!this.server?.listening
  }

  start(): Promise<void> {
    if (this.server) return Promise.resolve()
    const server = createServer((req, res) => {
      void this.handle(req, res).catch((e) =>
        send(res, 500, { error: e instanceof Error ? e.message : String(e) }),
      )
    })
    this.server = server
    return new Promise((resolve) => {
      server.once('error', (e: Error) => {
        this.problem = `Port ${this.opts.port} is not available — ${e.message}`
        this.server = null
        resolve()
      })
      server.listen(this.opts.port, '0.0.0.0', () => {
        this.problem = null
        resolve()
      })
    })
  }

  stop(): Promise<void> {
    const server = this.server
    this.server = null
    if (!server) return Promise.resolve()
    return new Promise((resolve) => server.close(() => resolve()))
  }

  /**
   * Who is calling, and what they may call it on. Three answers: not
   * authenticated, authenticated but the node is off, or a pensive that records
   * every write as the person the token was issued to.
   */
  private async caller(
    req: IncomingMessage,
  ): Promise<{ pensive: Pensive } | { status: number; error: string }> {
    const node = this.opts.node()
    if (!node) return { status: 404, error: 'this node no longer exists' }

    const token = bearer(req)
    const author = token ? this.opts.authorOf(token) : null
    if (!author) return { status: 401, error: 'invalid or missing bearer token' }
    if (node.paused) return { status: 403, error: `"${node.label}" is paused` }

    const built = await this.opts.pensive()
    if ('problem' in built) return { status: 503, error: built.problem }
    return { pensive: new AttributedPensive(built.pensive, author) }
  }

  private async handle(req: IncomingMessage, res: ServerResponse): Promise<void> {
    if (req.method === 'OPTIONS') {
      res.writeHead(204, {
        'access-control-allow-origin': '*',
        'access-control-allow-methods': 'GET, POST, OPTIONS',
        'access-control-allow-headers':
          (req.headers['access-control-request-headers'] as string | undefined) ??
          'authorization, content-type',
        'access-control-max-age': '86400',
      })
      res.end()
      return
    }

    const where = route(req.url ?? '')
    const wanted = this.opts.kind === 'mcp' ? ['mcp'] : ['tools', 'call']
    if (!wanted.includes(where)) {
      send(res, 404, { error: `nothing at /${where} — this serves ${wanted.join(', ')}` })
      return
    }

    const who = await this.caller(req)
    if ('error' in who) {
      send(res, who.status, { error: who.error })
      return
    }
    const { pensive } = who

    if (where === 'tools') {
      send(res, 200, await pensive.listTools())
      return
    }

    const raw = await readBody(req)
    const body = raw ? (JSON.parse(raw) as Record<string, unknown>) : {}

    if (where === 'mcp') {
      await handleMcpRequest(pensive, req, res, body)
      return
    }

    const tool = typeof body.tool === 'string' ? body.tool : ''
    if (!tool) {
      send(res, 200, { status: 'error', message: 'the body must include "tool"' })
      return
    }
    try {
      send(res, 200, { status: 'success', result: await pensive.callTool(tool, body.args ?? {}) })
    } catch (e) {
      // A refusal is still an answer: the call machine at the other end shows
      // the message, and only a broken *request* is an HTTP error.
      const status = e instanceof PausedError ? 403 : 200
      send(res, status, { status: 'error', message: formatError(e) })
    }
  }
}
