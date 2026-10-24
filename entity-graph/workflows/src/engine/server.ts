// The phone server: the built PWA shell plus a JSON API over plain node:http,
// bound to 127.0.0.1 ONLY. A remote phone reaches it over Tailscale.
//
// It exposes exactly the WorkflowsAPI *data* methods (PHONE_METHODS), never the
// phone-admin ones, so a remote client can neither run a CLI nor change its own
// access. The bearer token is read from settings on every request (so a
// regenerate takes effect at once) and compared length-safe, constant-time.

import { timingSafeEqual } from 'node:crypto'
import { readFile } from 'node:fs/promises'
import { createServer, type IncomingMessage, type RequestListener, type Server, type ServerResponse } from 'node:http'
import type { AddressInfo } from 'node:net'
import { extname, join, normalize, sep } from 'node:path'
import type { Engine } from './engine'

/** One named entry in the API surface: the renderer (IPC) and the phone server (HTTP) share it. */
export type Handler = (...args: never[]) => unknown
export type Handlers = Record<string, Handler>

/**
 * The data methods a remote phone may call: the WorkflowsAPI data surface, and
 * exactly it. The phone-admin methods (phone, setPhoneEnabled, setPhonePort,
 * regeneratePhoneToken, runTailscaleServe) are deliberately absent — they live
 * in the IPC handler table for the desktop window alone.
 */
export const PHONE_METHODS = [
  'overview',
  'detail',
  'eventDetail',
  'catalog',
  'next',
  'viewed',
  'skipped',
  'create',
  'send',
  'approve',
  'cancel',
  'close',
  'handledOutside',
  'pollSources',
  'integrations',
  'addSlack',
  'removeSlack',
  'mcp',
  'regenerateMcpToken',
  'setMcpPort',
]
const allowed = new Set(PHONE_METHODS)

const BODY_CAP = 1_000_000

const MIME: Record<string, string> = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.webmanifest': 'application/manifest+json',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.ico': 'image/x-icon',
  '.woff': 'font/woff',
  '.woff2': 'font/woff2',
  '.txt': 'text/plain; charset=utf-8',
}

class TooLarge extends Error {}

/** The bearer token from the Authorization header, or undefined. No token in the URL. */
function bearer(req: IncomingMessage): string | undefined {
  const h = req.headers.authorization
  return h?.startsWith('Bearer ') ? h.slice(7) : undefined
}

/** Length-safe, constant-time compare of the request's token against the stored one. */
function authed(engine: Engine, req: IncomingMessage): boolean {
  const provided = bearer(req)
  if (!provided) return false
  const a = Buffer.from(provided)
  const b = Buffer.from(engine.settings.phoneToken())
  return a.length === b.length && timingSafeEqual(a, b)
}

async function readBody(req: IncomingMessage): Promise<string> {
  const chunks: Buffer[] = []
  let size = 0
  for await (const c of req) {
    size += (c as Buffer).length
    if (size > BODY_CAP) throw new TooLarge('Request body too large')
    chunks.push(c as Buffer)
  }
  return Buffer.concat(chunks).toString('utf8')
}

/**
 * `GET /api/events`: a `data: change` line on every engine change; a keep-alive comment every ~25s.
 * The response joins `clients`, a set the single engine 'change' listener fans out to — so N phones
 * cost one engine listener, not N (which would trip the MaxListeners warning).
 */
function events(engine: Engine, clients: Set<ServerResponse>, req: IncomingMessage, res: ServerResponse): void {
  if (req.method !== 'GET') return void res.writeHead(405).end('Method not allowed')
  if (!authed(engine, req)) return void res.writeHead(401).end('Unauthorised')
  res.writeHead(200, { 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-store', Connection: 'keep-alive' })
  res.write(': connected\n\n')
  clients.add(res)
  const keepAlive = setInterval(() => res.write(': keep-alive\n\n'), 25_000)
  req.on('close', () => {
    clearInterval(keepAlive)
    clients.delete(res)
  })
}

/** `POST /api/<method>` over the allowlist, bearer-checked; the handler's result as JSON. */
async function api(engine: Engine, handlers: Handlers, path: string, req: IncomingMessage, res: ServerResponse): Promise<void> {
  res.setHeader('Cache-Control', 'no-store')
  if (req.method !== 'POST') return void res.writeHead(405).end('Method not allowed')
  if (!authed(engine, req)) return void res.writeHead(401).end('Unauthorised')
  const name = path.slice('/api/'.length)
  if (!allowed.has(name) || !handlers[name]) return void res.writeHead(404).end('Unknown method')
  let args: unknown[]
  try {
    const raw = await readBody(req)
    const body = raw ? (JSON.parse(raw) as { args?: unknown[] }) : {}
    args = Array.isArray(body.args) ? body.args : []
  } catch (e) {
    const code = e instanceof TooLarge ? 413 : 400
    return void res.writeHead(code).end((e as Error).message)
  }
  try {
    const out = await handlers[name](...(args as never[]))
    res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' })
    res.end(out === undefined ? '' : JSON.stringify(out))
  } catch (e) {
    res.writeHead(400).end((e as Error).message)
  }
}

/** The PWA shell from `staticDir`, with path-traversal refusal and SPA fallback to index.html. */
async function serveStatic(staticDir: string, path: string, req: IncomingMessage, res: ServerResponse): Promise<void> {
  if (req.method !== 'GET') return void res.writeHead(405).end('Method not allowed')
  let decoded: string
  try {
    decoded = decodeURIComponent(path)
  } catch {
    return void res.writeHead(400).end('Bad path')
  }
  const root = normalize(staticDir)
  const candidate = normalize(join(root, decoded === '/' ? 'index.html' : decoded))
  // Path traversal refusal: the resolved path must stay within the static root.
  if (candidate !== root && !candidate.startsWith(root + sep)) return void res.writeHead(403).end('Forbidden')

  const send = async (file: string, fallback: string | null): Promise<void> => {
    try {
      const buf = await readFile(file)
      const base = file.slice(root.length + 1)
      const headers: Record<string, string> = { 'Content-Type': MIME[extname(file).toLowerCase()] ?? 'application/octet-stream' }
      // Data is always live; the shell's entry points are never cached.
      if (base === 'index.html' || base === 'sw.js') headers['Cache-Control'] = 'no-store'
      res.writeHead(200, headers)
      res.end(buf)
    } catch {
      if (fallback) return send(fallback, null)
      res.writeHead(404).end('Not found')
    }
  }
  // A path with an extension is an asset: 404 if absent. Anything else is an SPA route: index.html.
  await send(candidate, extname(candidate) ? null : join(root, 'index.html'))
}

function listener(engine: Engine, clients: Set<ServerResponse>, handlers: Handlers, staticDir: string): RequestListener {
  return async (req, res) => {
    try {
      const path = new URL(req.url ?? '/', 'http://127.0.0.1').pathname
      if (path === '/api/events') return events(engine, clients, req, res)
      if (path.startsWith('/api/')) return void (await api(engine, handlers, path, req, res))
      await serveStatic(staticDir, path, req, res)
    } catch (e) {
      if (!res.headersSent) res.writeHead(500).end((e as Error).message)
    }
  }
}

const listen = (server: Server, port: number): Promise<void> =>
  new Promise((resolve, reject) => {
    server.once('error', reject)
    server.listen(port, '127.0.0.1', () => {
      server.off('error', reject)
      resolve()
    })
  })

export interface Phone {
  /** The loopback URL it is (or would be) reached at. */
  url: string
  /** Why it could not listen, if it could not. */
  error: string | null
  /** Re-listen to match settings: start when enabled, close when not; also after a port change. */
  restart: () => Promise<void>
  close: () => void
  /** The bound address, or null when not listening. */
  address: () => AddressInfo | null
}

/** Starts the phone server (listening only when enabled), serving the PWA shell from `staticDir`. */
export async function startPhoneServer(engine: Engine, handlers: Handlers, opts: { staticDir: string }): Promise<Phone> {
  const settings = engine.settings
  let server: Server | null = null
  // The live SSE responses, fanned out to by one engine listener for this server's lifetime.
  const clients = new Set<ServerResponse>()
  const onChange = (): void => {
    for (const res of clients) res.write('data: change\n\n')
  }
  engine.on('change', onChange)
  // Stop listening and drop open connections at once: close() alone waits for in-flight
  // requests, and an open /api/events stream never ends — so a phone connected would hang
  // the toggle-off or port change forever. closeAllConnections() (Node >=18.2) forces them shut.
  const stop = async (): Promise<void> => {
    if (!server) return
    const s = server
    server = null
    await new Promise<void>((resolve) => {
      s.close(() => resolve())
      s.closeAllConnections()
    })
  }
  const phone: Phone = {
    url: `http://127.0.0.1:${settings.phonePort()}/`,
    error: null,
    address: () => (server ? (server.address() as AddressInfo) : null),
    restart: async () => {
      await stop()
      if (!settings.phoneEnabled()) {
        phone.error = null
        phone.url = `http://127.0.0.1:${settings.phonePort()}/`
        return
      }
      const next = createServer(listener(engine, clients, handlers, opts.staticDir))
      try {
        await listen(next, settings.phonePort())
        server = next
        phone.error = null
      } catch (e) {
        phone.error = `Could not listen on port ${settings.phonePort()}: ${(e as Error).message}`
      }
      const port = server ? (server.address() as AddressInfo).port : settings.phonePort()
      phone.url = `http://127.0.0.1:${port}/`
    },
    close: () => {
      engine.off('change', onChange)
      server?.closeAllConnections()
      server?.close()
      server = null
    },
  }
  await phone.restart()
  return phone
}
