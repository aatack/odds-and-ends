// How the backend answers HTTP, on either of its two listeners (both loopback):
//
// - the app's: a port of its own each start, the token in backend.json. Every method.
// - the phone's: a port that lasts (`phone.port`), the phone token (`phone.token`, from the Phone page). Every method
//   but those that change who reaches it (PHONE_REFUSED), the built app (the PWA) to whoever asks, and nothing at
//   all to what comes from the tailnet (through `tailscale serve`) unless `phone.tailnet` is on.
//
//   GET  /health                 → { ok: true }
//   POST /rpc/<method>  [args]   → { result } or { error }   (WorkflowsAPI's methods, as x-workflows-user; api.ts)
//   GET  /events                 → server-sent events: `change` whenever anything changed
//   GET  anything else           → the app's files (the phone's listener only)

import { createHash, timingSafeEqual } from 'node:crypto'
import { readFile } from 'node:fs/promises'
import { createServer, type IncomingMessage, type RequestListener, type Server, type ServerResponse } from 'node:http'
import type { AddressInfo } from 'node:net'
import { extname, join, normalize, sep } from 'node:path'
import type { Engine } from '../engine/engine'
import { OPEN_METHODS } from './api'

/** What the phone may not call: what it is reached by is changed only from the app on this machine. */
export const PHONE_REFUSED = new Set(['phone', 'setPhoneTailnet', 'setPhonePort', 'regeneratePhoneToken'])

/** Every listener's `/events`: one `change` for a burst of changes, to everyone listening. */
export class Changes {
  private listeners = new Set<ServerResponse>()
  private pending: NodeJS.Timeout | null = null

  constructor(engine: Engine) {
    engine.on('change', () => {
      this.pending ??= setTimeout(() => {
        this.pending = null
        for (const res of this.listeners) res.write('event: change\ndata: {}\n\n')
      }, 30)
    })
  }

  add(req: IncomingMessage, res: ServerResponse): void {
    res.writeHead(200, { 'content-type': 'text/event-stream', 'cache-control': 'no-store', connection: 'keep-alive' })
    res.write(': hello\n\n')
    // A comment now and then, so a proxy (tailscale serve) or a phone's network does not drop it as idle.
    const alive = setInterval(() => res.write(': alive\n\n'), 25_000)
    this.listeners.add(res)
    req.on('close', () => {
      clearInterval(alive)
      this.listeners.delete(res)
    })
  }

  close(): void {
    for (const res of this.listeners) res.end()
    this.listeners.clear()
  }
}

/** The token a request carries matches `token`: compared in constant time, whatever their lengths. */
export function tokenMatches(req: IncomingMessage, token: string): boolean {
  const h = req.headers.authorization
  if (!h?.startsWith('Bearer ') || !token) return false
  const digest = (s: string) => createHash('sha256').update(s).digest()
  return timingSafeEqual(digest(h.slice(7)), digest(token))
}

const LOOPBACK_HOST = /^(127\.0\.0\.1|localhost|\[::1\])(:\d+)?$/i

/**
 * Whether a request came from elsewhere through a proxy on this machine (`tailscale serve`, which forwards to
 * loopback): it says whom it forwards for, or who the tailnet says is asking, or names a host that is not loopback.
 */
export function fromTailnet(req: IncomingMessage): boolean {
  const h = req.headers
  if (h['x-forwarded-for'] || h['x-forwarded-host'] || h.forwarded || h['tailscale-user-login']) return true
  return !LOOPBACK_HOST.test(String(h.host ?? ''))
}

export interface HandlerOptions {
  engine: Engine
  /** The calls, made as a person. */
  calls: (me: string) => Record<string, (...args: unknown[]) => unknown>
  changes: Changes
  /** The token a request must carry: read on each request, so a new one takes at once. */
  token: () => string
  /** The phone's listener: the methods it is refused, the app's files, and the tailnet's gate. */
  phone?: { files: string | null; tailnet: () => boolean }
}

const BODY_LIMIT = 5_000_000

class TooLarge extends Error {}

async function body(req: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = []
  let size = 0
  for await (const c of req) {
    size += (c as Buffer).length
    if (size > BODY_LIMIT) throw new TooLarge('Too large.')
    chunks.push(c as Buffer)
  }
  const raw = Buffer.concat(chunks).toString('utf8')
  return raw ? JSON.parse(raw) : []
}

const send = (res: ServerResponse, status: number, value: unknown) => {
  res.writeHead(status, { 'content-type': 'application/json', 'cache-control': 'no-store' }).end(JSON.stringify(value ?? null))
}

export function handler(o: HandlerOptions): RequestListener {
  return async (req, res) => {
    try {
      const path = new URL(req.url ?? '/', 'http://localhost').pathname
      if (o.phone && fromTailnet(req) && !o.phone.tailnet())
        return send(res, 403, { error: 'This app is not open to the tailnet: turn it on from the Phone page, on the machine it runs on.' })
      if (path === '/health') return send(res, 200, { ok: true })
      const isApi = path === '/events' || path.startsWith('/rpc/')
      if (!isApi && o.phone && req.method === 'GET') return await files(o.phone.files, path, res)
      if (!tokenMatches(req, o.token())) return send(res, 401, { error: o.phone ? 'The token is wrong: copy it again from the Phone page.' : 'A bearer token is needed: see backend.json.' })
      if (path === '/events' && req.method === 'GET') return o.changes.add(req, res)
      const m = /^\/rpc\/(\w+)$/.exec(path)
      const user = String(req.headers['x-workflows-user'] ?? '')
      const calls = o.calls(user || o.engine.me())
      if (!m || req.method !== 'POST' || !Object.hasOwn(calls, m[1]) || (o.phone && PHONE_REFUSED.has(m[1])))
        return send(res, 404, { error: `No method ${path}${o.phone && m && PHONE_REFUSED.has(m[1]) ? ' from the phone' : ''}.` })
      if (!OPEN_METHODS.has(m[1]) && !o.engine.isUser(user)) return send(res, 200, { error: user ? `Nobody is called ${user}: sign in again.` : 'Sign in first.' })
      let args: unknown
      try {
        args = await body(req)
      } catch (e) {
        return send(res, e instanceof TooLarge ? 413 : 400, { error: (e as Error).message })
      }
      try {
        send(res, 200, { result: await calls[m[1]](...(Array.isArray(args) ? args : [])) })
      } catch (e) {
        send(res, 200, { error: (e as Error).message })
      }
    } catch (e) {
      if (!res.headersSent) send(res, 500, { error: (e as Error).message })
    }
  }
}

const TYPES: Record<string, string> = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.webmanifest': 'application/manifest+json',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.ico': 'image/x-icon',
  '.woff': 'font/woff',
  '.woff2': 'font/woff2',
  '.ttf': 'font/ttf',
}

/** The built app: a file in `root`, or (for a path that names no file) index.html; nothing outside `root`. */
async function files(root: string | null, path: string, res: ServerResponse): Promise<void> {
  if (!root) return void res.writeHead(404, { 'content-type': 'text/plain' }).end('The app is not built.')
  let decoded: string
  try {
    decoded = decodeURIComponent(path)
  } catch {
    return void res.writeHead(400).end()
  }
  const base = normalize(root)
  const file = normalize(join(base, decoded === '/' ? 'index.html' : decoded))
  if (file !== base && !file.startsWith(base + sep)) return void res.writeHead(403).end()
  const serve = async (f: string, fallback: string | null): Promise<void> => {
    try {
      const data = await readFile(f)
      const name = f.slice(base.length + 1)
      res.writeHead(200, {
        'content-type': TYPES[extname(f).toLowerCase()] ?? 'application/octet-stream',
        // The hashed assets never change; what names them is asked for afresh each time.
        'cache-control': name.startsWith('assets/') ? 'public, max-age=31536000, immutable' : 'no-cache',
      })
      res.end(data)
    } catch {
      if (fallback) return serve(fallback, null)
      res.writeHead(404).end()
    }
  }
  await serve(file, extname(file) ? null : join(base, 'index.html'))
}

/** Listens on loopback; port 0 is any free one. */
export function listen(server: Server, port: number): Promise<number> {
  return new Promise((resolve, reject) => {
    server.once('error', reject)
    server.listen(port, '127.0.0.1', () => {
      server.off('error', reject)
      resolve((server.address() as AddressInfo).port)
    })
  })
}

/** The phone's listener, kept to its port setting: `restart` after the port changes. */
export interface PhoneListener {
  status: () => { url: string; port: number; error: string | null }
  restart: () => Promise<void>
  close: () => void
}

export async function startPhone(o: Omit<HandlerOptions, 'token' | 'phone'> & { files: string | null }): Promise<PhoneListener> {
  const settings = o.engine.settings
  const handle = handler({ ...o, token: () => settings.phoneToken(), phone: { files: o.files, tailnet: () => settings.phoneTailnet() } })
  let server: Server | null = null
  let port = settings.phonePort()
  let error: string | null = null
  const stop = async () => {
    const s = server
    server = null
    if (!s) return
    await new Promise<void>((resolve) => {
      s.close(() => resolve())
      // A phone listening to /events would hold it open for ever.
      s.closeAllConnections()
    })
  }
  const restart = async () => {
    await stop()
    const next = createServer(handle)
    try {
      port = await listen(next, settings.phonePort())
      server = next
      error = null
    } catch (e) {
      port = settings.phonePort()
      error = `The phone's port, ${port}, is taken (${(e as Error).message}): choose another below.`
    }
  }
  await restart()
  return {
    status: () => ({ url: `http://127.0.0.1:${port}/`, port, error }),
    restart,
    close: () => void stop(),
  }
}
