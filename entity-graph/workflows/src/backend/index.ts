// The backend, as a process of its own, on one source: the store (an
// egdb-server it runs on the source's file), the engine and its loop, the bots,
// the MCP server, and an HTTP API for the app on a loopback port behind a token.
//
//   node out/main/backend.js --file <source>      (or: npm run backend -- --file <source>)
//
// The source is made if it is not there (`~/` is the home folder). Beside it,
// <source>.data/ holds the rest (see info.ts); its backend.json says where the
// backend listens, and its token, which is how the app finds it. The app starts
// one of its own when none is running. One backend per source: a second refuses.
//
//   GET  /health                 → { ok: true }
//   POST /rpc/<method>  [args]   → { result } or { error }   (WorkflowsAPI's methods, as x-workflows-user; api.ts)
//   GET  /events                 → server-sent events: `change` whenever anything changed

import { randomBytes } from 'node:crypto'
import { mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { createServer, type IncomingMessage, type ServerResponse } from 'node:http'
import type { AddressInfo } from 'node:net'
import { dirname, join } from 'node:path'
import { Admin } from '../engine/admin'
import { openStore } from '../engine/egdb'
import { Engine } from '../engine/engine'
import { startMcp } from '../engine/mcp'
import { EVENT_TYPES } from '../engine/eventTypes'
import { BotRunner } from '../engine/workers'
import { keyFileSecrets } from './secrets'
import { handlers, OPEN_METHODS } from './api'
import { dataDirOf, runningBackend, sourceFile, type BackendInfo } from './info'

const LOOP_MS = 60_000

const arg = (name: string): string | undefined => {
  const i = process.argv.indexOf(`--${name}`)
  return i >= 0 ? process.argv[i + 1] : undefined
}

async function main(): Promise<void> {
  const typed = arg('file') ?? process.env.WORKFLOWS_FILE
  if (!typed) throw new Error('Which source? --file <path>, such as --file ~/tasks/tasks.sqlite.')
  const file = sourceFile(typed)
  const dir = dataDirOf(file)
  mkdirSync(dirname(file), { recursive: true })
  mkdirSync(dir, { recursive: true })
  const already = await runningBackend(dir)
  if (already) throw new Error(`A backend is running on ${file} already, at ${already.url} (pid ${already.pid}).`)
  // One backend per source: a lock, held from before the store opens until it is let go.
  const release = lock(join(dir, 'backend.lock'))

  const log = (...a: unknown[]) => console.log(new Date().toISOString(), ...a)
  const token = randomBytes(24).toString('base64url')
  const blocking = (type: string) => Boolean(EVENT_TYPES.find((t) => t.type === type)?.blocking)
  const { store, sink, egdb } = await openStore(file, randomBytes(24).toString('base64url'), blocking, (m) => log(m))
  const engine = new Engine({ store, dataDir: dir, secrets: keyFileSecrets(dir) })

  const mcp = await startMcp(engine)
  const runner = new BotRunner(engine, mcp.runUrl)
  const admin = new Admin(engine, mcp)
  const api = (me: string) => handlers(engine, admin, me) as unknown as Record<string, (...args: unknown[]) => unknown>
  engine.on('sourceError', (message: string) => log(message))

  // Change events, to whoever is listening: a burst of changes goes as one.
  const listeners = new Set<ServerResponse>()
  let pending: NodeJS.Timeout | null = null
  engine.on('change', () => {
    pending ??= setTimeout(() => {
      pending = null
      for (const res of listeners) res.write('event: change\ndata: {}\n\n')
    }, 30)
  })

  const body = async (req: IncomingMessage): Promise<unknown> => {
    const chunks: Buffer[] = []
    for await (const c of req) chunks.push(c as Buffer)
    const raw = Buffer.concat(chunks).toString('utf8')
    return raw ? JSON.parse(raw) : []
  }
  const send = (res: ServerResponse, status: number, value: unknown) => {
    res.writeHead(status, { 'content-type': 'application/json' }).end(JSON.stringify(value ?? null))
  }
  const server = createServer(async (req, res) => {
    const url = req.url ?? ''
    if (url === '/health') return send(res, 200, { ok: true })
    if (req.headers.authorization !== `Bearer ${token}`) return send(res, 401, { error: 'A bearer token is needed: see backend.json.' })
    if (url === '/events' && req.method === 'GET') {
      res.writeHead(200, { 'content-type': 'text/event-stream', 'cache-control': 'no-cache', connection: 'keep-alive' })
      res.write(': hello\n\n')
      listeners.add(res)
      req.on('close', () => listeners.delete(res))
      return
    }
    const m = /^\/rpc\/(\w+)$/.exec(url)
    const user = String(req.headers['x-workflows-user'] ?? '')
    const calls = api(user || engine.me())
    if (!m || req.method !== 'POST' || !Object.hasOwn(calls, m[1])) return send(res, 404, { error: `No method ${url}.` })
    if (!OPEN_METHODS.has(m[1]) && !engine.isUser(user)) return send(res, 200, { error: user ? `Nobody is called ${user}: sign in again.` : 'Sign in first.' })
    try {
      const args = (await body(req)) as unknown[]
      send(res, 200, { result: await calls[m[1]](...(Array.isArray(args) ? args : [])) })
    } catch (e) {
      send(res, 200, { error: (e as Error).message })
    }
  })
  await new Promise<void>((resolve, reject) => {
    server.once('error', reject)
    server.listen(Number(arg('port') ?? process.env.WORKFLOWS_PORT ?? 0), '127.0.0.1', () => resolve())
  })
  const info: BackendInfo = { url: `http://127.0.0.1:${(server.address() as AddressInfo).port}`, token, pid: process.pid }
  const infoFile = join(dir, 'backend.json')
  writeFileSync(infoFile, JSON.stringify(info, null, 2), { mode: 0o600 })

  // The central loop: every wait that is due, then every source; checks come due by the second.
  const tick = () => engine.tick().catch((e) => log('loop:', e))
  void tick()
  const loop = setInterval(tick, LOOP_MS)
  const checks = setInterval(() => void engine.checkDue().catch((e) => log('checks:', e)), 1000)
  runner.start()
  log(`backend for ${file} listening at ${info.url}; egdb at ${egdb.url}`)
  // Started by the app: it hears where it is at once.
  process.send?.({ ready: info })

  let stopping = false
  const stop = async () => {
    if (stopping) return
    stopping = true
    log('stopping')
    clearInterval(loop)
    clearInterval(checks)
    engine.processes.stopAll()
    // The runs going end first (their items handed back), then what they wrote is flushed.
    await Promise.race([runner.stop(), new Promise((r) => setTimeout(r, 10_000))])
    for (const res of listeners) res.end()
    server.close()
    mcp.close()
    await Promise.race([sink.flush(), new Promise((r) => setTimeout(r, 10_000))])
    egdb.stop()
    try {
      if (JSON.parse(readFileSync(infoFile, 'utf8')).pid === process.pid) rmSync(infoFile)
    } catch {
      // Gone already.
    }
    release()
    process.exit(0)
  }
  process.on('SIGINT', stop)
  process.on('SIGTERM', stop)
  // Started by the app, it stops with the app.
  process.on('disconnect', stop)
  process.on('message', (m) => m === 'stop' && void stop())
}

/** Takes the lock file, or says who has it; one left by a process that is gone is taken over. */
function lock(file: string): () => void {
  for (let tries = 0; ; tries++) {
    try {
      writeFileSync(file, String(process.pid), { flag: 'wx' })
      return () => rmSync(file, { force: true })
    } catch (e) {
      if ((e as NodeJS.ErrnoException).code !== 'EEXIST' || tries) throw e
      const pid = Number(readFileSync(file, 'utf8'))
      let alive = false
      try {
        process.kill(pid, 0)
        alive = pid !== process.pid
      } catch {
        // No such process.
      }
      if (alive) throw new Error(`A backend (pid ${pid}) is starting or running on this data folder already.`)
      rmSync(file, { force: true })
    }
  }
}

main().catch((e) => {
  console.error((e as Error).message)
  process.exit(1)
})
