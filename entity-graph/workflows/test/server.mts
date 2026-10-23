// The phone server, headless, against an in-memory node:sqlite store and a temp
// static dir. It checks the auth, the allowlist, the SSE push, the loopback
// bind, the static shell, and that the phone token is its own.

import assert from 'node:assert/strict'
import { mkdtemp, writeFile } from 'node:fs/promises'
import { connect } from 'node:net'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { DatabaseSync } from 'node:sqlite'
import { Admin } from '../src/engine/admin.ts'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { PHONE_METHODS, startPhoneServer } from '../src/engine/server.ts'
import { WorkerPool } from '../src/engine/workers.ts'
import { makeHandlers } from '../src/main/handlers.ts'

const engine = new Engine({ db: new DatabaseSync(':memory:') })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
const pool = new WorkerPool(engine, mcp.runUrl, 2)
const admin = new Admin(engine, mcp, 2)
const handlers = makeHandlers(engine, admin, pool)

const staticDir = await mkdtemp(join(tmpdir(), 'wf-phone-'))
await writeFile(join(staticDir, 'index.html'), '<!doctype html><title>Workflows</title>')
await writeFile(join(staticDir, 'manifest.webmanifest'), '{"name":"Workflows"}')

engine.settings.setPhoneEnabled(true)
engine.settings.setPhonePort(0) // an ephemeral port for the test
const phone = await startPhoneServer(engine, handlers, { staticDir })
admin.attachPhone(phone)

// ---- binds 127.0.0.1 only ----
const addr = phone.address()!
assert.equal(addr.address, '127.0.0.1')
const base = `http://127.0.0.1:${addr.port}`

const token = engine.settings.phoneToken()
const auth = { Authorization: `Bearer ${token}` }
const post = (name: string, headers: Record<string, string> = {}, body: unknown = { args: [] }) =>
  fetch(`${base}/api/${name}`, { method: 'POST', headers: { 'Content-Type': 'application/json', ...headers }, body: JSON.stringify(body) })

// ---- the phone token is its own, distinct from the MCP one ----
assert.notEqual(token, engine.settings.mcpToken())
assert.ok(token.length > 0)

// ---- the API needs the token ----
assert.equal((await post('overview')).status, 401, 'no token')
assert.equal((await post('overview', { Authorization: 'Bearer wrong' })).status, 401, 'wrong token')
const ok = await post('overview', auth)
assert.equal(ok.status, 200)
assert.ok(Array.isArray(((await ok.json()) as { items: unknown[] }).items))

// ---- the allowlist: data methods only ----
assert.ok(PHONE_METHODS.includes('overview'))
assert.ok(!PHONE_METHODS.includes('setPhoneEnabled'), 'phone-admin methods are not on the allowlist')
assert.equal((await post('setPhoneEnabled', auth, { args: [true] })).status, 404, 'a phone-admin method is 404 even with the token')
assert.equal((await post('runTailscaleServe', auth)).status, 404, 'a phone-admin method is 404 even with the token')
assert.equal((await post('nope', auth)).status, 404, 'an unknown method is 404 even with the token')

// ---- the static shell, served without a token ----
const index = await fetch(`${base}/`)
assert.equal(index.status, 200)
assert.match(index.headers.get('content-type') ?? '', /text\/html/)
assert.match(await index.text(), /Workflows/)
assert.match((await fetch(`${base}/index.html`)).headers.get('cache-control') ?? '', /no-store/)

// ---- the manifest's MIME ----
const manifest = await fetch(`${base}/manifest.webmanifest`)
assert.equal(manifest.status, 200)
assert.equal(manifest.headers.get('content-type'), 'application/manifest+json')

// ---- path traversal refused: encoded ../ survives URL parsing, and the root check rejects it ----
// (A literal ../ is resolved to within the root by the URL parser, so it can never escape anyway.)
assert.equal((await fetch(`${base}/..%2f..%2f..%2fetc%2fpasswd`)).status, 403)
assert.equal((await fetch(`${base}/%2e%2e%2f%2e%2e%2fetc%2fpasswd`)).status, 403)
// A raw request with an encoded traversal the URL parser keeps intact, straight to the server.
const rawStatus = await new Promise<number>((resolve, reject) => {
  const sock = connect(addr.port, '127.0.0.1', () => sock.write('GET /..%2f..%2f..%2fetc%2fpasswd HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'))
  let buf = ''
  sock.on('data', (d) => (buf += d.toString()))
  sock.on('end', () => resolve(Number(/^HTTP\/1\.\d (\d+)/.exec(buf)?.[1])))
  sock.on('error', reject)
})
assert.equal(rawStatus, 403, 'an encoded ../ is refused')

// ---- the SSE push: a line on every engine change ----
{
  const controller = new AbortController()
  const res = await fetch(`${base}/api/events`, { headers: auth, signal: controller.signal })
  assert.equal(res.status, 200)
  assert.match(res.headers.get('content-type') ?? '', /text\/event-stream/)
  const reader = res.body!.getReader()
  const decoder = new TextDecoder()
  engine.emit('change')
  let seen = ''
  const got = (async () => {
    while (!seen.includes('data: change')) {
      const { value, done } = await reader.read()
      if (done) break
      seen += decoder.decode(value, { stream: true })
    }
  })()
  const timeout = new Promise((_, reject) => setTimeout(() => reject(new Error('no SSE within 2s')), 2000).unref())
  await Promise.race([got, timeout])
  assert.match(seen, /data: change/)
  controller.abort()
}

// ---- restart() resolves even with an /api/events stream open (close alone would hang) ----
{
  const controller = new AbortController()
  const res = await fetch(`${base}/api/events`, { headers: auth, signal: controller.signal })
  assert.equal(res.status, 200)
  await res.body!.getReader().read() // the ': connected' line, so the stream is truly open
  const restarted = phone.restart()
  const timeout = new Promise((_, reject) => setTimeout(() => reject(new Error('restart() hung on an open SSE stream')), 2000).unref())
  await Promise.race([restarted, timeout])
  controller.abort()
}

// ---- disabled means not listening (the server itself respects the flag) ----
engine.settings.setPhoneEnabled(false)
await phone.restart()
assert.equal(phone.address(), null, 'not listening while disabled')
await assert.rejects(fetch(`${base}/`), 'the socket is gone')

phone.close()
mcp.close()
console.log('server: ok')
