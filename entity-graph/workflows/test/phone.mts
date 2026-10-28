// The phone: the backend's two listeners (the app's, and the phone's with its own token, the built app, and the
// tailnet's gate) and the renderer's HTTP transport against them, on an engine with an in-memory store. Also the
// QR code the Phone page draws, against qrencode where there is one.

import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { createServer, request } from 'node:http'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { handlers } from '../src/backend/api.ts'
import { Changes, handler, listen, startPhone, tokenMatches } from '../src/backend/http.ts'
import { Admin } from '../src/engine/admin.ts'
import { Engine } from '../src/engine/engine.ts'
import type { Mcp } from '../src/engine/mcp.ts'
import { encodeQr } from '../src/renderer/src/helpers/qr.ts'
import { httpApi, TOKEN_KEY, type KeptStrings } from '../src/renderer/src/transport.ts'

const dir = mkdtempSync(join(tmpdir(), 'workflows-phone-'))
// A stand-in for tailscale: what it serves, by HTTPS port.
const served = new Map<string, string>()
const exec = async (cmd: string, args: string[]) => {
  if (cmd === 'tailscale' && args[0] === 'status') return JSON.stringify({ Self: { DNSName: 'laptop.tail1234.ts.net.' } })
  if (cmd === 'tailscale' && args[0] === 'serve' && args[1] === 'status') return JSON.stringify({ TCP: Object.fromEntries([...served.keys()].map((p) => [p, { HTTPS: true }])) })
  if (cmd === 'tailscale' && args[0] === 'serve' && args.at(-1) === 'off') return void served.delete(args[1].replace('--https=', '')) ?? ''
  if (cmd === 'tailscale' && args[0] === 'serve') return void served.set(args[2].replace('--https=', ''), args[3]) ?? ''
  throw new Error(`no ${cmd}`)
}
const engine = new Engine({ exec, dataDir: dir })
await engine.signIn('human')
const admin = new Admin(engine, {} as Mcp)
const source = join(dir, 'tasks.sqlite')
const api = (me: string) => handlers(engine, admin, me, source) as unknown as Record<string, (...args: unknown[]) => unknown>
const changes = new Changes(engine)

// The app's own listener, as the backend runs it.
const desk = createServer(handler({ engine, calls: api, changes, token: () => 'desk-token' }))
const deskUrl = `http://127.0.0.1:${await listen(desk, 0)}`

// The phone's, on any free port, serving a stand-in build.
const built = join(dir, 'renderer')
mkdirSync(join(built, 'assets'), { recursive: true })
writeFileSync(join(built, 'index.html'), '<!doctype html><title>YAWN</title>')
writeFileSync(join(built, 'assets', 'app-1234.js'), 'console.log(1)')
writeFileSync(join(dir, 'secret.txt'), 'not for the phone')
engine.settings.set('phone.port', 0)
const phone = await startPhone({ engine, calls: api, changes, files: built })
admin.phone = phone
const phoneUrl = phone.status().url.replace(/\/$/, '')
assert.equal(phone.status().error, null)

const token = engine.settings.phoneToken()
assert.equal(engine.settings.phoneToken(), token, 'the phone token lasts')
assert.ok(engine.store.settings.get('phone.token') !== token, 'and is kept as a secret, not as it is')

const rpc = (base: string, method: string, args: unknown[] = [], headers: Record<string, string> = {}) =>
  fetch(`${base}/rpc/${method}`, { method: 'POST', headers: { 'content-type': 'application/json', 'x-workflows-user': 'human', ...headers }, body: JSON.stringify(args) })
const bearer = (t: string) => ({ authorization: `Bearer ${t}` })

// ---- 1. The token ----
{
  assert.equal((await rpc(phoneUrl, 'overview')).status, 401, 'no token, no answer')
  assert.equal((await rpc(phoneUrl, 'overview', [], bearer('wrong'))).status, 401, 'a wrong token, no answer')
  assert.equal((await rpc(phoneUrl, 'overview', [], bearer(token + 'x'))).status, 401, 'nor one a character longer')
  assert.equal((await rpc(phoneUrl, 'overview', [], bearer('desk-token'))).status, 401, "the app's token is not the phone's")
  assert.equal((await rpc(deskUrl, 'overview', [], bearer(token))).status, 401, "nor the phone's the app's")
  const ok = await rpc(phoneUrl, 'overview', [], bearer(token))
  assert.equal(ok.status, 200)
  assert.equal(((await ok.json()) as { result: { me: { id: string } } }).result.me.id, 'human')
  assert.equal((await fetch(`${phoneUrl}/events`)).status, 401, 'changes need the token too')

  // What changes how the phone is reached is the app's alone.
  for (const method of ['phone', 'setPhoneTailnet', 'setPhonePort', 'regeneratePhoneToken']) {
    const res = await rpc(phoneUrl, method, [true], bearer(token))
    assert.equal(res.status, 404, `${method} is refused to the phone`)
  }
  const view = ((await (await rpc(deskUrl, 'phone', [], bearer('desk-token'))).json()) as { result: Record<string, unknown> }).result
  assert.equal(view.token, token)
  assert.equal(view.tailnet, false, 'the tailnet is not let in until it is said')
  assert.equal(view.phoneUrl, 'https://laptop.tail1234.ts.net:8443/')
  assert.match(String(view.serveCommand), /^tailscale serve --bg --https=8443 http:\/\/127\.0\.0\.1:\d+$/)

  // A new token: the old one stops working at once.
  await rpc(deskUrl, 'regeneratePhoneToken', [], bearer('desk-token'))
  const fresh = engine.settings.phoneToken()
  assert.notEqual(fresh, token)
  assert.equal((await rpc(phoneUrl, 'overview', [], bearer(token))).status, 401)
  assert.equal((await rpc(phoneUrl, 'overview', [], bearer(fresh))).status, 200)

  const req = (authorization?: string) => ({ headers: authorization ? { authorization } : {} }) as Parameters<typeof tokenMatches>[0]
  assert.equal(tokenMatches(req('Bearer abc'), 'abc'), true)
  assert.equal(tokenMatches(req('Bearer abcd'), 'abc'), false, 'lengths that differ are compared without throwing')
  assert.equal(tokenMatches(req('abc'), 'abc'), false)
  assert.equal(tokenMatches(req(), ''), false, 'an empty token matches nothing')
  assert.equal(tokenMatches(req('Bearer '), ''), false)
}

// ---- 2. The tailnet: what comes through `tailscale serve` only while it is let in ----
{
  const t = engine.settings.phoneToken()
  const forwarded = { 'x-forwarded-for': '100.101.102.103', ...bearer(t) }
  assert.equal((await rpc(phoneUrl, 'overview', [], forwarded)).status, 403, 'refused from the tailnet by default')
  assert.equal((await fetch(`${phoneUrl}/`, { headers: { 'tailscale-user-login': 'a@b.c' } })).status, 403, 'the app itself too')
  // fetch will not set Host, so this one goes by hand.
  const hosted = await new Promise<number>((resolve, reject) =>
    request(`${phoneUrl}/health`, { headers: { host: 'laptop.tail1234.ts.net:8443' } }, (res) => {
      res.resume()
      resolve(res.statusCode ?? 0)
    })
      .on('error', reject)
      .end(),
  )
  assert.equal(hosted, 403, 'and a host that is not this machine')
  assert.equal((await rpc(deskUrl, 'setPhoneTailnet', [true], bearer('desk-token'))).status, 200)
  assert.match(String(served.get('8443')), /^http:\/\/127\.0\.0\.1:\d+$/, 'the app has Tailscale serve it')
  assert.equal(((await (await rpc(deskUrl, 'phone', [], bearer('desk-token'))).json()) as { result: { serving: boolean } }).result.serving, true)
  assert.equal((await rpc(phoneUrl, 'overview', [], forwarded)).status, 200, 'answered once it is let in')
  assert.equal((await rpc(phoneUrl, 'overview', [], { 'x-forwarded-for': '100.1.2.3' })).status, 401, 'still with the token')
  await rpc(deskUrl, 'setPhoneTailnet', [false], bearer('desk-token'))
  assert.equal(served.size, 0, 'and stop')
  assert.equal((await rpc(phoneUrl, 'overview', [], forwarded)).status, 403, 'and refused again once it is not')
  // The app's own listener is reached only from this machine, as before: it has no tailnet gate, as nothing forwards to it.
  assert.equal((await rpc(deskUrl, 'overview', [], { 'x-forwarded-for': '1.2.3.4', ...bearer('desk-token') })).status, 200)
}

// ---- 3. The built app, to whoever asks on this machine; nothing beside it ----
{
  const page = await fetch(`${phoneUrl}/`)
  assert.equal(page.status, 200)
  assert.match(await page.text(), /YAWN/)
  const js = await fetch(`${phoneUrl}/assets/app-1234.js`)
  assert.match(js.headers.get('content-type') ?? '', /javascript/)
  assert.match(js.headers.get('cache-control') ?? '', /immutable/)
  assert.match(await (await fetch(`${phoneUrl}/some/view`)).text(), /YAWN/, 'a path that names no file is the page')
  assert.equal((await fetch(`${phoneUrl}/missing.js`)).status, 404)
  const raw = (path: string) =>
    new Promise<{ status: number; body: string }>((resolve, reject) =>
      request(`${phoneUrl}${path}`, (res) => {
        let body = ''
        res.setEncoding('utf8').on('data', (c: string) => (body += c))
        res.on('end', () => resolve({ status: res.statusCode ?? 0, body }))
      })
        .on('error', reject)
        .end(),
    )
  for (const path of ['/../secret.txt', '/%2e%2e/secret.txt', '/a%2f..%2f..%2fsecret.txt', '/assets/..%2f..%2fsecret.txt']) {
    const res = await raw(path)
    assert.ok(res.status === 403 || res.status === 404, `nothing outside the build: ${path} → ${res.status}`)
    assert.ok(!res.body.includes('not for the phone'), path)
  }
  assert.equal((await raw('/a%2f..%2f..%2fsecret.txt')).status, 403, 'refused, once decoded')
  assert.equal((await fetch(`${deskUrl}/`)).status, 401, "the app's listener serves no files")
}

// ---- 4. The transport, against the phone's listener ----
{
  const kept = new Map<string, string>()
  const storage: KeptStrings = { getItem: (k) => kept.get(k) ?? null, setItem: (k, v) => void kept.set(k, v), removeItem: (k) => void kept.delete(k) }
  let refused = 0
  const http = httpApi({ base: phoneUrl, storage, onRefused: () => refused++, retryMs: 50 })

  http.setToken!('not it')
  await assert.rejects(http.openSource(''), /token is wrong/, 'a wrong token is refused as the source is opened')
  assert.equal(refused, 1)
  assert.equal(http.token!(), null, 'and forgotten')

  http.setToken!(` ${engine.settings.phoneToken()} `)
  assert.equal(kept.get(TOKEN_KEY), engine.settings.phoneToken(), 'kept trimmed')
  assert.equal(await http.openSource(''), source, 'the source is the backend’s')
  await assert.rejects(http.overview(), /Sign in first/, 'calls are as someone')
  const me = await http.signIn('human')
  http.setUser(me.id)
  assert.equal((await http.overview()).me.id, 'human')
  await assert.rejects(http.phone(), /No method/, 'the phone page is not the phone’s')

  // Changes: once as it connects, then as things change.
  let heard = 0
  const stop = http.onChange(() => heard++)
  for (let i = 0; i < 100 && heard < 1; i++) await new Promise((r) => setTimeout(r, 20))
  assert.equal(heard, 1, 'heard as it connects')
  const id = await http.create('From the phone', null)
  for (let i = 0; i < 100 && heard < 2; i++) await new Promise((r) => setTimeout(r, 20))
  assert.ok(heard >= 2, 'heard as something changes')
  assert.equal((await http.detail(id))?.item.title, 'From the phone')
  stop()

  await http.closeSource()
  assert.equal(http.token!(), null, 'disconnecting forgets the token')
}

// ---- 5. The phone's port: taken, it says so rather than failing the backend ----
{
  const blocker = createServer()
  const taken = await listen(blocker, 0)
  await rpc(deskUrl, 'setPhonePort', [taken], bearer('desk-token'))
  assert.match(phone.status().error ?? '', /taken/)
  const bad = (await (await rpc(deskUrl, 'setPhonePort', [80], bearer('desk-token'))).json()) as { error?: string }
  assert.match(bad.error ?? '', /1024 to 65535/)
  blocker.close()
}

// ---- 6. The QR code: the same modules as qrencode's, for one of the eight masks ----
let qrencode = true
try {
  execFileSync('qrencode', ['-V'], { stdio: 'ignore' })
} catch {
  qrencode = false
}
if (qrencode) {
  for (const text of ['hi', 'https://laptop.tail1234.ts.net:8443/#token=abcdefghijklmnopqrstuvwxyz012345', 'x'.repeat(200), 'é€ ' + 'y'.repeat(400)]) {
    const theirs = execFileSync('qrencode', ['-l', 'M', '-8', '-m', '0', '-t', 'ASCII', text], { encoding: 'utf8' })
      .split('\n')
      .filter(Boolean)
      .map((line) => [...line].filter((_, i) => i % 2 === 0).map((c) => c === '#'))
    assert.equal(encodeQr(text).length, theirs.length, `the same version for ${text.length} characters`)
    assert.ok(
      [0, 1, 2, 3, 4, 5, 6, 7].some((m) => JSON.stringify(encodeQr(text, m)) === JSON.stringify(theirs)),
      `the same code for ${text.length} characters`,
    )
  }
} else console.log('phone: the QR check skipped (no qrencode)')

phone.close()
desk.close()
changes.close()
console.log('phone: ok')
process.exit(0)
