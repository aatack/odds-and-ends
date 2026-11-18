// The headless backend: booted as plain `node` (no Electron) on a temp source, with its phone options set by flag
// (--phone-token, --tailnet, --phone-port) rather than from the app's Phone page. It checks that the phone listener
// answers on the chosen port, behind the chosen token, lets the tailnet in only when --tailnet is on, refuses the
// methods that change how it is reached, serves the built app, sends events, and stops cleanly on SIGTERM. It also
// scans the graph: nothing under src/backend, src/engine or src/shared may import Electron (the backend is plain Node).
// Needs the egdb-server binary (npm run egdb:build): the boot part is skipped without it, as test/egdb.mts is.

import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync } from 'node:fs'
import { createServer } from 'node:http'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { PHONE_REFUSED } from '../src/backend/http.ts'
import { egdbServerPath } from '../src/engine/egdb.ts'

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))

// ---- The graph: no Electron under the plain-Node parts (a recursive import scan) ----
{
  const roots = ['src/backend', 'src/engine', 'src/shared']
  const imports = [
    /\bfrom\s+['"]electron(\/[^'"]*)?['"]/,
    /\bimport\s*\(?\s*['"]electron(\/[^'"]*)?['"]/,
    /\brequire\s*\(\s*['"]electron(\/[^'"]*)?['"]/,
  ]
  const walk = (dir: string): string[] =>
    readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
      const p = join(dir, e.name)
      if (e.isDirectory()) return walk(p)
      return /\.tsx?$/.test(e.name) ? [p] : []
    })
  const offenders = roots.flatMap((root) => walk(root)).filter((file) => {
    const text = readFileSync(file, 'utf8')
    return imports.some((re) => re.test(text))
  })
  assert.deepEqual(offenders, [], `these import Electron but must not (the backend is plain Node):\n${offenders.join('\n')}`)
}

const bin = egdbServerPath()
if (bin === 'egdb-server' || !existsSync(bin)) {
  console.log('headless: the boot part skipped (no egdb-server binary: npm run egdb:build)')
  process.exit(0)
}

// A token of at least 24 characters, as the backend demands.
const PHONE_TOKEN = 'headless-test-token-0123456789'
const bearer = (t: string) => ({ authorization: `Bearer ${t}` })
const dir = mkdtempSync(join(tmpdir(), 'workflows-headless-'))

/** A free loopback port: taken, then let go, for a child to bind. */
const freePort = (): Promise<number> =>
  new Promise((resolve, reject) => {
    const s = createServer()
    s.on('error', reject)
    s.listen(0, '127.0.0.1', () => {
      const port = (s.address() as { port: number }).port
      s.close(() => resolve(port))
    })
  })

/** Boots a backend as plain node on its own temp source, on the given phone port, and waits for its backend.json. */
const boot = async (name: string, phonePort: number, tailnet: boolean) => {
  const source = join(dir, name, 'tasks.sqlite')
  const data = `${source}.data`
  const args = ['--import', 'tsx', 'src/backend/index.ts', '--file', source, '--phone-token', PHONE_TOKEN, '--phone-port', String(phonePort)]
  if (tailnet) args.push('--tailnet')
  const child = spawn(process.execPath, args, { stdio: ['ignore', 'pipe', 'pipe'] })
  let output = ''
  child.stdout.on('data', (c) => (output += c))
  child.stderr.on('data', (c) => (output += c))
  for (let i = 0; i < 200 && !existsSync(join(data, 'backend.json')); i++) await sleep(100)
  assert.ok(existsSync(join(data, 'backend.json')), `backend ${name} did not start:\n${output}`)
  return { child, data, base: `http://127.0.0.1:${phonePort}`, output: () => output }
}

const [port1, port2] = [await freePort(), await freePort()]
const one = await boot('tailnet', port1, true)
const rpc = (base: string, method: string, headers: Record<string, string> = {}, body: unknown[] = []) =>
  fetch(`${base}/rpc/${method}`, { method: 'POST', headers: { 'content-type': 'application/json', ...headers }, body: JSON.stringify(body) })

// ---- 1. Health, and the token ----
assert.equal((await fetch(`${one.base}/health`)).status, 200, 'health answers on the phone port')
assert.equal((await rpc(one.base, 'overview')).status, 401, 'no token, no answer')
assert.equal((await rpc(one.base, 'overview', bearer('wrong'))).status, 401, 'a wrong token, no answer')

// ---- 2. Signed in with the right token, a call as that person answers (signIn is an open method) ----
const signedIn = await rpc(one.base, 'signIn', bearer(PHONE_TOKEN), ['Tester'])
assert.equal(signedIn.status, 200)
assert.deepEqual(((await signedIn.json()) as { result: unknown }).result, { id: 'tester', name: 'tester' })
const overview = await rpc(one.base, 'overview', { ...bearer(PHONE_TOKEN), 'x-workflows-user': 'tester' })
assert.equal(overview.status, 200)
assert.equal(((await overview.json()) as { result: { me: { id: string } } }).result.me.id, 'tester')

// ---- 3. The tailnet: let in here because --tailnet is on ----
const forwarded = { ...bearer(PHONE_TOKEN), 'x-workflows-user': 'tester', 'x-forwarded-for': '100.1.2.3' }
assert.equal((await rpc(one.base, 'overview', forwarded)).status, 200, 'a forwarded request is let in with --tailnet')

// ---- 4. The methods that change how the phone is reached are refused on the phone port ----
for (const method of PHONE_REFUSED) assert.equal((await rpc(one.base, method, bearer(PHONE_TOKEN))).status, 404, `${method} is refused`)

// ---- 5. The built app: served when the renderer is beside the backend; else skipped (its place is fixed by __dirname) ----
// The backend runs as src/backend/index.ts here, so its `../renderer` is src/renderer, which holds index.html.
if (existsSync(join(process.cwd(), 'src', 'renderer', 'index.html'))) {
  const page = await fetch(`${one.base}/`)
  assert.equal(page.status, 200)
  assert.match(await page.text(), /YAWN/, 'GET / serves index.html')
} else console.log('headless: GET / skipped (no built renderer at the backend’s __dirname)')

// ---- 6. Events: a stream, behind the token ----
const events = await fetch(`${one.base}/events`, { headers: bearer(PHONE_TOKEN) })
assert.equal(events.status, 200)
assert.match(events.headers.get('content-type') ?? '', /text\/event-stream/, '/events is a server-sent stream')
await events.body?.cancel()

// ---- 7. A second backend, without --tailnet: a forwarded request is refused (403) ----
const two = await boot('loopback', port2, false)
assert.equal((await rpc(two.base, 'overview', { ...bearer(PHONE_TOKEN), 'x-forwarded-for': '100.1.2.3' })).status, 403, 'refused from the tailnet without --tailnet')

// ---- 8. SIGTERM stops each cleanly: exit 0, and backend.json gone ----
for (const b of [one, two]) {
  b.child.kill('SIGTERM')
  const code = await new Promise((r) => b.child.on('exit', (c) => r(c)))
  assert.equal(code, 0, `clean exit:\n${b.output()}`)
  assert.ok(!existsSync(join(b.data, 'backend.json')), 'backend.json is gone once it stops')
}

rmSync(dir, { recursive: true, force: true })
console.log('headless: ok')
process.exit(0)
