// Desktop client mode: the Electron window opens a backend running elsewhere by its URL and the phone token, through
// src/main/backend.ts `remote()`. This boots a backend as plain `node` on a temp source (with --phone-token and
// --phone-port, as a headless host is set up) and checks that remote() reaches it: `source` answers, a wrong token is
// a plain error, a call as a signed-in person works, a change fires onChange, and a URL with nothing listening is a
// clear error. It also unit-tests the URL detection helper (isRemote). Needs the egdb-server binary (npm run
// egdb:build): the boot part is skipped without it, as test/headless.mts is.

import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { createServer } from 'node:http'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { isRemote } from '../src/backend/info.ts'
import { remote } from '../src/main/backend.ts'
import { egdbServerPath } from '../src/engine/egdb.ts'

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))

// ---- 1. isRemote: a URL (http/https) is remote; a file path, or anything else, is not ----
assert.equal(isRemote('https://host.tailnet.ts.net:8443/'), true)
assert.equal(isRemote('http://127.0.0.1:47392/'), true)
assert.equal(isRemote('  https://host/  '), true, 'trimmed first')
assert.equal(isRemote('HTTPS://host/'), true, 'the scheme is case-insensitive')
assert.equal(isRemote('~/tasks/tasks.sqlite'), false)
assert.equal(isRemote('/home/me/tasks.sqlite'), false)
assert.equal(isRemote('tasks.sqlite'), false)
assert.equal(isRemote('ftp://host/'), false, 'only http and https')
assert.equal(isRemote(''), false)

const bin = egdbServerPath()
if (bin === 'egdb-server' || !existsSync(bin)) {
  console.log('remote: the boot part skipped (no egdb-server binary: npm run egdb:build)')
  process.exit(0)
}

// ---- 2. An http URL with nothing listening: a clear error, not a hang ----
const dead = createServer()
const deadPort: number = await new Promise((resolve) => dead.listen(0, '127.0.0.1', () => resolve((dead.address() as { port: number }).port)))
await new Promise<void>((r) => dead.close(() => r()))
await assert.rejects(remote(`http://127.0.0.1:${deadPort}/`, 'a-long-enough-token-0123456789'), /No backend answers at/)

// A token of at least 24 characters, as the backend demands.
const PHONE_TOKEN = 'remote-test-token-0123456789'
const dir = mkdtempSync(join(tmpdir(), 'workflows-remote-'))

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

/** Boots a backend as plain node on a temp source, on the given phone port, and waits for its backend.json. */
const boot = async (phonePort: number) => {
  const source = join(dir, 'tasks.sqlite')
  const data = `${source}.data`
  const args = ['--import', 'tsx', 'src/backend/index.ts', '--file', source, '--phone-token', PHONE_TOKEN, '--phone-port', String(phonePort)]
  const child = spawn(process.execPath, args, { stdio: ['ignore', 'pipe', 'pipe'] })
  let output = ''
  child.stdout.on('data', (c) => (output += c))
  child.stderr.on('data', (c) => (output += c))
  for (let i = 0; i < 200 && !existsSync(join(data, 'backend.json')); i++) await sleep(100)
  assert.ok(existsSync(join(data, 'backend.json')), `the backend did not start:\n${output}`)
  return { child, base: `http://127.0.0.1:${phonePort}/`, output: () => output }
}

const phonePort = await freePort()
const server = await boot(phonePort)

// ---- 3. A wrong token: a plain error, the URL and the token checked as it opens ----
await assert.rejects(remote(server.base, 'wrong-but-long-enough-token-xyz'), /The token is refused\./)

// ---- 4. The right token: the source method answers (a trailing slash is trimmed) ----
const back = await remote(server.base, PHONE_TOKEN)
assert.equal(back.owned, false, 'a remote backend is not owned by the window')
assert.equal(back.file, `http://127.0.0.1:${phonePort}`, 'its source is the URL, without a trailing slash')
assert.equal(typeof (await back.call('source', [])), 'string', 'source answers through remote()')

// ---- 5. A call as a person, after signing in: overview answers for them (signIn is an open method) ----
await back.call('signIn', ['Tester'])
const overview = (await back.call('overview', [], 'tester')) as { me: { id: string } }
assert.equal(overview.me.id, 'tester', 'a call as the signed-in person works')

// ---- 6. onChange fires after a write through the API ----
let changes = 0
back.onChange(() => (changes += 1))
// The first fire comes as it connects (anything may have changed meanwhile): wait for it, then count from there.
for (let i = 0; i < 50 && changes === 0; i++) await sleep(50)
const before = changes
await back.call('create', ['From the client', null], 'tester')
for (let i = 0; i < 100 && changes <= before; i++) await sleep(50)
assert.ok(changes > before, 'onChange fires after a write through the API')

// ---- Done: stop listening, then the backend ----
await back.close()
server.child.kill('SIGTERM')
await new Promise((r) => server.child.on('exit', r))
rmSync(dir, { recursive: true, force: true })
console.log('remote: ok')
process.exit(0)
