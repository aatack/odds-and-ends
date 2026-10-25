// The store against a real egdb-server: what the engine writes is read back the
// same, by a new engine and after the server restarts; a store from before egdb
// is imported; and the backend runs on its own, answering calls and sending
// changes. Needs the egdb-server binary (npm run egdb:build): skipped without it.

import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { DatabaseSync } from 'node:sqlite'
import { EgdbClient } from 'egdb-client'
import { importV4 } from '../src/backend/importV4.ts'
import { keyFileSecrets } from '../src/backend/secrets.ts'
import { egdbServerPath, loadStore, startEgdb } from '../src/engine/egdb.ts'
import { Engine } from '../src/engine/engine.ts'

const bin = egdbServerPath()
if (bin === 'egdb-server' || !existsSync(bin)) {
  console.log('egdb: skipped (no egdb-server binary: npm run egdb:build)')
  process.exit(0)
}

const dir = mkdtempSync(join(tmpdir(), 'workflows-egdb-'))
const token = 'test-token'
const open = async (file: string) => {
  const egdb = await startEgdb(file, token)
  const client = new EgdbClient({ url: egdb.url, token })
  const { store, sink } = await loadStore(client)
  return { egdb, client, store, sink }
}

// ---- 1. What the engine writes is read back the same ----
let clock = 1_000_000
const now = () => clock
const file = join(dir, 'round-trip.egdb')
{
  const { egdb, store } = await open(file)
  const engine = new Engine({ store, now, dataDir: dir })
  const bot = await engine.saveAgent({ name: 'Threepio', instructions: 'Be polite.', provider: 'claude', model: 'opus', debounceMinutes: 2 })
  await engine.saveAgent({ id: bot, name: 'Threepio', instructions: 'Be very polite.', provider: 'claude', model: 'opus', debounceMinutes: 2 })
  const id = (await engine.create('Round trip'))!
  clock += 1000
  await engine.send(id, 'message', `[${bot}]() hello`)
  await engine.addEvents(id, [{ type: 'github', params: { repo: 'o/r', number: 7, until: 'checks' } }])
  await engine.addEvents(id, [{ type: 'script', params: { source: 'async () => ({ done: false, state: 41 })', schedule: [0] } }])
  await engine.checkDue()
  const run = engine.startRun(bot, [id], 'the prompt')
  engine.appendTranscript(run, '{"type":"result"}\n')
  await engine.endRun(run, { exitCode: 0, structured: { updates: [{ item: id, message: 'Hi.', assign: ['human'] }] }, costUsd: 0.5 })
  engine.customActions.save({ type: 'item', name: 'Hello', description: 'd', notes: 'n', source: 'async () => "hi"', icon: 'zap', important: true })
  engine.settings.set('mcp.port', 1234)
  const done = (await engine.create('Done'))!
  await engine.close(done)
  await store.flush()
  const before = { overview: engine.overview(), detail: engine.detail(id)!, trace: store.trace.length, custom: engine.customActions.list() }
  egdb.stop()
  await new Promise((r) => setTimeout(r, 300))

  // A new server on the same file, and a new engine.
  const again = await open(file)
  const engine2 = new Engine({ store: again.store, now, dataDir: dir })
  const after = { overview: engine2.overview(), detail: engine2.detail(id)!, trace: again.store.trace.length, custom: engine2.customActions.list() }
  assert.equal(after.trace, before.trace)
  assert.deepEqual(after.custom, before.custom)
  assert.deepEqual(after.overview.items.map((i) => [i.id, i.title, i.closed, i.assignees, i.blockers.map((b) => b.type)]),
    before.overview.items.map((i) => [i.id, i.title, i.closed, i.assignees, i.blockers.map((b) => b.type)]))
  assert.deepEqual(after.overview.bots.map((b) => [b.id, b.name, b.instructions, b.model, b.debounceMinutes, b.sessionId]),
    before.overview.bots.map((b) => [b.id, b.name, b.instructions, b.model, b.debounceMinutes, b.sessionId]))
  assert.equal(after.overview.bots[0].instructions, 'Be very polite.')
  // Events keep their params, data, memo and order; waits are checked again at once.
  assert.deepEqual(after.detail.events.map((e) => [e.id, e.type, e.params, e.data, e.resolved, e.createdBy]),
    before.detail.events.map((e) => [e.id, e.type, e.params, e.data, e.resolved, e.createdBy]))
  assert.ok(after.detail.events.filter((e) => !e.resolved && e.type !== 'item').every((e) => e.nextCheckAt === clock))
  assert.equal(again.store.events.get(after.detail.events.find((e) => e.type === 'script')!.id)!.memo, 41)
  assert.deepEqual(after.detail.runs, before.detail.runs)
  assert.equal(after.detail.runs[0].costUsd, 0.5)
  assert.match((await engine2.transcript(run))!, /"type":"result"/, 'a transcript is read from egdb when asked for')
  assert.equal(engine2.settings.mcpPort(), 1234)
  // The tree can be walked in egdb itself, from the root.
  const branch = (await again.client.branches())[0].id
  const kids = await again.client.children(branch, '@root', 'child')
  assert.ok(kids.some((l) => l.destination === id) && kids.some((l) => l.destination === bot))
  const removed = engine2.customActions.list()[0].id
  engine2.customActions.remove(removed)
  await again.store.flush()
  const third = await loadStore(again.client)
  assert.equal(third.store.customActions.size, 0, 'a removed custom action is archived in egdb, and not read back')
  again.egdb.stop()
}

// ---- 2. A store from before egdb comes in, once ----
{
  const v4 = join(dir, 'workflows-v4.db')
  const old = new DatabaseSync(v4)
  old.exec(`
    CREATE TABLE events (id TEXT PRIMARY KEY, parent_id TEXT, seq INTEGER NOT NULL, type TEXT NOT NULL, params TEXT NOT NULL,
      created_by TEXT NOT NULL, created_at INTEGER NOT NULL, resolved INTEGER NOT NULL DEFAULT 0, resolved_at INTEGER, resolved_by TEXT,
      cancelled INTEGER NOT NULL DEFAULT 0, started INTEGER NOT NULL DEFAULT 0, data TEXT, check_step INTEGER NOT NULL DEFAULT 0, memo TEXT,
      next_check_at INTEGER, last_error TEXT);
    CREATE TABLE trace (id INTEGER PRIMARY KEY AUTOINCREMENT, at INTEGER NOT NULL, item_id TEXT NOT NULL, event_id TEXT, run_id TEXT,
      actor TEXT NOT NULL, kind TEXT NOT NULL, data TEXT);
    CREATE TABLE runs (id TEXT PRIMARY KEY, item_id TEXT NOT NULL, event_ids TEXT NOT NULL, worker INTEGER NOT NULL, started_at INTEGER NOT NULL,
      trace_mark INTEGER NOT NULL DEFAULT 0, ended_at INTEGER, cost_usd REAL, turns INTEGER, result TEXT, prompt TEXT NOT NULL, transcript TEXT NOT NULL DEFAULT '');
    CREATE TABLE settings (key TEXT PRIMARY KEY, value TEXT NOT NULL);
    CREATE TABLE custom_actions (id TEXT PRIMARY KEY, type TEXT NOT NULL, name TEXT NOT NULL, description TEXT NOT NULL DEFAULT '',
      notes TEXT NOT NULL DEFAULT '', source TEXT NOT NULL DEFAULT '', icon TEXT NOT NULL DEFAULT 'zap', important INTEGER NOT NULL DEFAULT 0,
      created_at INTEGER NOT NULL, updated_at INTEGER NOT NULL);
    INSERT INTO events VALUES ('item-1', NULL, 1, 'item', '{"title":"Old task","sessionId":"s-1"}', 'human', 10, 0, NULL, NULL, 0, 1, NULL, 0, NULL, NULL, NULL);
    INSERT INTO events VALUES ('msg-1', 'item-1', 1, 'message', '{"text":"Hello"}', 'human', 11, 1, 11, 'human', 0, 1, NULL, 0, NULL, NULL, NULL);
    INSERT INTO events VALUES ('bot-1', 'item-1', 2, 'bot', '{}', 'human', 12, 1, 20, 'bot', 0, 1, '{"runId":"run-1"}', 0, NULL, NULL, NULL);
    INSERT INTO events VALUES ('bot-2', 'item-1', 3, 'bot', '{}', 'human', 30, 0, NULL, NULL, 0, 1, NULL, 0, NULL, NULL, NULL);
    INSERT INTO trace VALUES (1, 10, 'item-1', 'item-1', NULL, 'human', 'event_added', '{"type":"item"}');
    INSERT INTO trace VALUES (2, 11, 'item-1', 'msg-1', NULL, 'human', 'event_added', NULL);
    INSERT INTO trace VALUES (5, 12, 'item-1', 'bot-1', NULL, 'human', 'event_added', NULL);
    INSERT INTO runs VALUES ('run-1', 'item-1', '["bot-1"]', 0, 13, 5, 20, 0.25, 4, 'Done.', 'the old prompt', '{"old":"transcript"}');
    INSERT INTO settings VALUES ('mcp.port', '4321');
    INSERT INTO custom_actions VALUES ('ca-1', 'item', 'Old action', '', '', 'async () => null', 'zap', 0, 1, 2);
  `)
  old.close()
  const { egdb, store } = await open(join(dir, 'imported.egdb'))
  const counts = await importV4(v4, store, 99)
  assert.deepEqual(counts, { events: 4, trace: 3, runs: 1 })
  await assert.rejects(importV4(v4, store, 99), /not empty/)
  await store.flush()
  egdb.stop()
  await new Promise((r) => setTimeout(r, 300))
  const back = await open(join(dir, 'imported.egdb'))
  const engine = new Engine({ store: back.store, now, dataDir: dir })
  const it = engine.get('item-1')!
  assert.equal(it.title, 'Old task')
  assert.deepEqual(it.blockers, [], 'an open bot blocker from before is cancelled')
  assert.ok(it.withHuman, 'and the item, which nobody has, is the human\'s')
  assert.equal(engine.event('bot-2')!.cancelled, true)
  assert.equal(engine.event('bot-1')!.message, null, 'an old bot event that ran reads as it did')
  assert.deepEqual(back.store.trace.map((t) => t.id), [1, 2, 5, 6], 'trace ids kept; the cancel traced after them')
  assert.deepEqual(engine.detail('item-1')!.runs.map((r) => [r.id, r.agentId, r.itemIds, r.costUsd]), [['run-1', null, ['item-1'], 0.25]])
  assert.match((await engine.transcript('run-1'))!, /"old":"transcript"/)
  assert.equal(engine.settings.mcpPort(), 4321)
  assert.equal(engine.customActions.list()[0].name, 'Old action')
  back.egdb.stop()
}

// ---- 3. Secrets under a key of the backend's own ----
{
  const secrets = keyFileSecrets(dir)
  const stored = secrets.encrypt('xoxp-secret')
  assert.match(stored, /^aes:/)
  assert.equal(keyFileSecrets(dir).decrypt(stored), 'xoxp-secret', 'the key lasts')
  assert.throws(() => secrets.decrypt('safe:abc'), /older app/)
  assert.equal(secrets.decrypt('plain:x'), 'x')
}

// ---- 4. The backend on its own: it imports, answers calls, and sends changes ----
{
  const data = join(dir, 'backend')
  const child = spawn(process.execPath, ['--import', 'tsx', 'src/backend/index.ts', '--data', data], { stdio: ['ignore', 'pipe', 'pipe'] })
  let output = ''
  child.stdout.on('data', (c) => (output += c))
  child.stderr.on('data', (c) => (output += c))
  for (let i = 0; i < 200 && !existsSync(join(data, 'backend.json')); i++) await new Promise((r) => setTimeout(r, 100))
  assert.ok(existsSync(join(data, 'backend.json')), output)
  const info = JSON.parse(readFileSync(join(data, 'backend.json'), 'utf8')) as { url: string; token: string }
  const call = async (method: string, ...args: unknown[]) => {
    const res = await fetch(`${info.url}/rpc/${method}`, { method: 'POST', headers: { authorization: `Bearer ${info.token}` }, body: JSON.stringify(args) })
    const out = (await res.json()) as { result?: unknown; error?: string }
    if (out.error) throw new Error(out.error)
    return out.result
  }
  assert.equal((await fetch(`${info.url}/rpc/overview`, { method: 'POST' })).status, 401)
  const changes: string[] = []
  const events = await fetch(`${info.url}/events`, { headers: { authorization: `Bearer ${info.token}` } })
  const reader = events.body!.getReader()
  void (async () => {
    for (let c = await reader.read(); !c.done; c = await reader.read()) changes.push(new TextDecoder().decode(c.value))
  })()
  const overview = (await call('overview')) as { bots: { name: string }[] }
  assert.deepEqual(overview.bots.map((b) => b.name), ['Claude'], 'the first start makes a bot')
  const id = (await call('create', 'From the API', null)) as string
  await call('send', id, 'message', 'A note')
  for (let i = 0; i < 50 && !changes.some((c) => c.includes('event: change')); i++) await new Promise((r) => setTimeout(r, 50))
  assert.ok(changes.some((c) => c.includes('event: change')), 'changes are sent')
  await assert.rejects(call('send', id, 'nope', 'x'), /No preset nope/)
  await assert.rejects(call('nothing'), /No method/)
  // A second backend on the same data refuses to start.
  const second = spawn(process.execPath, ['--import', 'tsx', 'src/backend/index.ts', '--data', data], { stdio: ['ignore', 'pipe', 'pipe'] })
  let secondOut = ''
  second.stderr.on('data', (c) => (secondOut += c))
  assert.equal(await new Promise((r) => second.on('exit', r)), 1)
  assert.match(secondOut, /running on .* already/)
  await reader.cancel()
  child.kill('SIGTERM')
  await new Promise((r) => child.on('exit', r))
  assert.ok(!existsSync(join(data, 'backend.json')), 'it says it has gone')
  // What it wrote lasts: it starts again with the same items.
  const again = spawn(process.execPath, ['--import', 'tsx', 'src/backend/index.ts', '--data', data], { stdio: ['ignore', 'pipe', 'pipe'] })
  for (let i = 0; i < 200 && !existsSync(join(data, 'backend.json')); i++) await new Promise((r) => setTimeout(r, 100))
  const info2 = JSON.parse(readFileSync(join(data, 'backend.json'), 'utf8')) as { url: string; token: string }
  const res = await fetch(`${info2.url}/rpc/detail`, { method: 'POST', headers: { authorization: `Bearer ${info2.token}` }, body: JSON.stringify([id]) })
  const detail = ((await res.json()) as { result: { events: { params: { text?: string } }[] } }).result
  assert.equal(detail.events[0].params.text, 'A note')
  again.kill('SIGTERM')
  await new Promise((r) => again.on('exit', r))
}

console.log('egdb: ok')
