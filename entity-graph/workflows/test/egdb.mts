// The store against a real egdb-server: what the engine writes is read back the
// same, by a new engine and after the server restarts, and kept as plain values
// and links; and the backend runs on its own, answering calls and sending
// changes. Needs the egdb-server binary (npm run egdb:build): skipped without it.

import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync } from 'node:fs'
import { homedir, tmpdir } from 'node:os'
import { join, relative } from 'node:path'
import { EgdbClient } from 'egdb-client'
import { egdbServerPath, loadStore, startEgdb } from '../src/engine/egdb.ts'
import { Engine } from '../src/engine/engine.ts'
import { EVENT_TYPES } from '../src/engine/eventTypes.ts'

const bin = egdbServerPath()
if (bin === 'egdb-server' || !existsSync(bin)) {
  console.log('egdb: skipped (no egdb-server binary: npm run egdb:build)')
  process.exit(0)
}

const dir = mkdtempSync(join(tmpdir(), 'workflows-egdb-'))
const token = 'test-token'
const blocking = (type: string) => EVENT_TYPES.find((t) => t.type === type)?.blocking ?? true
const open = async (file: string) => {
  const egdb = await startEgdb(file, token)
  const client = new EgdbClient({ url: egdb.url, token })
  const { store, sink } = await loadStore(client, blocking)
  return { egdb, client, store, sink }
}

// ---- 1. What the engine writes is read back the same ----
// egdb keeps time moving forward, so the clock starts now.
let clock = Date.now()
const now = () => clock
const file = join(dir, 'round-trip.egdb')
{
  const { egdb, store } = await open(file)
  const engine = new Engine({ store, now, dataDir: dir })
  await engine.signIn('alex')
  const bot = await engine.saveAgent({ name: 'Threepio', instructions: 'Be polite.', provider: 'claude', model: 'opus', debounceMinutes: 2 })
  await engine.saveAgent({ id: bot, name: 'Threepio', instructions: 'Be very polite.', provider: 'claude', model: 'opus', debounceMinutes: 2 })
  const id = (await engine.create('Round trip'))!
  clock += 1000
  await engine.send(id, 'note', `[${bot}]() hello`)
  await engine.send(bot, 'note', 'hello, bot')
  await engine.addEvents(id, [{ type: 'script', params: { source: 'async () => null', schedule: [600] } }])
  await engine.addEvents(id, [{ type: 'script', params: { source: 'async () => ({ done: false, state: 41 })', schedule: [0] } }])
  await engine.checkDue()
  const run = engine.startRun(bot, [id])
  await engine.endRun(run, { exitCode: 0, structured: { updates: [{ item: id, message: 'Hi.', assign: ['alex'] }] }, costUsd: 0.5 })
  // A thumbs-up is a `reactions` value on the event: those who gave one.
  const hi = engine.detail(id)!.events.find((e) => e.params.text === 'Hi.')!.id
  await engine.react(hi, true, 'alex')
  await engine.react(hi, true, `bot:${bot}`)
  await engine.react(hi, false, `bot:${bot}`)
  engine.customActions.save({ type: 'task', name: 'Hello', description: 'd', notes: 'n', source: 'async () => "hi"', icon: 'zap', important: true })
  engine.settings.set('mcp.port', 1234)
  const done = (await engine.create('Done'))!
  await engine.close(done)
  await store.flush()
  const before = { overview: engine.overview(), detail: engine.detail(id)!, bot: engine.agent(bot)!, custom: engine.customActions.list() }
  egdb.stop()
  await new Promise((r) => setTimeout(r, 300))

  // A new server on the same file, and a new engine.
  const again = await open(file)
  const engine2 = new Engine({ store: again.store, now, dataDir: dir })
  const after = { overview: engine2.overview(), detail: engine2.detail(id)!, bot: engine2.agent(bot)!, custom: engine2.customActions.list() }
  assert.deepEqual(after.custom, before.custom)
  assert.deepEqual(after.overview.items.map((i) => [i.id, i.title, i.closed, i.assignees, i.blockers.map((b) => b.type)]),
    before.overview.items.map((i) => [i.id, i.title, i.closed, i.assignees, i.blockers.map((b) => b.type)]))
  assert.deepEqual(after.overview.bots.map((b) => [b.id, b.name, b.instructions, b.model, b.debounceMinutes, b.sessionId]),
    before.overview.bots.map((b) => [b.id, b.name, b.instructions, b.model, b.debounceMinutes, b.sessionId]))
  assert.equal(after.overview.bots[0].instructions, 'Be very polite.')
  // Events keep their params, data, memo and order; waits are checked again at once.
  assert.deepEqual(after.detail.events.map((e) => [e.id, e.type, e.params, e.data, e.resolved, e.createdBy]),
    before.detail.events.map((e) => [e.id, e.type, e.params, e.data, e.resolved, e.createdBy]))
  assert.ok(after.detail.events.filter((e) => !e.resolved && e.type !== 'task' && e.type !== 'script').every((e) => e.nextCheckAt === clock))
  assert.equal(again.store.events.get(after.detail.events.filter((e) => e.type === 'script').at(-1)!.id)!.memo, 41)
  assert.deepEqual(after.detail.events.find((e) => e.id === hi)!.reactions, ['alex'])
  assert.ok(after.detail.events.filter((e) => e.id !== hi).every((e) => e.reactions.length === 0))
  // The edit log: what was done, by whom and when, the newest first.
  const log = await again.store.log({ entities: [hi] })
  assert.deepEqual(log.filter((e) => e.key === 'reactions').map((e) => [e.by, e.value]),
    [[`bot:${bot}`, ['alex']], [`bot:${bot}`, ['alex', `bot:${bot}`]], ['alex', ['alex']]])
  assert.ok(log.some((e) => e.kind === 'link' && e.key === 'contains' && e.other === hi))
  const renamed = await again.store.log({ entities: [id], keys: ['title'] })
  assert.deepEqual(renamed.map((e) => e.value), ['Round trip'])
  assert.deepEqual(await again.store.log({ authors: ['nobody'] }), [])
  // The two log tools: what happened to an item, and what someone did.
  const ctx = { engine: engine2, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }
  const toItem = (await engine2.callAction('edit_log', { item_id: hi }, ctx)) as { entity: string; other: string | null }[]
  assert.ok(toItem.length > 0 && toItem.length <= 10 && toItem.every((e) => e.entity === hi || e.other === hi))
  const byBot = (await engine2.callAction('activity_log', { who: 'Threepio', limit: 3 }, ctx)) as { by: string }[]
  assert.ok(byBot.length > 0 && byBot.length <= 3 && byBot.every((e) => e.by === `bot:${bot}`))
  // Who made each event, and when, is the link to it.
  assert.deepEqual(after.detail.events.map((e) => [e.createdAt, e.createdBy]), before.detail.events.map((e) => [e.createdAt, e.createdBy]))
  assert.deepEqual(after.bot.lastRun, before.bot.lastRun)
  assert.deepEqual([after.bot.lastRun!.id, after.bot.lastRun!.costUsd], [run, 0.5], 'a bot keeps its last run as a value; egdb keeps the rest')
  // A note is its text, and nothing else; where it sits, who wrote it and when, is a `contains` link.
  const note = after.detail.events.find((e) => e.type === 'note')!
  const seen = await engine2.inspect(note.id)
  assert.deepEqual(seen.values, { type: 'note', text: note.params.text })
  assert.deepEqual([seen.createdAt, seen.createdBy], [note.createdAt, note.createdBy])
  assert.ok(seen.links.some((l) => l.type === 'contains' && l.direction === 'inbound' && l.other === id))
  // Who has a task is an `assigned_to` link.
  assert.ok((await engine2.inspect(id)).links.some((l) => l.type === 'assigned_to' && l.other === 'alex'))
  // A closed task is `resolved: true`; when it closed, and who closed it, is egdb's event for that value.
  const closed = await engine2.inspect(done)
  assert.equal(closed.values.resolved, true)
  assert.ok(!('resolved_at' in closed.values) && !('resolved_by' in closed.values))
  assert.deepEqual([closed.set.resolved.at, closed.set.resolved.by], [engine.event(done)!.resolvedAt, 'alex'])
  assert.deepEqual([engine2.event(done)!.resolvedAt, engine2.event(done)!.resolvedBy], [engine.event(done)!.resolvedAt, 'alex'])
  assert.equal(engine2.settings.mcpPort(), 1234)
  // The tree can be walked in egdb itself, from the root.
  const branch = (await again.client.branches())[0].id
  const kids = await again.client.children(branch, '@root', 'contains')
  assert.ok(kids.some((l) => l.destination === id) && kids.some((l) => l.destination === bot))
  const removed = engine2.customActions.list()[0].id
  engine2.customActions.remove(removed)
  await again.store.flush()
  // Reopened, a task is open when read back too.
  await engine2.reopen(done, 'alex')
  await again.store.flush()
  // A later `contains` link is a link, not a move: read back, the note still sits where it was made.
  const linkedTo = (await engine2.create('Linked'))!
  await engine2.link(linkedTo, [note.id], 'alex')
  await again.store.flush()
  const third = await loadStore(again.client, blocking)
  assert.equal(third.store.events.get(note.id)!.parent_id, id)
  assert.deepEqual(third.store.linked(linkedTo).map((l) => l.id), [note.id])
  assert.deepEqual(third.store.containers(note.id), [id, linkedTo])
  assert.equal(third.store.customActions.size, 0, 'a removed custom action is archived in egdb, and not read back')
  assert.equal(third.store.events.get(done)!.resolved, false, 'a reopened task reads back open')
  again.egdb.stop()
}

// ---- 2. The backend on its own, on a source it makes: it answers calls, sends changes, and keeps what it was given ----
{
  // `~/` is the home folder; a source that is not there is made, its folder too.
  const typed = `~/${relative(homedir(), join(dir, 'fresh', 'tasks.sqlite'))}`
  const source = join(dir, 'fresh', 'tasks.sqlite')
  const data = `${source}.data`
  const child = spawn(process.execPath, ['--import', 'tsx', 'src/backend/index.ts', '--file', typed], { stdio: ['ignore', 'pipe', 'pipe'] })
  let output = ''
  child.stdout.on('data', (c) => (output += c))
  child.stderr.on('data', (c) => (output += c))
  for (let i = 0; i < 200 && !existsSync(join(data, 'backend.json')); i++) await new Promise((r) => setTimeout(r, 100))
  assert.ok(existsSync(join(data, 'backend.json')), output)
  const info = JSON.parse(readFileSync(join(data, 'backend.json'), 'utf8')) as { url: string; token: string }
  const call = async (method: string, ...args: unknown[]) => {
    const res = await fetch(`${info.url}/rpc/${method}`, {
      method: 'POST',
      headers: { authorization: `Bearer ${info.token}`, 'x-workflows-user': 'tester' },
      body: JSON.stringify(args),
    })
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
  // Signed in as nobody, or as nobody there is, only signing in works; signing in as someone new makes them.
  const as = async (user: string | null, method: string, ...args: unknown[]) =>
    (await (
      await fetch(`${info.url}/rpc/${method}`, {
        method: 'POST',
        headers: { authorization: `Bearer ${info.token}`, ...(user ? { 'x-workflows-user': user } : {}) },
        body: JSON.stringify(args),
      })
    ).json()) as { result?: unknown; error?: string }
  assert.match(String((await as(null, 'overview')).error), /Sign in first/)
  assert.match(String((await as('nobody', 'overview')).error), /Nobody is called nobody/)
  assert.deepEqual((await as(null, 'signIn', 'Tester')).result, { id: 'tester', name: 'tester' })
  const overview = (await call('overview')) as { bots: unknown[]; users: unknown[]; me: { id: string } }
  assert.deepEqual([overview.me.id, overview.users.length, overview.bots.length], ['tester', 1, 0], 'a fresh source has nothing in it but who signed in')
  assert.ok(existsSync(source), 'the source was made')
  const id = (await call('create', 'From the API', null)) as string
  await call('send', id, 'note', 'A note')
  for (let i = 0; i < 50 && !changes.some((c) => c.includes('event: change')); i++) await new Promise((r) => setTimeout(r, 50))
  assert.ok(changes.some((c) => c.includes('event: change')), 'changes are sent')
  await assert.rejects(call('send', id, 'nope', 'x'), /No preset nope/)
  await assert.rejects(call('nothing'), /No method/)
  // A second backend on the same data refuses to start.
  const second = spawn(process.execPath, ['--import', 'tsx', 'src/backend/index.ts', '--file', typed], { stdio: ['ignore', 'pipe', 'pipe'] })
  let secondOut = ''
  second.stderr.on('data', (c) => (secondOut += c))
  assert.equal(await new Promise((r) => second.on('exit', r)), 1)
  assert.match(secondOut, /running on .* already/)
  await reader.cancel()
  child.kill('SIGTERM')
  await new Promise((r) => child.on('exit', r))
  assert.ok(!existsSync(join(data, 'backend.json')), 'it says it has gone')
  // What it wrote lasts: it starts again with the same items.
  const again = spawn(process.execPath, ['--import', 'tsx', 'src/backend/index.ts', '--file', typed], { stdio: ['ignore', 'pipe', 'pipe'] })
  for (let i = 0; i < 200 && !existsSync(join(data, 'backend.json')); i++) await new Promise((r) => setTimeout(r, 100))
  const info2 = JSON.parse(readFileSync(join(data, 'backend.json'), 'utf8')) as { url: string; token: string }
  const res = await fetch(`${info2.url}/rpc/detail`, { method: 'POST', headers: { authorization: `Bearer ${info2.token}`, 'x-workflows-user': 'tester' }, body: JSON.stringify([id]) })
  const detail = ((await res.json()) as { result: { events: { params: { text?: string } }[] } }).result
  assert.equal(detail.events[0].params.text, 'A note')
  again.kill('SIGTERM')
  await new Promise((r) => again.on('exit', r))
}

console.log('egdb: ok')
