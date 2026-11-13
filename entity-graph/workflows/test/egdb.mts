// The store against a real egdb-server: what the engine writes is read back the
// same, by a new engine and after the server restarts, and kept as plain values
// and links; and the backend runs on its own, answering calls and sending
// changes. Needs the egdb-server binary (npm run egdb:build): skipped without it.

import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync } from 'node:fs'
import { homedir, tmpdir } from 'node:os'
import { join, relative } from 'node:path'
import { EgdbClient, type Op } from 'egdb-client'
import { egdbServerPath, loadStore, startEgdb, storeBranch } from '../src/engine/egdb.ts'
import { Engine } from '../src/engine/engine.ts'
import { ITEM_TYPES } from '../src/engine/itemTypes.ts'

const bin = egdbServerPath()
if (bin === 'egdb-server' || !existsSync(bin)) {
  console.log('egdb: skipped (no egdb-server binary: npm run egdb:build)')
  process.exit(0)
}

const dir = mkdtempSync(join(tmpdir(), 'workflows-egdb-'))
const token = 'test-token'
const blocking = (type: string) => ITEM_TYPES.find((t) => t.type === type)?.blocking ?? true
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
  const bot = await engine.saveAgent({ name: 'Threepio', instructions: 'Be polite.', provider: 'claude', model: 'opus', cycleMinutes: 2 })
  await engine.saveAgent({ id: bot, name: 'Threepio', instructions: 'Be very polite.', provider: 'claude', model: 'opus', cycleMinutes: 2 })
  const id = (await engine.create('Round trip'))!
  clock += 1000
  await engine.send(id, 'note', `[${bot}]() hello`)
  await engine.send(bot, 'note', 'hello, bot')
  await engine.send(id, 'note', 'see https://example.com/a?b=1#c')
  await engine.addItems(id, [{ type: 'script', params: { source: 'return null' } }])
  const [stateful] = await engine.addItems(id, [{ type: 'script', params: { source: 'return { state: 41 }' } }])
  await engine.runNow(stateful, 'alex')
  const run = engine.startRun(bot, [id])
  await engine.endRun(run, { exitCode: 0, structured: { updates: [{ item: id, message: 'Hi.', assign: ['alex'] }] }, costUsd: 0.5 })
  // A fresh session's recap: what was given to it, and what it did, each named beside its id.
  const recap = (await engine.recap(bot))!
  assert.match(recap, new RegExp(`alex gave you "Round trip" \\[${id}\\]`))
  assert.match(recap, new RegExp(`set text on a topic \\[[^\\]]+\\] in "Round trip" \\[${id}\\]: Hi\\.`))
  assert.match(recap, /assigned "Round trip" .* to @alex/)
  assert.doesNotMatch(recap, /last_run|session_id/)
  // An item's values keep their history: who set each, and when.
  const hi = engine.detail(id)!.children.find((e) => e.params.text === 'Hi.')!.id
  await engine.setValue(hi, 'colour', 'red', 'alex')
  await engine.setValue(hi, 'colour', 'blue', `bot:${bot}`)
  engine.customActions.save({ type: 'topic', name: 'Hello', description: 'd', notes: 'n', source: 'async () => "hi"', icon: 'zap', important: true })
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
  const listed = (o: typeof before.overview) => o.items.map((i) => [i.id, i.title, i.closed, i.assignees, i.blockers.map((b) => b.type)]).sort((a, b) => String(a[0]).localeCompare(String(b[0])))
  assert.deepEqual(listed(after.overview), listed(before.overview))
  assert.deepEqual(after.overview.bots.map((b) => [b.id, b.name, b.instructions, b.model, b.cycleMinutes, b.sessionId]),
    before.overview.bots.map((b) => [b.id, b.name, b.instructions, b.model, b.cycleMinutes, b.sessionId]))
  assert.equal(after.overview.bots[0].instructions, 'Be very polite.')
  // Items keep their params, data, memo and order; waits are checked again at once.
  assert.deepEqual(after.detail.children.map((e) => [e.id, e.type, e.params, e.data, e.resolved, e.createdBy]),
    before.detail.children.map((e) => [e.id, e.type, e.params, e.data, e.resolved, e.createdBy]))
  assert.ok(after.detail.children.filter((e) => !e.resolved && e.type !== 'topic' && e.type !== 'script').every((e) => e.nextCheckAt === clock))
  assert.equal(again.store.items.get(after.detail.children.filter((e) => e.type === 'script').at(-1)!.id)!.memo, 41)
  assert.equal(after.detail.children.find((e) => e.id === hi)!.params.colour, 'blue')
  assert.deepEqual([engine2.info('https://example.com/a?b=1#c')!.type, engine2.info('https://example.com/a?b=1#c')!.parentId], ['link', null], 'an item whose id is a URL reads back, in no task')
  // The edit log: what was done, by whom and when, the newest first.
  const log = await again.store.log({ entities: [hi] })
  assert.deepEqual(log.filter((e) => e.key === 'colour').map((e) => [e.by, e.value]), [[`bot:${bot}`, 'blue'], ['alex', 'red']])
  assert.ok(log.some((e) => e.kind === 'link' && e.key === 'comments' && e.other === hi))
  const renamed = await again.store.log({ entities: [id], keys: ['text'] })
  assert.deepEqual(renamed.map((e) => e.value), ['Round trip'])
  assert.deepEqual(await again.store.log({ authors: ['nobody'] }), [])
  // The two log tools: what happened to an item, and what someone did.
  const ctx = { engine: engine2, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }
  const toItem = (await engine2.callAction('edit_log', { item_id: hi }, ctx)) as { entity: string; other: string | null }[]
  assert.ok(toItem.length > 0 && toItem.length <= 10 && toItem.every((e) => e.entity === hi || e.other === hi))
  const byBot = (await engine2.callAction('activity_log', { who: 'Threepio', limit: 3 }, ctx)) as { by: string }[]
  assert.ok(byBot.length > 0 && byBot.length <= 3 && byBot.every((e) => e.by === `bot:${bot}`))
  // Who made each item, and when, is the link to it.
  assert.deepEqual(after.detail.children.map((e) => [e.createdAt, e.createdBy]), before.detail.children.map((e) => [e.createdAt, e.createdBy]))
  assert.deepEqual(after.bot.lastRun, before.bot.lastRun)
  assert.deepEqual([after.bot.lastRun!.id, after.bot.lastRun!.costUsd], [run, 0.5], 'a bot keeps its last run as a value; egdb keeps the rest')
  // A comment is its text, and nothing else; where it sits is a `comments` link, and who wrote it and when, its `type`.
  const note = after.detail.children.find((e) => e.type === 'topic')!
  const seen = await engine2.inspect(note.id)
  assert.deepEqual(seen.values, { type: 'topic', text: note.params.text })
  assert.deepEqual([seen.createdAt, seen.createdBy], [note.createdAt, note.createdBy])
  assert.ok(seen.links.some((l) => l.type === 'comments' && l.direction === 'inbound' && l.other === id))
  // Who has a topic is an `assignees` link; who owns it, an `owns` link to it.
  assert.ok((await engine2.inspect(id)).links.some((l) => l.type === 'assignees' && l.other === 'alex'))
  assert.equal(engine2.owner(id), 'alex')
  assert.equal(engine2.owner(bot), 'alex', 'a bot is owned by whoever made it')
  // A closed topic is `open: false`; when it closed, and who closed it, is egdb's event for that value.
  const closed = await engine2.inspect(done)
  assert.equal(closed.values.open, false)
  assert.ok(!('resolved' in closed.values) && !('resolved_at' in closed.values))
  assert.deepEqual([closed.set.open.at, closed.set.open.by], [engine.info(done)!.resolvedAt, 'alex'])
  assert.deepEqual([engine2.info(done)!.resolvedAt, engine2.info(done)!.resolvedBy], [engine.info(done)!.resolvedAt, 'alex'])
  assert.equal(engine2.settings.mcpPort(), 1234)
  const branch = (await again.client.branches())[0].id
  const removed = engine2.customActions.list()[0].id
  engine2.customActions.remove(removed)
  await again.store.flush()
  // Reopened, a task is open when read back too.
  await engine2.reopen(done, 'alex')
  await again.store.flush()
  // A pin is a `pins` link, not a move: read back, the comment still sits where it was made, and is pinned. A later
  // `contains` link (an older build's link_items) is read in as a pin, once: unpinned, it stays unpinned.
  const linkedTo = (await engine2.create('Linked'))!
  await engine2.pin(linkedTo, [note.id], true, 'alex')
  const older = (await engine2.create('Older'))!
  await again.store.flush()
  await again.client.write({ branch, author: 'alex', timestamp: Date.now() + 1000, ops: [{ link: { source: older, destination: note.id, link_type: 'contains', sort: { explicit: Date.now() + 1000 } } }] })
  await again.store.flush()
  const third = await loadStore(again.client, blocking)
  assert.equal(third.store.items.get(note.id)!.parent_id, id)
  assert.deepEqual(third.store.pins(linkedTo).map((p) => p.id), [note.id])
  assert.deepEqual(third.store.pinnedIn(note.id), [linkedTo, older])
  third.store.unpin(older, note.id, 'alex', Date.now())
  third.store.unpin(linkedTo, note.id, 'alex', Date.now())
  await third.store.flush()
  const fourth = await loadStore(again.client, blocking)
  assert.deepEqual(fourth.store.pinnedIn(note.id), [], 'unpinned, both stay unpinned')
  assert.equal(fourth.store.items.get(note.id)!.parent_id, id)
  assert.equal(third.store.customActions.size, 0, 'a removed custom action is archived in egdb, and not read back')
  assert.equal(third.store.items.get(done)!.resolved, false, 'a reopened task reads back open')
  // A move is a link removed and another added: read back, the item sits where it went (made when and by whom it was).
  const notes = fourth.store
  const [moved, into] = ['n-moved', 'n-into'].map((nid, i) => {
    notes.insertItem({ id: nid, parent_id: id, via: 'notes', seq: notes.nextSeq(id) + i, type: 'topic', params: { text: nid }, created_by: 'alex', created_at: Date.now(), resolved: true, resolved_at: null, resolved_by: null, cancelled: false, data: null, memo: null, next_check_at: null, last_error: null, runs: 0, added_n: 0, resolved_n: 0 })
    return nid
  })
  const movedAt = Date.now() + 5000
  notes.move(moved, into, 'notes', 'bea', movedAt)
  notes.move(into, null, 'notes', 'bea', movedAt)
  await notes.flush()
  const fifth = await loadStore(again.client, blocking)
  assert.deepEqual([fifth.store.items.get(moved)!.parent_id, fifth.store.items.get(moved)!.via, fifth.store.items.get(moved)!.created_by], [into, 'notes', 'alex'])
  assert.equal(fifth.store.items.get(into)!.parent_id, null)
  assert.deepEqual(fifth.store.pinnedIn(moved), [], 'a move is not read back as a pin')
  again.egdb.stop()
}

// ---- 3. A source an older build wrote: its tasks and notes read as topics, keeping their ids; its links read in by
// adding this build's beside them, once; nothing of the older build's is rewritten ----
{
  const egdb = await startEgdb(join(dir, 'older.egdb'), token)
  const client = new EgdbClient({ url: egdb.url, token })
  const branch = await storeBranch(client)
  const t0 = Date.now()
  const set = (entity: string, values: Record<string, unknown>): Op[] => Object.entries(values).map(([key, value]) => ({ set: { entity, key, value } }) as Op)
  const contains = (source: string, destination: string, sort: number): Op => ({ link: { source, destination, link_type: 'contains', sort: { explicit: sort } } })
  await client.write({ branch, author: 'alex', timestamp: t0, ops: [
    ...set('@root', { type: 'wf.root' }),
    ...set('alex', { type: 'user', username: 'alex' }), contains('@root', 'alex', 0),
    ...set('old-bot', { type: 'bot', name: 'Old', instructions: '', provider: 'claude', model: 'opus' }), contains('@root', 'old-bot', 1),
    ...set('old-task', { type: 'task', title: 'Old task', resolved: false }), contains('@root', 'old-task', 2),
    ...set('old-comment', { type: 'note', text: 'A comment' }), contains('old-task', 'old-comment', 0),
    ...set('old-done', { type: 'task', title: 'Old done', resolved: true }), contains('@root', 'old-done', 3),
    ...set('old-plan', { type: 'note', text: 'Plan' }), contains('@root', 'old-plan', 4),
    ...set('old-step', { type: 'note', text: 'Step', open: true }), contains('old-plan', 'old-step', 0),
    { link: { source: 'old-task', destination: 'old-bot', link_type: 'assigned_to', sort: { explicit: t0 } } },
  ] })
  await client.write({ branch, author: 'bot:old-bot', timestamp: t0 + 1, ops: [
    ...set('old-sub', { type: 'task', title: 'Made by the bot', resolved: false }), contains('old-task', 'old-sub', 1),
  ] })
  const ids = ['@root', 'alex', 'old-bot', 'old-task', 'old-comment', 'old-done', 'old-plan', 'old-step', 'old-sub']
  const olds = async () => {
    const values = await client.values(branch, ids)
    const links = Object.values(await client.links(branch, ids, [['contains', 'outbound'], ['assigned_to', 'outbound']])).flat().flatMap((g) => g.links)
    return [
      ...ids.map((i) => `${i} ${JSON.stringify(Object.entries(values[i] ?? {}).sort())}`),
      ...links.map((l) => `${l.source} ${l.link_type} ${l.destination} ${l.sort} ${l.event?.seq}`).sort(),
    ]
  }
  const before = await olds()
  const count = async (type: string) => Object.values(await client.links(branch, ids, [[type, 'outbound']])).flat().reduce((n, g) => n + g.links.length, 0)

  const known = (type: string) => ITEM_TYPES.some((t) => t.type === type)
  const first = await loadStore(client, blocking, undefined, known)
  await first.store.flush()
  const engine = new Engine({ store: first.store, now: () => Date.now(), dataDir: dir })
  await engine.signIn('alex')
  assert.deepEqual(['old-task', 'old-comment', 'old-done', 'old-plan', 'old-step', 'old-sub'].map((i) => engine.info(i)!.type), Array(6).fill('topic'), 'tasks and notes are topics, by the same ids')
  assert.deepEqual([engine.get('old-task')!.title, engine.isClosed('old-task'), engine.isClosed('old-done')], ['Old task', false, true])
  assert.deepEqual(engine.children('old-task').map((c) => c.id), ['old-comment', 'old-sub'], "a task's items are its comments")
  assert.deepEqual(engine.subtree('old-plan').map((c) => c.id), ['old-plan', 'old-step'], "a note's are its notes")
  assert.equal(engine.info('old-plan')!.params.display, 'notes', 'and a note shows them')
  assert.equal(engine.info('old-step')!.params.open, true, 'its checkbox is its status')
  assert.deepEqual(engine.assignees('old-task'), ['old-bot'])
  assert.deepEqual(['old-task', 'old-sub', 'old-bot', 'old-plan'].map((i) => engine.owner(i)), ['alex', 'alex', 'alex', 'alex'], "owners, by the older build's rule")
  assert.deepEqual(await olds(), before, "the older build's values and links are as they were")
  const added = { comments: await count('comments'), notes: await count('notes'), assignees: await count('assignees'), owns: await count('owns') }
  assert.deepEqual(added, { comments: 2, notes: 1, assignees: 1, owns: 7 })

  // Loaded again, nothing is added twice; what the older build adds since is read in.
  await client.write({ branch, author: 'alex', timestamp: Date.now() + 10, ops: [...set('old-later', { type: 'note', text: 'Later' }), contains('old-task', 'old-later', 2)] })
  const second = await loadStore(client, blocking, undefined, known)
  await second.store.flush()
  const engine2 = new Engine({ store: second.store, now: () => Date.now(), dataDir: dir })
  assert.deepEqual(engine2.children('old-task').map((c) => c.id), ['old-comment', 'old-sub', 'old-later'])
  assert.deepEqual({ comments: await count('comments'), notes: await count('notes'), assignees: await count('assignees'), owns: await count('owns') }, { ...added, comments: 3, owns: added.owns + 1 }, 'the new comment is owned too')
  egdb.stop()
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
  const detail = ((await res.json()) as { result: { children: { params: { text?: string } }[] } }).result
  assert.equal(detail.children[0].params.text, 'A note')
  again.kill('SIGTERM')
  await new Promise((r) => again.on('exit', r))
}

console.log('egdb: ok')
