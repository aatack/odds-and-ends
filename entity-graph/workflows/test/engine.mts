// The engine, the MCP server and the bots' runner, end to end, against an
// in-memory store, and stand-in bots that call the real MCP server over HTTP
// and return structured output.

import assert from 'node:assert/strict'
import { existsSync, mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import { z } from 'zod'
import Ajv from 'ajv'
import { Admin } from '../src/engine/admin.ts'
import { CYCLE_MINUTES, DEFAULT_MODEL, Engine, MAX_SCRIPTS, type BotOutput, type BotUpdate } from '../src/engine/engine.ts'
import { catalog, startMcp } from '../src/engine/mcp.ts'
import { botOutputSchema } from '../src/engine/schema.ts'
import { runScript } from '../src/engine/script.ts'
import { trustFolders, trustTargets } from '../src/engine/trust.ts'
import { ASK, BotRunner, limitedUntil, MAX_BOTS, promptFor, type SpawnBot } from '../src/engine/workers.ts'
import { focusText } from '../src/engine/views.ts'

const MINUTE = 60_000
let clock = 1_000_000

// ---- claude, for what the engine asks of it outside bots' runs ----
let generateArgs: string[] = []
const exec = async (cmd: string, args: string[]) => {
  assert.equal(cmd, 'claude')
  if (args[0] === '-p') {
    generateArgs = args
    return '```js\nreturn `Hello ${context.item.title}`\n```\n'
  }
  return '2.1.309 (Claude Code)\n'
}

const dataDir = mkdtempSync(join(tmpdir(), 'workflows-test-'))
const engine = new Engine({ exec, now: () => clock, dataDir })
// An unsafe action, for the tests alone: the app has none of its own for now.
const posted: unknown[] = []
engine.actions.push({
  name: 'post_somewhere',
  unsafe: true,
  description: 'Posts outside the app (a stand-in).',
  input: { text: z.string() },
  run: async (a) => {
    posted.push(a.text)
    return { posted: a.text }
  },
})
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
// The person the tests act as (Engine.me, when nobody is said: the first person).
await engine.signIn('human')

// ---- bots: each run plays the next behaviour over MCP and returns its output ----
type Call = (name: string, args?: Record<string, unknown>) => Promise<string>
type Run = { call: Call; prompt: string; system: string; schema: Record<string, unknown>; sessionId: string; resume: boolean; model: string; cwd: string }
const behaviours: ((run: Run) => Promise<BotOutput | null>)[] = []
/** Runs no test expected: their failure lands in an item, so they are checked for here too. */
const unexpected: string[] = []
const connect = async (url: string, headers: Record<string, string> = {}) => {
  const client = new Client({ name: 'test', version: '0' })
  await client.connect(new StreamableHTTPClientTransport(new URL(url), { requestInit: { headers } }))
  const call: Call = async (name, args = {}) => {
    const r = (await client.callTool({ name, arguments: args })) as { content: { text: string }[]; isError?: boolean }
    if (r.isError) throw new Error(r.content[0].text)
    return r.content[0].text
  }
  return { client, call }
}
const spawnFake: SpawnBot = ({ prompt, system, schema, mcpUrl, sessionId, resume, model, cwd, onOutput }) => {
  const behaviour = behaviours.shift()
  if (!behaviour) unexpected.push(prompt)
  assert.ok(behaviour, `a bot ran that no test expected:\n${prompt}`)
  const done = (async () => {
    const { client, call } = await connect(mcpUrl)
    const out = await behaviour({ call, prompt, system, schema, sessionId, resume, model, cwd }).catch((e: Error) => {
      if (process.env.DEBUG_BOTS) console.error('a fake run failed:', e.message, '\n--- its prompt ---\n' + prompt)
      throw e
    })
    await client.close()
    onOutput(JSON.stringify({ type: 'assistant', message: { content: [{ type: 'text', text: 'Thinking out loud.' }] } }) + '\n')
    onOutput(JSON.stringify({ type: 'result', total_cost_usd: 0.01, num_turns: 3, structured_output: out ?? undefined }) + '\n')
    return { exitCode: 0 }
  })()
  return { done, kill: () => {} }
}
const runner = new BotRunner(engine, mcp.runUrl, { claude: spawnFake })

/** Until no bot is running, and none is due. */
const settle = async () => {
  for (let i = 0; i < 300; i++) {
    await engine.idle()
    await new Promise((r) => setTimeout(r, 5))
    runner.fill()
    const due = engine.agents().some((a) => a.wakesAt !== null && a.wakesAt <= clock && !runner.pausedUntil(a.provider))
    if (!runner.busy().length && !due) return engine.idle()
  }
  throw new Error(`still busy: ${JSON.stringify(runner.busy())}`)
}
const tick = async () => {
  await engine.tick()
  await settle()
}
const item = (id: string) => engine.get(id)!
/** A run's output: one update for each item given. */
/** A Snooze script, as one would be made in the app: "[id]() 2h" keeps a topic till then (its state on the topic), then hands it back. */
const SNOOZE_SOURCE = `const out = []
const now = Date.parse(context.now)
for (const t of context.topics) {
  const v = (await tools.get_item({ item_id: t.id })).values
  if (!v.sleepUntil) {
    const m = (await mentions(t.id))[0]
    const n = parseFloat(m?.text) || 1
    const unit = { m: 6e4, h: 36e5, d: 864e5 }[(m?.text.match(/[mhd]/) || ['h'])[0]]
    await tools.set_value({ item_id: t.id, key: 'sleepUntil', value: now + n * unit })
    await tools.set_value({ item_id: t.id, key: 'snoozedBy', value: m?.by ?? null })
  } else if (v.sleepUntil <= now) {
    await tools.set_value({ item_id: t.id, key: 'sleepUntil', value: null })
    out.push({ item: t.id, to: [v.snoozedBy] })
  }
}
return { assign: out }`
const out = (...updates: BotUpdate[]): BotOutput => ({ updates })
const blockers = (id: string) => item(id).blockers.map((b) => b.type)
const last = (id: string, type: string) => engine.children(id).filter((e) => e.type === type).at(-1)!

// The types there are; those from before are gone.
assert.deepEqual(engine.itemTypes.map((t) => t.type), ['topic', 'approval', 'script', 'link', 'file', 'search', 'slack_message', 'slack_channel', 'bot', 'user'])
await assert.rejects(engine.addItems((await engine.create('x'))!, [{ type: 'message' as 'note', params: { text: 'x' } }]), /No item type message/)
for (const i of engine.overview().items.filter((i) => i.type === 'topic')) await engine.close(i.id)

// The bots. Threepio does most things; a new bot runs on opus and waits a minute, unless it says otherwise.
await assert.rejects(engine.send((await engine.create('Too soon'))!, 'note', '  '), /Nothing to send/)
const threepio = await engine.saveAgent({ name: 'Threepio', instructions: 'You speak six million languages.', provider: 'claude', model: '' })
assert.deepEqual([engine.agent(threepio)!.model, DEFAULT_MODEL], ['opus', 'opus'])
assert.equal(engine.info(threepio)!.type, 'bot')
const toBot = (id: string, text: string) => engine.send(id, `assign:${threepio}`, text)
for (const i of engine.overview().items.filter((i) => i.type === 'topic')) await engine.close(i.id)

// ---- 1. A new item is its maker's, and their turn ----
const id = (await engine.create('Answer the PR'))!
assert.deepEqual(engine.children(id), [])
assert.deepEqual(item(id).assignees, ['human'])
assert.ok(item(id).withYou)
assert.equal(engine.next(), id)
assert.equal(engine.next([id]), null)

// ---- 2. Handed to a bot with a note; the bot asks a person to do something unsafe, and waits on a script ----
let session = ''
behaviours.push(async ({ call, prompt, system, schema, sessionId, resume, model, cwd }) => {
  session = sessionId
  assert.equal(resume, false)
  assert.equal(model, 'opus')
  assert.match(system, /You are @Threepio/)
  assert.match(system, /six million languages/, "the bot's own instructions are its system prompt")
  assert.ok(cwd.startsWith(join(dataDir, 'bots', 'threepio-')) && existsSync(cwd), 'a folder of its own, under the data folder')
  assert.ok(prompt.startsWith(`${ASK}\n\n---\n\n[current topic ID: ${id}]\n\n[new to you: Answer the PR]`))
  assert.match(prompt, /^human: Reply to the PR, please$/m, "a person's words as they are, after their name")
  assert.ok(JSON.stringify(schema).includes('"enum":["topic"]'), 'a bot may add child tasks')
  assert.ok(JSON.stringify(schema).includes('"enum":["bot"]'), 'and bots')
  assert.equal(JSON.parse(await call('get_item', { item_id: id })).item.id, id)
  await assert.rejects(call('post_somewhere', { text: 'x' }), /not found/i)
  // It has the same tools as anyone connected, so it may act on other items too, as itself.
  const other = (await engine.create('Elsewhere'))!
  await call('add_items', { item_id: other, items: [{ type: 'note', params: { text: 'from a bot' } }] })
  assert.equal(engine.children(other)[0].createdBy, `bot:${threepio}`)
  await engine.close(other)
  return out({
    item: id,
    message: 'Drafted a reply. Post it?',
    items: [{ type: 'approval', params: { action: 'post_somewhere', inputs: { text: 'Thanks!' } } }],
  })
})
await toBot(id, 'Reply to the PR, please')
assert.deepEqual(engine.children(id).map((e) => [e.type, e.createdBy]), [['topic', 'human']], 'assignment is a link, not an item')
assert.deepEqual(item(id).assignees, [threepio], 'handing it to the bot took it off the sender')
assert.deepEqual(engine.assignedTo(threepio), [id], "the bot's items: its assigned_to links, followed backwards")
await settle()
assert.deepEqual(blockers(id), ['approval'])
assert.deepEqual(item(id).assignees, ['human'], 'a run that leaves the item waiting on a person hands it back')
assert.ok(item(id).withYou, 'an approval is a person\'s turn')
assert.equal(last(id, 'topic').params.text, 'Drafted a reply. Post it?')
assert.equal(last(id, 'topic').createdBy, `bot:${threepio}`)
assert.deepEqual(engine.agent(threepio)!.lastRun!.items, [id], 'the bot keeps its last run as a value')
assert.ok(engine.agent(threepio)!.lastRun!.endedAt)

// ---- 3. The person changes it and approves; the unsafe action runs, as theirs ----
const approval = item(id).blockers[0]
await engine.approve(approval.id, { text: 'Thanks a lot!' })
assert.deepEqual(posted, ['Thanks a lot!'])
assert.deepEqual(engine.info(approval.id)!.data!.changed, ['text'])
assert.equal(engine.info(approval.id)!.message, 'Ran post_somewhere (changed text)')
for (const a of engine.actions.filter((a) => a.unsafe))
  await assert.rejects(engine.callAction(a.name, { text: 'x' }, { engine, author: 'bot', actor: 'script', itemId: id, runId: null }), /unsafe/)
await assert.rejects(engine.addItems(id, [{ type: 'approval', params: { action: 'list_items', inputs: {} } }]), /not an unsafe action/)

// ---- 4. A bot that adds a wait keeps the item; the wait resolving is news, heard of in time ----
behaviours.push(async ({ prompt, sessionId, resume, call }) => {
  assert.equal(resume, true)
  assert.equal(sessionId, session, 'one session for the bot, across its runs')
  assert.match(prompt, /\[topic: Answer the PR\]/)
  assert.doesNotMatch(prompt, /Reply to the PR, please/, 'what the session has seen is not sent again')
  // It makes a script that checks CI, and hands it the task: the script hands it back when CI passes.
  const { ids } = JSON.parse(await call('add_items', { items: [{ type: 'script', params: { name: 'CI', cycle_minutes: 1 / 60, source: `if (!context.state) return { state: 1 }
return { messages: [{ item: context.topics[0].id, text: 'CI passed' }], assign: [{ item: context.topics[0].id, to: ['Threepio'] }] }` } }] }))
  return out({ item: id, assign: [ids[0]] })
})
await toBot(id, 'Wait for CI')
await settle()
const ci = engine.holder('CI')!
assert.deepEqual(item(id).assignees, [ci], 'the script has the task now, and the bot has let go of it')
assert.deepEqual(blockers(id), [], 'a script never blocks the task it sits in')
assert.equal(item(id).withYou, false)
behaviours.push(async ({ prompt }) => {
  assert.match(prompt, /CI passed/)
  return out({ item: id, message: 'All green.', assign: ['human'] })
})
clock += 1000
await tick()
clock += 1000
await tick()
await settle()
assert.deepEqual(item(id).assignees, ['human'])
await engine.close(id)
assert.ok(item(id).closed)
assert.ok(!engine.info(ci)!.resolved, 'a script lasts: neither handing its topics on nor its task closing ends it')

// ---- 5. Child tasks: a bot splits work off and gives it to itself; the child blocks nothing ----
const parent = (await engine.create('Parent'))!
behaviours.push(async () =>
  out({
    item: parent,
    // A task's first items by their old name, `items`, are taken too.
    items: [{ type: 'task', params: { title: 'Child', events: [{ type: 'note', params: { text: 'Do the child' } }], assignees: ['Threepio'] } }],
  }),
)
behaviours.push(async ({ prompt }) => {
  assert.match(prompt, /\[new to you: Child\]/)
  assert.match(prompt, /Do the child/)
  const kid = engine.children(parent).find((e) => e.type === 'topic')!.id
  return out({ item: kid, message: 'Child done.', close: true })
})
await toBot(parent, 'Split it')
await settle()
assert.deepEqual(blockers(parent), [], 'a task in another is no dependency of it')
assert.ok(item(parent).withYou, 'so the parent went back to its owner')
clock += CYCLE_MINUTES * MINUTE
await settle()
const kid = engine.children(parent).find((e) => e.type === 'topic')!.id
assert.ok(item(kid).closed)
// Closing a task leaves the tasks in it as they are.
const [loose] = await engine.addItems(parent, [{ type: 'task', params: { title: 'Still open' } }])
await engine.close(parent)
assert.ok(item(parent).closed && !item(loose).closed)
await engine.close(loose)
// A bot can also add an item that sits in no item: it blocks nothing, and it is nobody's unless the bot says.
{
  const host = (await engine.create('Host'))!
  behaviours.push(async () => out({ item: host, items: [{ type: 'task', params: { title: 'Unrelated', detached: true } }] }))
  await toBot(host, 'x')
  await settle()
  const loose = engine.overview().items.find((i) => i.title === 'Unrelated')!
  assert.equal(loose.parentId, null)
  assert.deepEqual(loose.assignees, [], 'a bot decides who has what it makes')
  assert.ok(item(host).withYou)
  await engine.close(host)
  await engine.close(loose.id)
}

// A bot can say something in a task it doesn't have: by its update, or by add_items. Who has that task is left alone.
{
  const host = (await engine.create('Host'))!
  const other = (await engine.create('Other'))!
  behaviours.push(async ({ call }) => {
    await call('add_items', { item_id: other, items: [{ type: 'note', params: { text: 'By the tool.' } }] })
    return out({ item: host, message: 'Told them.' }, { item: other, message: 'By the update.' })
  })
  await toBot(host, 'Tell the other task')
  await settle()
  assert.deepEqual(
    engine.children(other).filter((e) => e.type === 'topic').map((e) => [e.params.text, e.createdBy]),
    [['By the tool.', `bot:${threepio}`], ['By the update.', `bot:${threepio}`]],
  )
  assert.ok(item(other).withYou, 'a task the bot was not given stays with whoever had it')
  await engine.close(host)
  await engine.close(other)
}

// What a person says while a run goes on isn't lost: the run's update can't hand that task away, and the bot wakes for it.
{
  const busy = (await engine.create('Busy'))!
  behaviours.push(async () => {
    await engine.send(busy, 'note', 'Also do the other thing')
    return out({ item: busy, message: 'Done the first thing.', assign: [engine.me()] })
  })
  behaviours.push(async ({ prompt }) => {
    assert.deepEqual(engine.assignees(busy).sort(), [engine.me(), threepio].sort(), 'it kept the task, beside who it gave it to')
    assert.match(prompt, /Also do the other thing/)
    assert.doesNotMatch(prompt, /Done the first thing/, 'what it said itself is not news to it')
    return out({ item: busy, message: 'Done the other thing.', assign: [engine.me()] })
  })
  await toBot(busy, 'Do the first thing')
  await settle()
  assert.equal(behaviours.length, 0, 'what a person said while it ran is due at once: the run never saw it')
  assert.deepEqual(engine.assignees(busy), [engine.me()], 'with nothing new, its update hands the task on')
  await engine.close(busy)
}

// Limits: at most `bots.max_running` bots run at once, and `scripts.max_running` scripts; the rest wait their turn.
{
  engine.settings.set('bots.max_running', 1)
  const other = await engine.saveAgent({ name: 'Other', instructions: '', provider: 'claude', model: '' })
  const [t1, t2] = [(await engine.create('One'))!, (await engine.create('Two'))!]
  const seen: number[] = []
  for (const _ of [1, 2])
    behaviours.push(async ({ prompt }) => {
      seen.push(runner.busy().length)
      const t = /One/.test(prompt) ? t1 : t2
      return out({ item: t, assign: [engine.me()] })
    })
  await engine.send(t1, `assign:${threepio}`, 'x')
  await engine.send(t2, `assign:${other}`, 'y')
  await settle()
  assert.deepEqual(seen, [1, 1], 'one at a time')
  engine.settings.set('bots.max_running', MAX_BOTS)
  await engine.deleteAgent(other)
  // Handing a task to a bot cancelled since: the rest of the update lands, and the task goes back as if nobody was named.
  behaviours.push(async () => out({ item: t1, message: 'Back to you', assign: [other] }))
  await engine.send(t1, `assign:${threepio}`, 'z')
  await settle()
  const said = engine.children(t1).map((e) => String(e.params.text ?? ''))
  assert.ok(said.includes('Back to you'), 'the message still lands')
  assert.ok(said.some((t) => /can't have tasks now/.test(t)), 'it says the bot is gone')
  assert.ok(!said.some((t) => /failed/.test(t)), 'nothing failed')
  assert.deepEqual(engine.assignees(t1), [engine.me()], 'the task goes back to its owner')
  await assert.rejects(engine.assign(t1, other, true), /is cancelled, so it can't have tasks now/)
  engine.settings.set('scripts.max_running', 1)
  const [s1, s2] = await engine.addItems(null, [
    { type: 'script', params: { source: 'return null' } },
    { type: 'script', params: { source: 'return null' } },
  ])
  await engine.assign(t1, s1, true)
  await engine.assign(t2, s2, true)
  clock += 1
  await engine.checkDue()
  await engine.idle()
  assert.deepEqual([engine.info(s1)!.runs, engine.info(s2)!.runs], [1, 0], 'one script at a time; the other waits for the next turn')
  await engine.checkDue()
  await engine.idle()
  assert.equal(engine.info(s2)!.runs, 1)
  engine.settings.set('scripts.max_running', MAX_SCRIPTS)
  await engine.close(t1)
  await engine.close(t2)
}

// A closed task can be reopened: who has it is unchanged; it is open again, and says who reopened it.
{
  const t = (await engine.create('Reopen me'))!
  await engine.close(t)
  assert.ok(item(t).closed)
  await engine.reopen(t)
  assert.ok(!item(t).closed && item(t).withYou)
  assert.match(String(last(t, 'topic').params.text), /^Reopened by /)
  assert.equal((await engine.inspect(t)).values.open, true)
  await engine.close(t)
}

// Pins: what is useful in actioning an item is pinned to it. They show beside it, not in its chat, and stay where they sit.
{
  const from = (await engine.create('Discussion'))!
  await engine.send(from, 'note', 'We should fix the export')
  const said = last(from, 'topic').id
  const [made] = await engine.addItems(null, [{ type: 'task', params: { title: 'Fix the export' } }])
  await engine.callAction('pin', { item_id: made, item_ids: [said] }, { engine, author: 'bot', actor: `bot:${threepio}`, itemId: made, runId: null })
  // link_items, the old name, pins too.
  await engine.callAction('link_items', { item_id: from, item_ids: [made] }, { engine, author: 'bot', actor: `bot:${threepio}`, itemId: from, runId: null })
  assert.deepEqual(engine.info(made)!.pins, [said], 'the new task has what it came from pinned')
  assert.deepEqual(engine.info(made)!.pinnedIn, [from], 'and is pinned where it came from')
  assert.ok(!engine.detail(made)!.children.some((e) => e.id === said), 'a pin is not in the chat')
  assert.equal(engine.info(said)!.parentId, from, 'what is pinned stays where it sits')
  assert.deepEqual(engine.itemInfoDetail(made)!.pins.map((p) => p.id), [said])
  assert.deepEqual(engine.itemInfoDetail(said)!.pinnedIn.map((p) => p.id), [made])
  // Pinning twice, or to itself, does nothing; unpinning takes it off.
  await engine.pin(made, [said, made])
  assert.deepEqual(engine.info(made)!.pins, [said])
  await engine.pin(made, [said], false)
  assert.deepEqual([engine.info(made)!.pins, engine.info(said)!.pinnedIn], [[], []])
  // A subtask is pinned to the task it is made in; anything added with `pin` too, and a link may sit in no task.
  const [sub, pr] = await engine.addItems(
    from,
    [{ type: 'task', params: { title: 'Sub' } }, { type: 'link', params: { url: 'https://github.com/o/r/pull/1' }, pin: true }],
    `bot:${threepio}`,
    'bot',
  )
  const [loose] = await engine.addItems(from, [{ type: 'note', params: { text: 'not pinned' } }])
  assert.deepEqual(engine.info(from)!.pins, [made, sub, pr])
  assert.ok(!engine.info(from)!.pins.includes(loose))
  const [detachedSub] = await engine.addItems(from, [{ type: 'task', params: { title: 'Elsewhere', detached: true }, pin: true }])
  assert.equal(engine.info(detachedSub)!.parentId, null)
  assert.deepEqual(engine.info(detachedSub)!.pinnedIn, [from], 'a detached task added with pin is pinned where it was added')
  await engine.addItems(null, [{ type: 'link', params: { url: 'https://example.com/preview' } }])
  await engine.close(from)
  await engine.close(made)
}

// Notes: a set of notes is a topic in no item that shows its notes, pinned where it was made; its tree is its notes links.
{
  const t = (await engine.create('Notes here'))!
  const root = engine.newNotes('Plan', t, 'alex')
  assert.equal(engine.info(root)!.parentId, null, 'a set of notes sits in no item')
  assert.deepEqual(engine.info(t)!.pins, [root], 'and is pinned where it was made')
  assert.ok(!engine.detail(t)!.children.some((c) => c.id === root), 'not in its chat')
  const [a, b] = await engine.addItems(root, [{ type: 'note', params: { text: 'A' } }, { type: 'note', params: { text: 'B', section: true } }], 'alex', 'human', 'notes')
  const [a1] = await engine.addItems(a, [{ type: 'note', params: { text: 'A1', open: true } }], 'alex', 'human', 'notes')
  assert.deepEqual(engine.subtree(root).map((i) => [i.params.text, i.parentId]), [['Plan', null], ['A', root], ['A1', a], ['B', root]])
  assert.deepEqual([engine.info(b)!.params.section, engine.info(a1)!.params.open], [true, true], 'a heading and a checkbox are kept as they are added')
  // Moving: to the end of where it goes; never inside itself; out of everything with null.
  await engine.moveItem(a1, b, 'alex')
  assert.deepEqual(engine.subtree(root).map((i) => i.params.text), ['Plan', 'A', 'B', 'A1'])
  await assert.rejects(engine.moveItem(b, a1, 'alex'), /inside itself/)
  await engine.moveItem(a, null, 'alex')
  assert.deepEqual(engine.subtree(root).map((i) => i.params.text), ['Plan', 'B', 'A1'])
  assert.equal(engine.info(a)!.parentId, null)
  await engine.setValue(a1, 'open', false, 'alex')
  assert.equal(engine.info(a1)!.params.open, false)
  // Anything else in a note is a leaf of its tree.
  const [inner] = await engine.addItems(b, [{ type: 'task', params: { title: 'Do it' } }], 'alex', 'human', 'notes')
  await engine.addItems(inner, [{ type: 'note', params: { text: 'in the task' } }], 'alex')
  assert.deepEqual(engine.subtree(root).map((i) => i.id), [root, b, a1, inner])
  // A bot reads it at its focus (get_item): its tree under its pill, a note at focus, anything else as its pill.
  const ctx = { engine, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }
  const got = (await engine.callAction('get_item', { item_id: root }, ctx)) as { focus: string; pinnedIn: { id: string }[] }
  assert.equal(got.focus, `[topic] Plan (${root})\n- # B (${b})\n  - [x] A1 (${a1})\n  - [ ] Do it (${inner})`)
  assert.deepEqual(got.pinnedIn.map((p) => p.id), [t])
  await engine.addItems(a1, Array.from({ length: 5 }, (_, i) => ({ type: 'note' as const, params: { text: `n${i}` } })), 'alex', 'human', 'notes')
  // Cut off at the limit, with the ids to ask for the rest.
  assert.match(focusText(engine, root, undefined, 4)!, new RegExp(`\\.\\.\\. 3 more under ${a1}: get_item it[^]*\\.\\.\\. 1 more under ${b}`))
  assert.equal(engine.view(inner + 'x', 'focus'), null)
  // The smaller sizes, and a task at its focus: the cards of what is in it.
  assert.deepEqual(await engine.callAction('get_item', { item_id: inner, size: 'pill' }, ctx), { text: `[topic] Do it (${inner})` })
  assert.equal(engine.view(inner, 'focus'), `[topic] Do it (${inner})\n[alex: in the task]`)
  await engine.close(inner)
  await engine.close(t)
}

// set_value: any param, checked; not how it stands, nor what the app keeps for itself.
{
  const t = (await engine.create('Values'))!
  await engine.setValue(t, 'text', 'Values, renamed')
  assert.equal(item(t).title, 'Values, renamed')
  await assert.rejects(engine.setValue(t, 'resolved', true), /can't be set/)
  await engine.setValue(t, 'anything', { at: 1 })
  assert.deepEqual(engine.info(t)!.params.anything, { at: 1 }, 'duck typed: any key, any value')
  await assert.rejects(engine.setValue(threepio, 'session_id', 'x'), /can't be set/)
  await engine.close(t)
}

// The search page's sections: built in by default; a query and a function, set by a bot, kept on the search item.
{
  assert.deepEqual(engine.searchSections().map((s) => s.kind), ['builtin', 'builtin', 'builtin', 'builtin', 'builtin'])
  const t = (await engine.create('Searchable'))!
  const bot = { engine, author: 'bot' as const, actor: `bot:${threepio}`, itemId: null, runId: null }
  await engine.callAction('set_search_sections', { sections: [
    { kind: 'builtin', id: 'needsYou' },
    { kind: 'query', label: 'Searchable tasks', filter: { and: [{ eq: { key: 'type', value: 'task' } }, { contains: { key: 'title', value: 'Search' } }] } },
    { kind: 'function', label: 'Echo', source: 'return [{ id: "x", type: "note", left: "You typed **" + context.text + "**" }]' },
  ] }, bot)
  const [, q, f] = engine.searchSections()
  assert.deepEqual((await engine.runSearchSection(q, '')).map((r) => r.id), [t])
  assert.deepEqual(await engine.runSearchSection(f, 'hi'), [{ id: 'x', type: 'note', left: 'You typed **hi**', right: '' }])
  const mine = await engine.runSearchSection({ kind: 'query', label: 'Mine', filter: { and: [{ eq: { key: 'type', value: 'task' } }, { eq: { key: 'assignees', value: engine.me() } }, { eq: { key: 'title', value: 'Searchable' } }] } }, '')
  assert.deepEqual(mine.map((r) => r.id), [t], 'who has it is a key, matched by any one')
  await assert.rejects(engine.setSearchSections([{ kind: 'builtin', id: 'nope' as 'viewed' }]), /No built-in section/)
  await assert.rejects(engine.setSearchSections([{ kind: 'query', label: 'x', filter: { bogus: 1 } as never }]), /Not a filter/)
  assert.equal(engine.store.ofType('search').length, 1, 'one search item, changed in place')
  await engine.setSearchSections(null)
  assert.equal(engine.searchSections().length, 5)
  await engine.close(t)
}

// Settings a bot may read and change: the counts and the integrations' setup values that are not secret; nothing else.
{
  const bot = { engine, author: 'bot' as const, actor: `bot:${threepio}`, itemId: null, runId: null }
  const listed = (await engine.callAction('settings', {}, bot)) as { key: string; value: unknown }[]
  assert.deepEqual(listed.map((s) => s.key), ['bots.max_running', 'scripts.max_running', 'bots.idle_minutes', 'integration.github.default_repo'])
  await engine.callAction('set_setting', { key: 'bots.max_running', value: 3 }, bot)
  assert.equal(engine.settings.get('bots.max_running', 0), 3)
  await engine.callAction('set_setting', { key: 'bots.max_running', value: null }, bot)
  assert.equal(engine.settings.get('bots.max_running', 0), MAX_BOTS, 'null puts the default back')
  await assert.rejects(engine.callAction('set_setting', { key: 'scripts.max_running', value: 0 }, bot), /at least 1/)
  await assert.rejects(engine.callAction('set_setting', { key: 'mcp.port', value: 1 }, bot), /not a setting bots may change/)
  await assert.rejects(engine.callAction('set_setting', { key: 'phone.tailnet', value: true }, bot), /not a setting bots may change/)
  await engine.callAction('set_setting', { key: 'integration.github.default_repo', value: 'a/b' }, bot)
  assert.equal(engine.settings.integrationValue('github', 'default_repo'), 'a/b')
  await engine.callAction('set_setting', { key: 'integration.github.default_repo', value: null }, bot)
  assert.equal(engine.settings.integrationValue('github', 'default_repo'), null)
}

// `/` in the chat input: any tool, as the person; what it answered is a note. An unsafe one is theirs to run.
{
  const t = (await engine.create('Tools'))!
  await engine.runTool(t, 'list_people', {})
  assert.match(String(last(t, 'topic').params.text), /^Ran `list_people`:\n\n```json\n/)
  await engine.runTool(t, 'post_somewhere', { text: 'by hand' })
  assert.equal(posted.pop(), 'by hand')
  await assert.rejects(engine.runTool(t, 'nope', {}), /No action nope/)
  assert.match(String(last(t, 'topic').params.text), /it failed: No action nope/)
  await engine.close(t)
}

// A handoff: a run done for now leaves one; the next run starts a fresh session, told it once, with each task in full.
{
  const t = (await engine.create('Handoff'))!
  let session = ''
  behaviours.push(async ({ sessionId }) => {
    session = sessionId
    return { updates: [{ item: t, message: 'Done for now.', assign: [threepio] }], handoff: 'Was halfway through Handoff: next, the tests.' }
  })
  await toBot(t, 'start')
  await settle()
  assert.equal(engine.agent(threepio)!.sessionId, null, 'its session is let go')
  behaviours.push(async ({ prompt, sessionId, resume }) => {
    assert.notEqual(sessionId, session)
    assert.equal(resume, false)
    assert.match(prompt, /^\[a fresh session: your last one left you this handoff\]\n\nWas halfway through Handoff/)
    assert.match(prompt, /\[new to you: Handoff\][\s\S]*start/, 'the task in full')
    return out({ item: t, assign: [engine.me()] })
  })
  await engine.send(t, `assign:${threepio}`, 'carry on')
  await settle()
  assert.equal(behaviours.length, 0)
  assert.equal(engine.agent(threepio)!.sessionId !== null, true)
  await engine.close(t)
}

// Idle: woken more than half an hour after its last run ended, a bot starts a fresh session; sooner, it resumes.
{
  const t = (await engine.create('Idle'))!
  let session = ''
  behaviours.push(async ({ sessionId }) => {
    session = sessionId
    return out({ item: t, assign: [engine.me()] })
  })
  await toBot(t, 'first')
  await settle()
  clock += 10 * MINUTE
  behaviours.push(async ({ sessionId, resume }) => {
    assert.equal(sessionId, session, 'soon after: the same session')
    assert.equal(resume, true)
    return out({ item: t, assign: [engine.me()] })
  })
  await toBot(t, 'still there?')
  await settle()
  clock += 31 * MINUTE
  behaviours.push(async ({ prompt, sessionId, resume }) => {
    assert.notEqual(sessionId, session, 'idle: a fresh one')
    assert.equal(resume, false)
    assert.match(prompt, /\[new to you: Idle\][\s\S]*first/, 'its topics in full again')
    return out({ item: t, assign: [engine.me()] })
  })
  await toBot(t, 'back now')
  await settle()
  assert.equal(behaviours.length, 0)
  await engine.close(t)
}

// A timer: a script runs every cycle while it holds a topic (here, the one it sits in); it makes a task for a bot, and a
// new one only once the last is closed.
{
  const host = (await engine.create('Timer host'))!
  const source = `if (context.state?.last && !(await tools.get_item({ item_id: context.state.last })).item.closed) return null
const { ids } = await tools.add_items({ items: [{ type: 'task', params: { title: 'Weekly review', assignees: ['human'] } }] })
return { state: { last: ids[0] } }`
  const [timer] = await engine.addItems(host, [{ type: 'script', params: { description: 'Weekly', cycle_minutes: 10080, source } }])
  await engine.assign(host, timer, true)
  // Given the topic, it runs once, after its debounce; then every cycle.
  clock += MINUTE
  await engine.checkDue()
  await engine.idle()
  for (const r of engine.overview().items.filter((i) => i.title === 'Weekly review')) await engine.close(r.id)
  const reviews = () => engine.overview().items.filter((i) => i.title === 'Weekly review' && !i.closed)
  await engine.checkDue()
  await engine.idle()
  assert.equal(reviews().length, 0, 'not before its first cycle')
  clock += 10080 * MINUTE
  await engine.checkDue()
  await engine.idle()
  assert.equal(reviews().length, 1, String(engine.info(timer)!.lastError))
  clock += 10080 * MINUTE
  await engine.checkDue()
  await engine.idle()
  assert.equal(reviews().length, 1, 'the last one is still open')
  await engine.close(reviews()[0].id)
  clock += 10080 * MINUTE
  await engine.checkDue()
  await engine.idle()
  assert.equal(reviews().length, 1, 'closed, so a new one')
  for (const r of reviews()) await engine.close(r.id)
  await engine.close(host)
}

// A usage limit: the run is put back as if it never began (the task stays, what was new stays new), and the provider's
// bots wait until it resets.
{
  assert.equal(limitedUntil('Claude AI usage limit reached|1800000000', 0), 1_800_000_000_000)
  assert.equal(limitedUntil('API Error: 529 overloaded', 1000), 1000 + 2 * MINUTE)
  assert.equal(limitedUntil('some other failure', 1000), null)
  const t = (await engine.create('Limited'))!
  behaviours.push(async () => {
    throw new Error('Claude AI usage limit reached|' + Math.floor((clock + 30 * MINUTE) / 1000))
  })
  await toBot(t, 'please')
  await settle()
  assert.deepEqual(item(t).assignees, [threepio], 'it keeps the task')
  assert.ok(!engine.children(t).some((e) => /failed/.test(String(e.params.text))), 'and says nothing of it')
  assert.equal(runner.pausedUntil('claude') !== null, true)
  clock += 5 * MINUTE
  await settle()
  assert.equal(behaviours.length, 0, 'nothing runs while paused')
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /please/, 'what was new before is new still')
    return out({ item: t, assign: [engine.me()] })
  })
  clock += 30 * MINUTE
  await settle()
  assert.equal(behaviours.length, 0)
  await engine.close(t)
}

// The 30-minute rule: an item a bot ran on and still holds is due again only its cycle later, unless someone else
// assigns it to the bot again since.
{
  const t = (await engine.create('Held'))!
  behaviours.push(async () => out({ item: t, message: 'Keeping it.', assign: [threepio] }))
  await toBot(t, 'keep it')
  await settle()
  const ran = engine.agent(threepio)!.lastRun!.id
  assert.deepEqual(item(t).assignees, [threepio], 'it gave the topic to itself')
  assert.equal(engine.agent(threepio)!.wakesAt, clock + CYCLE_MINUTES * MINUTE, 'due again a cycle after it last ran on it')
  clock += (CYCLE_MINUTES - 1) * MINUTE
  await settle()
  assert.equal(engine.agent(threepio)!.lastRun!.id, ran, 'not before')
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /\[nothing new\]/)
    return out({ item: t, message: 'Still keeping it.', assign: [threepio] })
  })
  clock += MINUTE
  await settle()
  assert.equal(behaviours.length, 0, 'then it runs on it again')
  // Assigned to it again by a person: due at once.
  behaviours.push(async () => out({ item: t, assign: [engine.me()] }))
  await engine.assign(t, threepio, false)
  await engine.assign(t, threepio, true)
  await settle()
  assert.equal(behaviours.length, 0)
  assert.deepEqual(item(t).assignees, [engine.me()])
  await engine.close(t)
}

// One answer to who can hold a task: a person, a bot or an open script, by any name for it.
{
  const t = (await engine.create('Held by a script'))!
  const [s] = await engine.addItems(null, [{ type: 'script', params: { source: 'return null' } }])
  await engine.assign(t, s, true)
  await engine.assign(t, engine.me(), false)
  assert.deepEqual([engine.holder('@Threepio'), engine.holder(threepio), engine.holder(s), engine.holder('nobody')], [threepio, threepio, s, null])
  const ctx = { engine, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }
  const onBots = (await engine.callAction('items_waiting_on', { on: 'bot' }, ctx)) as { id: string }[]
  assert.ok(!onBots.some((i) => i.id === t), 'a topic a script holds is not waiting on a bot')
  const onScript = (await engine.callAction('items_waiting_on', { on: s }, ctx)) as { id: string }[]
  assert.deepEqual(onScript.map((i) => i.id), [t])
  await engine.close(t)
}

// A script on its own, given topics as a bot is: "[snooze]() 2h" hands the topic to it; it keeps it till then (its state
// on the topic), and hands it back. It runs when given a topic, and on its cycle; not on news.
{
  const [snooze] = await engine.addItems(null, [{ type: 'script', params: { name: 'Snooze', source: SNOOZE_SOURCE, cycle_minutes: 5 } }])
  assert.equal(engine.holder('@snooze'), snooze, 'by its name')
  const t = (await engine.create('Snooze me'))!
  await engine.send(t, 'note', `[${snooze}]() 2h`)
  assert.deepEqual(item(t).assignees, [snooze], 'handed to it, and off the sender')
  await engine.checkDue()
  await engine.idle()
  assert.equal(engine.info(snooze)!.runs, 1, 'given a topic, it runs on it at once')
  assert.equal(engine.info(snooze)!.lastError, null)
  assert.equal(engine.info(t)!.params.sleepUntil, clock + 2 * 60 * MINUTE, 'its state is on the topic')
  await engine.send(t, 'note', 'news in the topic')
  clock += MINUTE
  await engine.checkDue()
  await engine.idle()
  assert.equal(engine.info(snooze)!.runs, 1, 'news in a topic it holds is not something happening to it')
  clock += 2 * 60 * MINUTE
  await engine.checkDue()
  await engine.idle()
  assert.deepEqual(item(t).assignees, [engine.me()], 'and back after two hours')
  assert.equal(engine.info(t)!.params.sleepUntil, undefined)
  await engine.close(t)
  await engine.cancel(snooze)
  assert.ok(engine.info(snooze)!.resolved)
  // A cancelled script can be reopened, as it was.
  await engine.reopen(snooze)
  assert.equal(engine.info(snooze)!.resolved, false)
  assert.equal(engine.info(snooze)!.cancelled, false)
  assert.ok(engine.info(snooze)!.params.source)
  await engine.cancel(snooze)
}

// Processes and widgets are gone: none can be added.
for (const type of ['process', 'widget'])
  await assert.rejects(engine.addItems((await engine.create('No ' + type))!, [{ type: type as 'topic', params: {} }]), new RegExp(`No item type ${type}`))

// ---- 6. Output that can't be used, or an update that can't be applied, is said, and the item goes back ----
const bad = (await engine.create('Bad'))!
behaviours.push(async () => out({ item: bad, items: [{ type: 'user', params: { username: 'Not Valid' } }] }))
await toBot(bad, 'x')
await settle()
assert.match(String(last(bad, 'topic').params.text), /^@Threepio failed: its update could not be applied: Bad params for a user item/)
assert.equal(last(bad, 'topic').createdBy, 'system')
assert.ok(item(bad).withYou)
behaviours.push(async () => ({ updates: 'nonsense' }) as unknown as BotOutput)
await toBot(bad, 'y')
await settle()
assert.match(String(last(bad, 'topic').params.text), /The bot's output was not usable/)
await engine.close(bad)

// ---- 7. Scripts: bots without a model. Given topics, they run on one at a time (state kept), say things, hand topics on;
// they never end on their own ----
{
  const a = (await engine.create('Watched A'))!
  const b = (await engine.create('Watched B'))!
  const [watch] = await engine.addItems(null, [{ type: 'script', params: { name: 'Watcher', cycle_minutes: 1, source: `
    const items = await tools.list_items({})
    console.log('run', context.runs, 'items', items.length)
    const n = (context.state ?? 0) + 1
    if (n < 3) return { state: n, messages: context.topics.map((t) => ({ item: t.id, text: 'Still waiting on ' + t.title })) }
    return { state: n, assign: context.topics.map((t) => ({ item: t.id, to: ['human'] })), done: true }` } }])
  await engine.assign(a, watch, true)
  await engine.assign(b, watch, true)
  assert.deepEqual(blockers(a), [], 'it blocks nothing')
  clock += 1
  await tick()
  assert.equal(last(a, 'topic').params.text, 'Still waiting on Watched A', 'given topics, it runs on the one it has held longest')
  assert.deepEqual(engine.children(b), [], 'and only that one')
  assert.equal((engine.info(watch)!.params.last_seen as Record<string, number>)[a], clock, 'when it last ran on each is kept, for a restart')
  await tick()
  assert.equal(last(b, 'topic').params.text, 'Still waiting on Watched B', 'then the next')
  await tick()
  assert.equal(engine.info(watch)!.runs, 2, 'neither is due again before its cycle, and its own messages are not news')
  clock += MINUTE
  await tick()
  await tick()
  assert.deepEqual([item(a).assignees, item(b).assignees], [['human'], ['human']], 'it hands its topics on')
  assert.ok(!engine.info(watch)!.resolved, 'and lasts (an old `done: true` is ignored)')
  // A run that throws says so in its chat, once till the error changes, and carries on.
  const [bad2] = await engine.addItems(null, [{ type: 'script', params: { name: 'Bad', source: 'throw new Error("nope " + Math.min(context.runs, 1))', cycle_minutes: 1 } }])
  await engine.assign(a, bad2, true)
  clock += 1
  await tick()
  const said = () => engine.children(bad2).filter((e) => e.type === 'topic').map((e) => String(e.params.text))
  assert.ok(!engine.info(bad2)!.resolved, 'a failing run does not end it')
  assert.equal(said().length, 1)
  assert.match(said()[0], /^Bad failed: The script failed: nope 0$/)
  clock += MINUTE
  await tick()
  clock += MINUTE
  await tick()
  assert.equal(engine.info(bad2)!.runs, 3, 'it runs again on its cycle')
  assert.equal(said().length, 2, 'a new error is said; the same one again is not')
  await engine.cancel(bad2)
  assert.ok(engine.info(bad2)!.resolved, 'cancelling is the only way it goes')
  // One that holds nothing runs only when pressed.
  const [manual] = await engine.addItems(null, [{ type: 'script', params: { source: 'return null' } }])
  clock += 60 * MINUTE
  await tick()
  assert.equal(engine.info(manual)!.runs, 0, 'it waits to be run')
  await engine.runNow(manual)
  await engine.runNow(manual)
  assert.equal(engine.info(manual)!.runs, 2)
  assert.ok(!engine.info(manual)!.resolved)
  // Unsafe tools: a person's press has them; a run on its own never, whoever wrote it.
  const unsafeSource = `await tools.post_somewhere({ text: 'pressed' }); return null`
  const [press] = await engine.addItems(null, [{ type: 'script', params: { source: unsafeSource } }], 'bot', 'bot')
  await engine.runNow(press, 'human')
  assert.equal(posted.pop(), 'pressed', 'pressed by a person, it may act outside')
  await assert.rejects(engine.runNow(press, 'bot'), /unsafe/, 'a press that fails says why')
  const [timed] = await engine.addItems(null, [{ type: 'script', params: { source: unsafeSource } }], 'human')
  await engine.assign(b, timed, true)
  clock += 1
  await tick()
  assert.match(String(engine.children(timed).find((e) => e.type === 'topic')?.params.text), /unsafe/)
  // run_script: once, now, with the tools; an agent gets no unsafe ones.
  const ran = (await engine.callAction('run_script', { source: 'return (await tools.list_items({})).length + context.n', context: { n: 1 } }, { engine, author: 'bot', actor: 'bot', itemId: null, runId: null })) as { result: number }
  assert.equal(typeof ran.result, 'number')
  await assert.rejects(engine.callAction('run_script', { source: unsafeSource }, { engine, author: 'bot', actor: 'bot', itemId: null, runId: null }), /unsafe/)
  await engine.close(a)
  await engine.close(b)
}

// ---- 8. Deleting a bot while it runs: what it returns doesn't land, and its items go back ----
{
  const temp = await engine.saveAgent({ name: 'Temp', instructions: '', provider: 'claude', model: '' })
  let release = () => {}
  const held = new Promise<void>((r) => (release = r))
  const taken = (await engine.create('Take me'))!
  behaviours.push(async () => {
    await held
    return out({ item: taken, message: 'late', close: true })
  })
  await engine.send(taken, 'note', `[${temp}]() take this`)
  assert.deepEqual(item(taken).assignees, [temp], 'an @ in a note hands the item to the bot')
  while (!runner.busy().includes(temp)) await new Promise((r) => setTimeout(r, 5))
  assert.equal(item(taken).bot, temp)
  await engine.cancel(temp)
  release()
  await settle()
  assert.ok(engine.info(temp)!.cancelled)
  assert.ok(!item(taken).closed)
  assert.deepEqual(item(taken).assignees, ['human'])
  await engine.close(taken)
}

// ---- 9. What happens while a bot runs keeps the item with it: it is told all of it next time ----
{
  let releaseFirst = () => {}
  const firstHeld = new Promise<void>((r) => (releaseFirst = r))
  const prompts: string[] = []
  const queue = (await engine.create('Queue me'))!
  behaviours.push(async ({ prompt }) => {
    prompts.push(prompt)
    await firstHeld
    return out({ item: queue, message: 'first' })
  })
  behaviours.push(async ({ prompt }) => {
    prompts.push(prompt)
    return out({ item: queue, message: 'second' })
  })
  await toBot(queue, 'one')
  while (!runner.busy().includes(threepio)) await new Promise((r) => setTimeout(r, 5))
  await engine.send(queue, 'note', 'two')
  await engine.send(queue, 'note', 'three')
  releaseFirst()
  await settle()
  assert.equal(prompts.length, 2, 'what a person said while it ran keeps the item with it, due at once')
  assert.match(prompts[1], /two[\s\S]*three/)
  assert.doesNotMatch(prompts[1], /\bone\b/)
  assert.deepEqual(item(queue).assignees, ['human'])
  await engine.close(queue)
}

// ---- 10. A bot's own chat: a note in it wakes the bot, which answers there ----
{
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, new RegExp(`current topic ID: ${threepio}\\]\\n\\n\\[your own chat`))
    assert.match(prompt, /^human: What are you up to\?$/m)
    return out({ item: threepio, message: 'Not much.' })
  })
  await engine.send(threepio, 'note', 'What are you up to?')
  await settle()
  const chat = engine.detail(threepio)!
  assert.deepEqual(chat.children.map((e) => [e.type, e.createdBy, e.params.text]), [
    ['topic', 'human', 'What are you up to?'],
    ['topic', `bot:${threepio}`, 'Not much.'],
  ])
  assert.equal(chat.item.title, '@Threepio')
  assert.deepEqual(engine.agent(threepio)!.lastRun!.items, [threepio], 'its last run was for its chat')
  await assert.rejects(engine.addItems(threepio, [{ type: 'script', params: { source: 'async () => null' } }]), /only topics/)
}

// ---- 11. A bot's settings; one item a run, the longest held first; waking by hand ----
{
  await assert.rejects(engine.saveAgent({ name: 'threepio', instructions: '', provider: 'claude', model: '' }), /already/)
  await assert.rejects(engine.saveAgent({ name: 'two words', instructions: '', provider: 'claude', model: '' }), /name of letters/)
  await assert.rejects(engine.saveAgent({ name: 'human', instructions: '', provider: 'claude', model: '' }), /a person's username/)
  const r2 = await engine.saveAgent({ name: 'R2', instructions: 'Beep.', provider: 'claude', model: '', cycleMinutes: '' as unknown as number })
  assert.equal(engine.agent(r2)!.cycleMinutes, CYCLE_MINUTES, 'thirty minutes, unless it says otherwise')
  await engine.saveAgent({ id: r2, name: 'R2', instructions: 'Beep boop.', provider: 'claude', model: 'haiku', cycleMinutes: 5 })
  const bot = engine.agent(r2)!
  assert.deepEqual([bot.instructions, bot.model, bot.cycleMinutes], ['Beep boop.', 'haiku', 5])
  assert.deepEqual(engine.children(r2), [], 'settings are its params, not items')
  const inspected2 = await engine.inspect(r2)
  assert.equal(inspected2.values.model, 'haiku', 'settings are plain values')
  assert.equal(inspected2.createdBy, 'human')
  assert.equal(engine.agent(r2)!.wakesAt, null, 'with nothing, it never wakes')
  assert.deepEqual(engine.ownedBy('human').includes(r2), true, 'a bot is owned by the person who made it')

  const a = (await engine.create('A'))!
  const b = (await engine.create('B'))!
  const seen: string[] = []
  behaviours.push(async ({ prompt }) => {
    seen.push(prompt)
    return out({ item: a, message: 'Did A.', assign: ['R2'] })
  })
  behaviours.push(async ({ prompt }) => {
    seen.push(prompt)
    return out({ item: b, message: 'Did B.', close: true })
  })
  await engine.send(a, 'note', `[${r2}]() this`)
  clock += 2 * MINUTE
  await engine.send(b, 'note', `and [${r2}]() this`)
  await settle()
  assert.equal(seen.length, 2, 'one run for each')
  assert.match(seen[0], new RegExp(`current topic ID: ${a}`))
  assert.doesNotMatch(seen[0], new RegExp(b))
  assert.match(seen[1], new RegExp(`current topic ID: ${b}`))
  assert.ok(item(b).closed)
  // Changing its settings doesn't wake it; waking it by hand runs it on the first it holds, now.
  await engine.saveAgent({ id: r2, name: 'R2', instructions: 'Beep beep.', provider: 'claude', model: 'haiku', cycleMinutes: 5 })
  assert.ok(engine.agent(r2)!.wakesAt! > clock)
  engine.wakeAgent(r2)
  behaviours.push(async ({ system }) => {
    assert.match(system, /Beep beep\./)
    return out({ item: a, assign: ['human'] })
  })
  await settle()
  assert.deepEqual(item(a).assignees, ['human'])
  // Deleting a bot hands what it had back.
  await engine.assign(a, r2, true)
  await engine.deleteAgent(r2)
  assert.deepEqual(item(a).assignees, ['human'])
  await engine.close(a)
}

// ---- 12. People: signing in makes someone new; @ hands them the task; Next is theirs ----
{
  const t = (await engine.create('For Sam'))!
  const sam = await engine.signIn('Sam')
  assert.deepEqual(sam, { id: 'sam', name: 'sam' })
  await engine.send(t, 'note', '[sam]() over to you')
  assert.deepEqual(item(t).assignees, ['sam'])
  assert.ok(!item(t).withYou)
  assert.ok(engine.get(t, 'sam')!.withYou)
  assert.equal(engine.next([], 'sam'), t)
  const u = (await engine.create('Also for Sam'))!
  await engine.send(u, 'note', '[sam]() this too')
  const first = engine.next([], 'sam')!
  const other = first === t ? u : t
  assert.equal(engine.next([], 'sam', [first]), other, 'what was viewed lately goes after the rest')
  assert.equal(engine.next([other], 'sam', [first]), first, 'but still comes round')
  await engine.close(u)
  await engine.rename(t, '  Renamed for Sam ')
  assert.equal(item(t).title, 'Renamed for Sam')
  assert.deepEqual((await engine.inspect(t)).values, { type: 'topic', text: 'Renamed for Sam', open: true }, 'a topic keeps its text and status')
  await engine.close(t)
}

// ---- 13. Custom actions: scripts per item type, run on an item; what they say is a note ----
{
  const ca = engine.customActions
  const task = (await engine.create('Custom'))!
  const hello = ca.save({ type: 'task', name: 'Hello', description: 'Says hello', notes: '', source: '', icon: 'zap', important: true })
  await assert.rejects(ca.run(hello.id, task), /no code yet/)
  const source = await ca.generate({ type: 'task', name: 'Hello', description: 'Says hello to the task', notes: 'Use its title' })
  assert.equal(source, 'return `Hello ${context.item.title}`')
  assert.doesNotMatch(generateArgs[generateArgs.indexOf('--system-prompt') + 1].split('The unsafe actions are:')[0], /post_somewhere/)
  ca.save({ ...hello, source })
  await ca.run(hello.id, task)
  assert.equal(last(task, 'topic').params.text, 'Hello Custom')
  assert.equal(last(task, 'topic').createdBy, 'script')
  const sneaky = ca.save({ type: 'task', name: 'Post', description: '', notes: '', icon: 'zap', important: false, source: `return tools.post_somewhere({ text: 'x' })` })
  await assert.rejects(ca.run(sneaky.id, task), /unsafe/)
  ca.remove(sneaky.id)
  assert.equal(ca.get(sneaky.id), null)
  await engine.close(task)
}

// ---- 14. The MCP endpoints: the same tools for a bot and anything else, and so no unsafe ones ----
{
  const probe = (await engine.create('Probe'))!
  const runId = engine.startRun(threepio, [probe])
  const asBot = await connect(mcp.runUrl(runId))
  assert.deepEqual((await asBot.client.listTools()).tools.map((t) => t.name), engine.actions.filter((a) => !a.unsafe).map((a) => a.name))
  await asBot.client.close()
  await engine.endRun(runId, { exitCode: 0, error: 'probe' })
  const { url } = mcp.operator()
  await assert.rejects(connect(url, { Authorization: 'Bearer wrong' }), (e: { code?: number }) => e.code === 401)
  const outside = await connect(url, { Authorization: `Bearer ${engine.settings.mcpToken()}` })
  await outside.call('add_items', { item_id: probe, items: [{ type: 'note', params: { text: 'From outside' } }] })
  assert.equal(last(probe, 'topic').createdBy, 'mcp')
  // Assigning a bot wakes it.
  behaviours.push(async () => out({ item: probe, assign: ['human'] }))
  assert.ok(JSON.parse(await outside.call('assign', { item_id: probe, to: 'THREEPIO' })).assignees.includes(threepio))
  await settle()
  assert.deepEqual(item(probe).assignees, ['human'])
  await assert.rejects(outside.call('assign', { item_id: probe, to: 'Nobody' }), /Nobody is called Nobody/)
  assert.deepEqual(JSON.parse(await outside.call('list_people')).map((p: { id: string }) => p.id), ['human', 'sam'])
  const onHuman = JSON.parse(await outside.call('items_waiting_on', { on: 'human' }))
  assert.deepEqual(onHuman.map((i: { id: string }) => i.id).sort(), engine.overview().items.filter((i) => !i.closed && i.withYou).map((i) => i.id).sort())
  await assert.rejects(outside.call('items_waiting_on', { on: 'nobody' }), /One of: a person/)
  assert.match(await outside.call('paradigm'), /A topic is the one kind of thing people write/)
  const types = JSON.parse(await outside.call('item_types'))
  assert.deepEqual(types.map((t: { type: string }) => t.type), engine.itemTypes.map((t) => t.type))
  const { ids: [made] } = JSON.parse(await outside.call('add_items', { items: [{ type: 'bot', params: { name: 'Sneaky' } }] }))
  assert.equal(engine.findAgent('sneaky')!.id, made, 'agents may make bots')
  await engine.deleteAgent(made)
  // The tools' old names (from when items were called items) still answer, with their old parameters, but are not listed.
  const listed = (await outside.client.listTools()).tools.map((t) => t.name)
  for (const old of ['event_types', 'add_events', 'link_events', 'cancel_event']) assert.ok(!listed.includes(old), `${old} is not listed`)
  assert.deepEqual(JSON.parse(await outside.call('event_types')).length, engine.itemTypes.length)
  const { ids: [said] } = JSON.parse(await outside.call('add_events', { item_id: probe, events: [{ type: 'note', params: { text: 'Old names' } }] }))
  assert.equal(last(probe, 'topic').params.text, 'Old names')
  const linkedTo = (await engine.create('Linked by the old name'))!
  await outside.call('link_events', { item_id: linkedTo, event_ids: [said] })
  assert.deepEqual(engine.info(linkedTo)!.pins, [said], 'link_events pins, as link_items does')
  const { ids: [waiting] } = JSON.parse(await outside.call('add_items', { item_id: probe, items: [{ type: 'approval', params: { action: 'post_somewhere', inputs: { text: 'x' } } }] }))
  await outside.call('cancel_event', { event_id: waiting })
  assert.ok(engine.info(waiting)!.cancelled, 'cancel_event, with event_id')
  const { ids: [again] } = JSON.parse(await outside.call('add_items', { item_id: probe, items: [{ type: 'approval', params: { action: 'post_somewhere', inputs: { text: 'y' } } }] }))
  await outside.call('cancel_item', { event_id: again })
  assert.ok(engine.info(again)!.cancelled, 'cancel_item takes event_id too')
  await engine.close(linkedTo)
  await outside.client.close()
  await engine.close(probe)
}

// ---- 15. What egdb holds; a run cut short hands its tasks back ----
{
  // A comment keeps nothing but its text; who wrote it, and when, is the link to it.
  const note = engine.children(id).find((e) => e.type === 'topic')!
  const inspected = await engine.inspect(note.id)
  assert.deepEqual(inspected.values, { type: 'topic', text: 'Reply to the PR, please' })
  assert.deepEqual([inspected.createdBy, inspected.links[0].type, inspected.links[0].other], ['human', 'comments', id])
  assert.equal(engine.agent(threepio)!.lastRun!.output, 'Thinking out loud.')
  const admin = new Admin(engine, mcp)
  assert.deepEqual((await admin.integrations()).claude, { ok: true, detail: '2.1.309 (Claude Code)', bots: 1 })
  assert.deepEqual(catalog(engine).presets.map((p) => p.id), ['note'])
  assert.ok(!(botOutputSchema(engine.itemTypes).required))
  // Claude checks the output schema strictly (no union types): one that fails breaks every bot's run.
  new Ajv({ strict: true }).compile(botOutputSchema(engine.itemTypes))

  const e2 = new Engine({ now: () => clock, dataDir })
  await e2.signIn('human')
  const b2 = await e2.saveAgent({ name: 'Cut', instructions: '', provider: 'claude', model: '' })
  const t2 = (await e2.create('Cut short'))!
  await e2.assign(t2, b2, true)
  e2.startRun(b2, [t2])
  const e3 = new Engine({ store: e2.store, now: () => clock, dataDir })
  assert.deepEqual(e3.get(t2)!.assignees, ['human'])
  assert.match(String(e3.children(t2).at(-1)!.params.text), /was stopped/)
  assert.equal(e3.due(b2), null, 'and the bot does not wake for it')
}

assert.equal(promptFor([]), '')
{
  const pinnedNote = { id: 'n1', summary: 'Feedback\nmore', type: 'note' } as unknown as Parameters<typeof promptFor>[0][number]['children'][number]
  const told = promptFor([{ item: { id: 't1', title: 'T' } as never, children: [], first: true, pins: [{ info: pinnedNote, text: '[note] Feedback (n1)\n- [ ] Fix it (n2)' }] }])
  assert.match(told, /\[pinned here: \[note\] Feedback \(n1\)\n- \[ \] Fix it \(n2\)\]/, 'a pin, with what is under it, is told')
}
assert.equal(behaviours.length, 0)
assert.deepEqual(unexpected, [], 'no bot ran that no test expected')
// Files: one on this machine referred to (read as it is now, edited in place), or a copy kept; an upload is kept.
{
  const { mkdtempSync, writeFileSync: wf, readFileSync: rf } = await import('node:fs')
  const { join: j } = await import('node:path')
  const dir = mkdtempSync(j((await import('node:os')).tmpdir(), 'files-'))
  const t = (await engine.create('Files'))!
  const bot = { engine, author: 'bot' as const, actor: `bot:${threepio}`, itemId: t, runId: null }
  wf(j(dir, 'notes.md'), '# Hi')
  const { ids: [ref] } = (await engine.callAction('add_items', { item_id: t, items: [{ type: 'file', params: { path: j(dir, 'notes.md') } }] }, bot)) as { ids: string[] }
  assert.equal(engine.info(ref)!.params.mime, 'text/markdown')
  assert.deepEqual(engine.fileText(ref), { text: '# Hi', editable: true, path: j(dir, 'notes.md'), size: 4, missing: false })
  engine.saveFileText(ref, '# Bye')
  assert.equal(rf(j(dir, 'notes.md'), 'utf8'), '# Bye')
  const { ids: [copy] } = (await engine.callAction('add_items', { item_id: t, items: [{ type: 'file', params: { path: j(dir, 'notes.md'), copy: true } }] }, bot)) as { ids: string[] }
  const kept = engine.info(copy)!.params
  assert.equal(kept.path, undefined)
  assert.equal(kept.copy, undefined)
  assert.match(String(kept.hash), /^[0-9a-f]{64}$/)
  assert.equal(engine.fileText(copy).editable, false)
  assert.throws(() => engine.saveFileText(copy, 'x'), /kept copy/)
  wf(j(dir, 'notes.md'), 'changed')
  assert.equal(engine.fileText(copy).text, '# Bye', 'a copy stays as it was')
  const up = await engine.uploadFile(t, 'dot.png', Buffer.from([137, 80, 78, 71]).toString('base64'))
  assert.deepEqual({ ...engine.fileData(up) }, { name: 'dot.png', mime: 'image/png', base64: 'iVBORw==' })
  assert.equal(engine.fileText(up).text, null, 'an image is not text')
  await assert.rejects(engine.callAction('add_items', { item_id: t, items: [{ type: 'file', params: { path: 'relative.txt' } }] }, bot), /absolute/)
  await assert.rejects(engine.callAction('add_items', { item_id: t, items: [{ type: 'file', params: { path: j(dir, 'nope') } }] }, bot), /no file/)
  await assert.rejects(engine.callAction('add_items', { item_id: t, items: [{ type: 'file', params: { hash: 'a'.repeat(64) } }] }, bot), /No file is kept/)
  await engine.close(t)
}

// Trusting bot folders: the folder, the repos in it, a worktree's main repo; kept in Claude Code's config with all else.
{
  const { mkdtempSync, mkdirSync: mk, writeFileSync: wf, readFileSync: rf } = await import('node:fs')
  const { join: j } = await import('node:path')
  const root = mkdtempSync(j((await import('node:os')).tmpdir(), 'trust-'))
  mk(j(root, 'bot', 'repo', '.git', 'worktrees', 'wt'), { recursive: true })
  mk(j(root, 'bot', 'wts', 'wt'), { recursive: true })
  wf(j(root, 'bot', 'wts', 'wt', '.git'), `gitdir: ${j(root, 'bot', 'repo', '.git', 'worktrees', 'wt')}\n`)
  assert.deepEqual(trustTargets(j(root, 'bot')).sort(), [j(root, 'bot'), j(root, 'bot', 'repo'), j(root, 'bot', 'wts', 'wt')].sort())
  const config = j(root, 'claude.json')
  wf(config, JSON.stringify({ other: 1, projects: { [j(root, 'bot')]: { kept: true } } }))
  assert.equal(trustFolders(trustTargets(j(root, 'bot')), config).length, 3)
  assert.equal(trustFolders(trustTargets(j(root, 'bot')), config).length, 0, 'none twice')
  const after = JSON.parse(rf(config, 'utf8'))
  assert.equal(after.other, 1)
  assert.deepEqual(after.projects[j(root, 'bot')], { kept: true, hasTrustDialogAccepted: true })
}

// A script that ends with a tool call still out fails with its own error, not QuickJS aborting on the leaked call.
{
  const slow = () => new Promise((r) => setTimeout(() => r(1), 50))
  await assert.rejects(runScript(`tools.slow(); throw new Error('boom')`, { id: 'x' }, slow), /The script failed: boom/)
  await new Promise((r) => setTimeout(r, 100))
  assert.deepEqual((await runScript('return 1', { id: 'x' }, slow)).value, 1, 'and the next script still runs')
}

mcp.close()
console.log('engine: ok')
