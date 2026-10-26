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
import { Admin } from '../src/engine/admin.ts'
import { DEFAULT_DEBOUNCE, DEFAULT_MODEL, Engine, PERIODIC_MINUTES, type BotOutput, type BotUpdate } from '../src/engine/engine.ts'
import { catalog, startMcp } from '../src/engine/mcp.ts'
import { botOutputSchema } from '../src/engine/schema.ts'
import { ASK, BotRunner, promptFor, type SpawnBot } from '../src/engine/workers.ts'

const MINUTE = 60_000
let clock = 1_000_000

// ---- claude, for what the engine asks of it outside bots' runs ----
let generateArgs: string[] = []
const exec = async (cmd: string, args: string[]) => {
  assert.equal(cmd, 'claude')
  if (args[0] === '-p') {
    generateArgs = args
    return '```js\nasync (context) => `Hello ${context.item.title}`\n```\n'
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
    const out = await behaviour({ call, prompt, system, schema, sessionId, resume, model, cwd })
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
    const due = engine.agents().some((a) => a.wakesAt !== null && a.wakesAt <= clock)
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
const out = (...updates: BotUpdate[]): BotOutput => ({ updates })
const blockers = (id: string) => item(id).blockers.map((b) => b.type)
const last = (id: string, type: string) => engine.children(id).filter((e) => e.type === type).at(-1)!

// The types there are; those from before are gone.
assert.deepEqual(engine.eventTypes.map((t) => t.type), ['task', 'note', 'approval', 'script', 'bot', 'user'])
await assert.rejects(engine.addEvents((await engine.create('x'))!, [{ type: 'message' as 'note', params: { text: 'x' } }]), /No event type message/)
for (const i of engine.overview().items) await engine.close(i.id)

// The bots. Threepio does most things; a new bot runs on opus and waits a minute, unless it says otherwise.
await assert.rejects(engine.send((await engine.create('Too soon'))!, 'note', '  '), /Nothing to send/)
const threepio = await engine.saveAgent({ name: 'Threepio', instructions: 'You speak six million languages.', provider: 'claude', model: '', debounceMinutes: 0 })
assert.deepEqual([engine.agent(threepio)!.model, DEFAULT_MODEL], ['opus', 'opus'])
assert.equal(engine.event(threepio)!.type, 'bot')
const toBot = (id: string, text: string) => engine.send(id, `assign:${threepio}`, text)
for (const i of engine.overview().items) await engine.close(i.id)

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
  assert.ok(prompt.startsWith(`${ASK}\n\n---\n\n[current task ID: ${id}]\n\n[new to you: Answer the PR]`))
  assert.match(prompt, /^human: Reply to the PR, please$/m, "a person's words as they are, after their name")
  assert.ok(JSON.stringify(schema).includes('"enum":["task"]'), 'a bot may add child tasks')
  assert.ok(JSON.stringify(schema).includes('"enum":["bot"]'), 'and bots')
  assert.equal(JSON.parse(await call('get_item', { item_id: id })).item.id, id)
  await assert.rejects(call('post_somewhere', { text: 'x' }), /not found/i)
  // It has the same tools as anyone connected, so it may act on other items too, as itself.
  const other = (await engine.create('Elsewhere'))!
  await call('add_events', { item_id: other, events: [{ type: 'note', params: { text: 'from a bot' } }] })
  assert.equal(engine.children(other)[0].createdBy, `bot:${threepio}`)
  await engine.close(other)
  return out({
    item: id,
    message: 'Drafted a reply. Post it?',
    events: [{ type: 'approval', params: { action: 'post_somewhere', inputs: { text: 'Thanks!' } } }],
  })
})
await toBot(id, 'Reply to the PR, please')
assert.deepEqual(engine.children(id).map((e) => [e.type, e.createdBy]), [['note', 'human']], 'assignment is a link, not an event')
assert.deepEqual(item(id).assignees, [threepio], 'handing it to the bot took it off the sender')
assert.deepEqual(engine.assignedTo(threepio), [id], "the bot's items: its assigned_to links, followed backwards")
await settle()
assert.deepEqual(blockers(id), ['approval'])
assert.deepEqual(item(id).assignees, ['human'], 'a run that leaves the item waiting on a person hands it back')
assert.ok(item(id).withYou, 'an approval is a person\'s turn')
assert.equal(last(id, 'note').params.text, 'Drafted a reply. Post it?')
assert.equal(last(id, 'note').createdBy, `bot:${threepio}`)
assert.deepEqual(engine.agent(threepio)!.lastRun!.items, [id], 'the bot keeps its last run as a value')
assert.ok(engine.agent(threepio)!.lastRun!.endedAt)

// ---- 3. The person changes it and approves; the unsafe action runs, as theirs ----
const approval = item(id).blockers[0]
await engine.approve(approval.id, { text: 'Thanks a lot!' })
assert.deepEqual(posted, ['Thanks a lot!'])
assert.deepEqual(engine.event(approval.id)!.data!.changed, ['text'])
assert.equal(engine.event(approval.id)!.message, 'Ran post_somewhere (changed text)')
for (const a of engine.actions.filter((a) => a.unsafe))
  await assert.rejects(engine.callAction(a.name, { text: 'x' }, { engine, author: 'bot', actor: 'script', itemId: id, runId: null }), /unsafe/)
await assert.rejects(engine.addEvents(id, [{ type: 'approval', params: { action: 'list_items', inputs: {} } }]), /not an unsafe action/)

// ---- 4. A bot that adds a wait keeps the item; the wait resolving is news, heard of in time ----
behaviours.push(async ({ prompt, sessionId, resume }) => {
  assert.equal(resume, true)
  assert.equal(sessionId, session, 'one session for the bot, across its runs')
  assert.match(prompt, /\[task: Answer the PR\]/)
  assert.doesNotMatch(prompt, /Reply to the PR, please/, 'what the session has seen is not sent again')
  return out({ item: id, events: [{ type: 'script', params: { description: 'CI', source: 'async (ctx) => (ctx.state ? { done: true, message: "CI passed" } : { done: false, state: 1 })', schedule: [1] } }] })
})
await toBot(id, 'Wait for CI')
await settle()
assert.deepEqual(item(id).assignees, [threepio], 'it added a wait, so it keeps the item')
clock += 1000
await tick()
clock += 1000
await tick()
assert.deepEqual(blockers(id), [])
assert.equal(last(id, 'script').message, 'CI passed')
assert.equal(engine.agent(threepio)!.wakesAt! > clock, true, 'news in its items is not something happening to it')
behaviours.push(async ({ prompt }) => {
  assert.match(prompt, /\[CI passed\]/)
  return out({ item: id, message: 'All green.', assign: ['human'] })
})
clock += PERIODIC_MINUTES * MINUTE
await settle()
assert.deepEqual(item(id).assignees, ['human'])
await engine.close(id)
assert.ok(item(id).closed)

// ---- 5. Child items: a bot splits work off, gives it to itself, and hears of it closing ----
const parent = (await engine.create('Parent'))!
behaviours.push(async () =>
  out({
    item: parent,
    events: [{ type: 'task', params: { title: 'Child', events: [{ type: 'note', params: { text: 'Do the child' } }], assignees: ['Threepio'] } }],
  }),
)
behaviours.push(async ({ prompt }) => {
  assert.match(prompt, /\[new to you: Child\]/)
  assert.match(prompt, /Do the child/)
  assert.match(prompt, /\[task: Parent\]\n\n\[nothing new\]/, 'every item it has, with only what is new in each')
  const kid = engine.children(parent).find((e) => e.type === 'task')!.id
  return out({ item: kid, message: 'Child done.', close: true })
})
behaviours.push(async ({ prompt }) => {
  assert.match(prompt, /\[Child: closed\]/, 'a child closing is news for its parent, even when the bot closed it')
  return out({ item: parent, message: 'All done.', close: true })
})
await toBot(parent, 'Split it')
await settle()
assert.equal(behaviours.length, 2, 'what it did itself is not something happening to it')
clock += PERIODIC_MINUTES * MINUTE
await settle()
clock += PERIODIC_MINUTES * MINUTE
await settle()
assert.ok(item(parent).closed)
// A bot can also add an item that sits in no item: it blocks nothing, and it is nobody's unless the bot says.
{
  const host = (await engine.create('Host'))!
  behaviours.push(async () => out({ item: host, events: [{ type: 'task', params: { title: 'Unrelated', detached: true } }] }))
  await toBot(host, 'x')
  await settle()
  const loose = engine.overview().items.find((i) => i.title === 'Unrelated')!
  assert.equal(loose.parentId, null)
  assert.deepEqual(loose.assignees, [], 'a bot decides who has what it makes')
  assert.ok(item(host).withYou)
  await engine.close(host)
  await engine.close(loose.id)
}

// ---- 6. Output that can't be used, or an update that can't be applied, is said, and the item goes back ----
const bad = (await engine.create('Bad'))!
behaviours.push(async () => out({ item: bad, events: [{ type: 'user', params: { username: 'Not Valid' } }] }))
await toBot(bad, 'x')
await settle()
assert.match(String(last(bad, 'note').params.text), /^@Threepio failed: its update could not be applied: Bad params for a user event/)
assert.equal(last(bad, 'note').createdBy, 'system')
assert.ok(item(bad).withYou)
behaviours.push(async () => ({ updates: 'nonsense' }) as unknown as BotOutput)
await toBot(bad, 'y')
await settle()
assert.match(String(last(bad, 'note').params.text), /The bot's output was not usable/)
await engine.close(bad)

// ---- 7. Scripts: on their schedule, with state, done when they say; a bot's script gets no unsafe actions ----
{
  const counting = (await engine.create('Counting'))!
  const [sid] = await engine.addEvents(counting, [
    {
      type: 'script',
      params: {
        description: 'Count to two',
        schedule: [1, 5, 10],
        source: `async (ctx, actions) => {
          const items = await actions.list_items({})
          console.log('run', ctx.event.runs, 'items', items.length)
          const n = (ctx.state ?? 0) + 1
          return n > 2 ? { done: true, data: { n: ctx.state, item: ctx.item.title }, message: 'Counted to two' } : { done: false, state: n }
        }`,
      },
    },
  ])
  for (const _ of [1, 5, 10]) {
    clock = engine.event(sid)!.nextCheckAt!
    await tick()
  }
  const done = engine.event(sid)!
  assert.deepEqual(done.data!.result, { n: 2, item: 'Counting' })
  assert.equal(done.message, 'Counted to two')
  assert.match((done.data!.logs as string[])[0], /^run 2 items \d+$/)
  const [bad2] = await engine.addEvents(counting, [{ type: 'script', params: { source: 'async () => { throw new Error("nope") }', schedule: [0] } }])
  await tick()
  assert.match(engine.event(bad2)!.message!, /The script failed: .*nope/)
  // A script with a timeout resolves as timed_out.
  const [slow] = await engine.addEvents(counting, [{ type: 'script', params: { source: 'async () => null', schedule: [60], timeout_minutes: 1 } }])
  clock += MINUTE
  await tick()
  assert.equal(engine.event(slow)!.data!.outcome, 'timed_out')
  // A bot's script gets no unsafe actions.
  behaviours.push(async () =>
    out({ item: counting, assign: ['human'], events: [{ type: 'script', params: { source: `async (ctx, a) => { await a.post_somewhere({ text: 'x' }); return { done: true } }`, schedule: [0] } }] }),
  )
  await toBot(counting, 'post it')
  await tick()
  assert.match(last(counting, 'script').message!, /unsafe/)
  assert.deepEqual(posted, ['Thanks a lot!'])
  await engine.close(counting)
}

// ---- 8. Deleting a bot while it runs: what it returns doesn't land, and its items go back ----
{
  const temp = await engine.saveAgent({ name: 'Temp', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 })
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
  assert.ok(engine.event(temp)!.cancelled)
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
  assert.equal(prompts.length, 1)
  assert.deepEqual(item(queue).assignees, [threepio], 'news came in while it ran, so it keeps the item')
  clock += PERIODIC_MINUTES * MINUTE
  await settle()
  assert.match(prompts[1], /two[\s\S]*three/)
  assert.doesNotMatch(prompts[1], /\bone\b/)
  assert.deepEqual(item(queue).assignees, ['human'])
  await engine.close(queue)
}

// ---- 10. A bot's own chat: a note in it wakes the bot, which answers there ----
{
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, new RegExp(`current task ID: ${threepio}\\]\\n\\n\\[your own chat`))
    assert.match(prompt, /^human: What are you up to\?$/m)
    return out({ item: threepio, message: 'Not much.' })
  })
  await engine.send(threepio, 'note', 'What are you up to?')
  await settle()
  const chat = engine.detail(threepio)!
  assert.deepEqual(chat.events.map((e) => [e.type, e.createdBy, e.params.text]), [
    ['note', 'human', 'What are you up to?'],
    ['note', `bot:${threepio}`, 'Not much.'],
  ])
  assert.equal(chat.item.title, '@Threepio')
  assert.deepEqual(engine.agent(threepio)!.lastRun!.items, [threepio], 'its last run was for its chat')
  await assert.rejects(engine.addEvents(threepio, [{ type: 'script', params: { source: 'async () => null' } }]), /only notes/)
}

// ---- 11. A bot's settings: changing them wakes it; a debounce gathers a burst of work into one run; waking by hand ----
{
  await assert.rejects(engine.saveAgent({ name: 'threepio', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 }), /already/)
  await assert.rejects(engine.saveAgent({ name: 'two words', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 }), /name of letters/)
  await assert.rejects(engine.saveAgent({ name: 'human', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 }), /a person's username/)
  const r2 = await engine.saveAgent({ name: 'R2', instructions: 'Beep.', provider: 'claude', model: '', debounceMinutes: '' as unknown as number })
  assert.equal(engine.agent(r2)!.debounceMinutes, DEFAULT_DEBOUNCE, 'a minute, unless it says otherwise')
  await engine.saveAgent({ id: r2, name: 'R2', instructions: 'Beep boop.', provider: 'claude', model: 'haiku', debounceMinutes: 5 })
  const bot = engine.agent(r2)!
  assert.deepEqual([bot.instructions, bot.model, bot.debounceMinutes], ['Beep boop.', 'haiku', 5])
  assert.deepEqual(engine.children(r2), [], 'settings are its params, not events')
  const inspected2 = await engine.inspect(r2)
  assert.equal(inspected2.values.model, 'haiku', 'settings are plain values')
  assert.equal(inspected2.createdBy, 'human')
  assert.equal(engine.agent(r2)!.wakesAt, null, 'with nothing, it never wakes')

  const a = (await engine.create('A'))!
  const b = (await engine.create('B'))!
  await engine.send(a, 'note', `[${r2}]() this`)
  clock += 2 * MINUTE
  await engine.send(b, 'note', `and [${r2}]() this`)
  await settle()
  assert.equal(engine.agent(r2)!.wakesAt, clock + 5 * MINUTE)
  clock += 5 * MINUTE - 1
  await settle()
  assert.equal(engine.agent(r2)!.runId, null, 'not yet')
  const seen: string[] = []
  behaviours.push(async ({ prompt }) => {
    seen.push(prompt)
    return out({ item: a, message: 'Did A.', assign: ['R2'] }, { item: b, message: 'Did B.', close: true })
  })
  clock += 1
  await settle()
  assert.equal(seen.length, 1)
  assert.match(seen[0], new RegExp(`current task ID: ${a}[\\s\\S]*---[\\s\\S]*current task ID: ${b}`), 'both items, in one prompt')
  assert.ok(item(b).closed)
  // Changing its settings wakes it, its debounce after; waking it by hand is now.
  await engine.saveAgent({ id: r2, name: 'R2', instructions: 'Beep beep.', provider: 'claude', model: 'haiku', debounceMinutes: 5 })
  assert.equal(engine.agent(r2)!.wakesAt, clock + 5 * MINUTE)
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
  await engine.rename(t, '  Renamed for Sam ')
  assert.equal(item(t).title, 'Renamed for Sam')
  assert.deepEqual((await engine.inspect(t)).values, { type: 'task', title: 'Renamed for Sam' }, 'a task keeps only its title')
  await engine.close(t)
}

// ---- 13. Custom actions: scripts per event type, run on an event; what they say is a note ----
{
  const ca = engine.customActions
  const task = (await engine.create('Custom'))!
  const hello = ca.save({ type: 'task', name: 'Hello', description: 'Says hello', notes: '', source: '', icon: 'zap', important: true })
  await assert.rejects(ca.run(hello.id, task), /no code yet/)
  const source = await ca.generate({ type: 'task', name: 'Hello', description: 'Says hello to the task', notes: 'Use its title' })
  assert.equal(source, 'async (context) => `Hello ${context.item.title}`')
  assert.doesNotMatch(generateArgs[generateArgs.indexOf('--system-prompt') + 1].split('The unsafe actions are:')[0], /post_somewhere/)
  ca.save({ ...hello, source })
  await ca.run(hello.id, task)
  assert.equal(last(task, 'note').params.text, 'Hello Custom')
  assert.equal(last(task, 'note').createdBy, 'script')
  const sneaky = ca.save({ type: 'task', name: 'Post', description: '', notes: '', icon: 'zap', important: false, source: `async (c, actions) => actions.post_somewhere({ text: 'x' })` })
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
  await outside.call('add_events', { item_id: probe, events: [{ type: 'note', params: { text: 'From outside' } }] })
  assert.equal(last(probe, 'note').createdBy, 'mcp')
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
  assert.match(await outside.call('paradigm'), /An item is a thing to be done/)
  const types = JSON.parse(await outside.call('event_types'))
  assert.deepEqual(types.map((t: { type: string }) => t.type), engine.eventTypes.map((t) => t.type))
  const { ids: [made] } = JSON.parse(await outside.call('add_events', { events: [{ type: 'bot', params: { name: 'Sneaky' } }] }))
  assert.equal(engine.findAgent('sneaky')!.id, made, 'agents may make bots')
  await engine.deleteAgent(made)
  await outside.client.close()
  await engine.close(probe)
}

// ---- 15. What egdb holds; a run cut short hands its tasks back ----
{
  // A note keeps nothing but its text; who wrote it, and when, is the link to it.
  const note = engine.children(id).find((e) => e.type === 'note')!
  const inspected = await engine.inspect(note.id)
  assert.deepEqual(inspected.values, { type: 'note', text: 'Reply to the PR, please' })
  assert.deepEqual([inspected.createdBy, inspected.links[0].type, inspected.links[0].other], ['human', 'contains', id])
  assert.equal(engine.agent(threepio)!.lastRun!.output, 'Thinking out loud.')
  const admin = new Admin(engine, mcp)
  assert.deepEqual((await admin.integrations()).claude, { ok: true, detail: '2.1.309 (Claude Code)', bots: 1 })
  assert.deepEqual(catalog(engine).presets.map((p) => p.id), ['note'])
  assert.ok(!(botOutputSchema(engine.eventTypes).required))

  const e2 = new Engine({ now: () => clock, dataDir })
  await e2.signIn('human')
  const b2 = await e2.saveAgent({ name: 'Cut', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 })
  const t2 = (await e2.create('Cut short'))!
  await e2.assign(t2, b2, true)
  e2.startRun(b2, [t2])
  const e3 = new Engine({ store: e2.store, now: () => clock, dataDir })
  assert.deepEqual(e3.get(t2)!.assignees, ['human'])
  assert.match(String(e3.children(t2).at(-1)!.params.text), /was stopped/)
  assert.equal(e3.wake(b2), null, 'and the bot does not wake for it')
}

assert.equal(promptFor([]), '')
assert.equal(behaviours.length, 0)
assert.deepEqual(unexpected, [], 'no bot ran that no test expected')
mcp.close()
console.log('engine: ok')
