// The engine, the MCP server and the worker pool, end to end, against an
// in-memory node:sqlite store, stand-ins for gh and Slack, and stand-in bots
// that call the real MCP server over HTTP and return structured output.

import assert from 'node:assert/strict'
import { DatabaseSync } from 'node:sqlite'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import { Admin } from '../src/engine/admin.ts'
import type { EventSpec } from '../src/shared/types.ts'
import { BACKOFF_MINUTES, Engine, type BotOutput } from '../src/engine/engine.ts'
import { catalog, startMcp } from '../src/engine/mcp.ts'
import { botOutputSchema } from '../src/engine/schema.ts'
import { WorkerPool, type SpawnBot } from '../src/engine/workers.ts'

const MINUTE = 60_000
let clock = 1_000_000

// ---- gh ----
const ghCalls: string[][] = []
let checks: 'pending' | 'failed' | 'passed' = 'pending'
let prState = 'OPEN'
let summaryArgs: string[] = []
const exec = async (cmd: string, args: string[]) => {
  if (cmd === 'claude' && args[0] === '-p') {
    summaryArgs = args
    return '- It is about the PR.\n'
  }
  if (cmd === 'claude') return '2.1.309 (Claude Code)\n'
  assert.equal(cmd, 'gh')
  ghCalls.push(args)
  if (args[0] === 'api' && args[1] === 'user') return 'octocat\n'
  if (args[0] === 'pr' && args[1] === 'view' && args.includes('statusCheckRollup'))
    return JSON.stringify({
      statusCheckRollup: [
        { __typename: 'CheckRun', name: 'lint', status: 'COMPLETED', conclusion: 'SUCCESS' },
        {
          __typename: 'CheckRun',
          name: 'e2e',
          status: checks === 'pending' ? 'IN_PROGRESS' : 'COMPLETED',
          conclusion: checks === 'failed' ? 'FAILURE' : checks === 'passed' ? 'SUCCESS' : '',
        },
      ],
    })
  if (args[0] === 'pr' && args[1] === 'view' && args.at(-1) === 'state') return JSON.stringify({ state: prState })
  if (args[0] === 'pr' && ['comment', 'merge'].includes(args[1])) return 'https://github.com/o/r/pull/7\n'
  if (args[0] === 'search' && args.includes('--review-requested=@me'))
    return JSON.stringify([{ number: 7, title: 'Fix it', url: 'https://github.com/o/r/pull/7', repository: { nameWithOwner: 'o/r' } }])
  if (args[0] === 'search')
    return args[2] === 'nothing like this' ? '[]' : JSON.stringify([{ number: 7, title: 'Fix the flaky test', url: 'https://github.com/o/r/pull/7', repository: { nameWithOwner: 'o/r' } }])
  throw new Error(`unexpected gh ${args.join(' ')}`)
}

// ---- Slack ----
const slackCalls: { method: string; body: Record<string, unknown> }[] = []
let thread: { ts: string; user: string; text: string }[] = []
const fakeFetch = (async (url: string, init: { headers: Record<string, string>; body?: string }) => {
  const u = new URL(url)
  const method = u.pathname.replace('/api/', '')
  const body = init.body ? JSON.parse(init.body) : Object.fromEntries(u.searchParams)
  slackCalls.push({ method, body })
  const ok = (r: Record<string, unknown>) => ({ status: 200, json: async () => ({ ok: true, ...r }) })
  if (method === 'auth.test')
    return init.headers.Authorization === 'Bearer xoxp-good-1234'
      ? ok({ team: 'Acme', user: 'alex', user_id: 'UME' })
      : { status: 200, json: async () => ({ ok: false, error: 'invalid_auth' }) }
  if (method === 'chat.postMessage') {
    thread = [{ ts: '100.1', user: 'UME', text: String(body.text) }]
    return ok({ channel: body.channel, ts: '100.1' })
  }
  if (method === 'conversations.replies') return ok({ messages: thread })
  throw new Error(`unexpected Slack ${method}`)
}) as unknown as typeof fetch

const engine = new Engine({ db: new DatabaseSync(':memory:'), exec, fetch: fakeFetch, now: () => clock })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)

// ---- bots: each run plays the next behaviour over MCP and returns its output ----
type Call = (name: string, args?: Record<string, unknown>) => Promise<string>
type Run = { call: Call; prompt: string; schema: Record<string, unknown>; sessionId: string; resume: boolean; forkFrom: string | null }
const behaviours: ((run: Run) => Promise<BotOutput | null>)[] = []
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
const spawnFake: SpawnBot = ({ prompt, schema, mcpUrl, sessionId, resume, forkFrom, onOutput }) => {
  const behaviour = behaviours.shift()
  assert.ok(behaviour, `a bot ran that no test expected:\n${prompt}`)
  const done = (async () => {
    const { client, call } = await connect(mcpUrl)
    const out = await behaviour({ call, prompt, schema, sessionId, resume, forkFrom })
    await client.close()
    onOutput(JSON.stringify({ type: 'assistant', message: { content: [{ type: 'text', text: 'Thinking out loud.' }] } }) + '\n')
    onOutput(JSON.stringify({ type: 'result', total_cost_usd: 0.01, num_turns: 3, structured_output: out ?? undefined }) + '\n')
    return { exitCode: 0 }
  })()
  return { done, kill: () => {} }
}
const pool = new WorkerPool(engine, mcp.runUrl, 2, spawnFake)

const settle = async () => {
  for (let i = 0; i < 300; i++) {
    await engine.idle()
    await new Promise((r) => setTimeout(r, 5))
    const busy = pool.status().some((w) => w.itemId) || engine.overview().items.some((i) => i.blockers.some((b) => b.type === 'bot'))
    if (!busy) return engine.idle()
  }
  throw new Error(`still busy: ${JSON.stringify(pool.status())}`)
}
const tick = async () => {
  await engine.tick()
  await settle()
}
const item = (id: string) => engine.get(id)!
const blockers = (id: string) => item(id).blockers.map((b) => b.type)
const last = (id: string, type: string) => engine.children(id).filter((e) => e.type === type).at(-1)!

// The script a bot leaves behind a CI wait: another bot if it failed; nothing (so the human's turn) if it passed.
const route = `async ({ event }) => event.data.outcome === 'failed'
  ? { type: 'bot', params: { instructions: 'Fix the failing checks: ' + event.data.failed.join(', ') } }
  : null`
const ciThenRoute: EventSpec[] = [
  { type: 'github', ref: 'ci', params: { repo: 'o/r', number: 7, until: 'checks' } },
  { type: 'after', params: { event: 'ci', script: route, description: 'A bot if CI failed' } },
]

// ---- 1. A new item has no events, so it is the human's turn ----
const id = (await engine.create('Answer the PR'))!
assert.deepEqual(engine.children(id), [])
assert.ok(item(id).withHuman)
assert.equal(engine.next(), id)
assert.equal(engine.next([id]), null)

// ---- 2. The human sends a message for a bot; the bot asks to post a reply and chains a CI wait behind it ----
let session = ''
behaviours.push(async ({ call, prompt, schema, sessionId, resume }) => {
  session = sessionId
  assert.equal(resume, false)
  assert.ok(prompt.startsWith(`[current item ID: ${id}]\n\n[new item: Answer the PR]`))
  assert.match(prompt, /^Reply to the PR, please$/m, "the human's words as they are")
  assert.doesNotMatch(prompt, /\d{4}-\d\d-\d\dT/, 'no timestamps')
  assert.ok(JSON.stringify(schema).includes('"after"'))
  assert.ok(JSON.stringify(schema).includes('"enum":["item"]'), 'a bot may add child items')
  assert.equal(JSON.parse(await call('get_item', { item_id: id })).item.id, id)
  await assert.rejects(call('github_pr_comment', { repo: 'o/r', number: 7, body: 'x' }), /not found/i)
  // It has the same tools as anyone connected, so it may act on other items too, as a bot.
  const other = (await engine.create('Elsewhere'))!
  await call('add_events', { item_id: other, events: [{ type: 'message', params: { text: 'from a bot' } }] })
  assert.equal(engine.children(other)[0].createdBy, 'bot')
  await engine.close(other)
  return {
    message: 'Drafted a reply. Post it?',
    events: [
      { type: 'approval', ref: 'post', params: { action: 'github_pr_comment', inputs: { repo: 'o/r', number: 7, body: 'Thanks!' } } },
      { type: 'after', params: { event: 'post', then: ciThenRoute } },
    ],
  }
})
await engine.send(id, 'bot', 'Reply to the PR, please')
assert.deepEqual(engine.children(id).slice(0, 2).map((e) => [e.type, e.createdBy]), [['message', 'human'], ['bot', 'human']])
assert.ok(!item(id).withHuman, 'a bot blocker is waiting')
await settle()
assert.deepEqual(blockers(id), ['approval', 'after'])
assert.ok(item(id).withHuman, 'an approval, and an after waiting on it, are the human\'s turn')
assert.equal(last(id, 'message').params.text, 'Drafted a reply. Post it?')
assert.equal(last(id, 'message').createdBy, 'bot')
const firstBot = engine.children(id).find((e) => e.type === 'bot')!
assert.ok(firstBot.resolved)
assert.equal(firstBot.data!.output, 'Thinking out loud.')

// ---- 3. The human changes the reply and approves it; the CI wait starts and backs off ----
clock += 2 * MINUTE
engine.viewed(id)
const approval = item(id).blockers.find((b) => b.type === 'approval')!
await engine.approve(approval.id, { repo: 'o/r', number: 7, body: 'Thanks a lot!' })
assert.deepEqual(ghCalls.at(-1), ['pr', 'comment', '7', '-R', 'o/r', '--body', 'Thanks a lot!'])
assert.deepEqual(engine.event(approval.id)!.data!.changed, ['body'])
assert.equal(engine.event(approval.id)!.message, 'Ran github_pr_comment (changed body)')
assert.equal(engine.event(approval.id)!.actor, 'human')
await settle()
assert.deepEqual(blockers(id), ['github', 'after'])
assert.ok(!item(id).withHuman)
await tick()
for (const minutes of BACKOFF_MINUTES.concat([10])) {
  const ci = item(id).blockers[0]
  assert.equal(ci.nextCheckAt, clock + minutes * MINUTE)
  clock = ci.nextCheckAt! - 1
  await tick()
  assert.equal(item(id).blockers[0].nextCheckAt, ci.nextCheckAt, 'not checked before it is due')
  clock = ci.nextCheckAt!
  await tick()
}

// ---- 4. CI fails: the after's script adds a bot, which resumes the item's session and chains the same again ----
behaviours.push(async ({ prompt, sessionId, resume }) => {
  assert.equal(resume, true)
  assert.equal(sessionId, session)
  assert.doesNotMatch(prompt, /new item/)
  assert.match(prompt, /^\[o\/r#7: 1 of 2 checks failed: e2e\]$/m)
  assert.doesNotMatch(prompt, /: done\]/, 'an after that ran says nothing: what it added speaks for it')
  assert.match(prompt, /Fix the failing checks: e2e/)
  assert.doesNotMatch(prompt, /Reply to the PR, please/, 'what the session has seen is not sent again')
  checks = 'pending' // it pushed a fix, so CI runs again
  return { message: 'Fixed e2e.', events: ciThenRoute }
})
checks = 'failed'
clock = item(id).blockers[0].nextCheckAt!
await tick()
assert.deepEqual(blockers(id), ['github', 'after'])

// ---- 5. CI passes: the script adds nothing, so it is the human's turn, and nothing is asked ----
checks = 'passed'
clock = item(id).blockers[0].nextCheckAt!
await tick()
assert.deepEqual(blockers(id), [])
assert.ok(item(id).withHuman)
assert.equal(engine.next(engine.overview().items.filter((i) => i.id !== id).map((i) => i.id)), id)

// Every event reads the same way, whatever its type: a summary always, a message once resolved.
{
  const all = engine.children(id)
  assert.ok(all.every((e) => typeof e.summary === 'string' && e.summary.length))
  assert.ok(all.filter((e) => !e.resolved).every((e) => e.message === null))
  const ci = all.find((e) => e.type === 'github')!
  assert.equal(ci.summary, 'o/r#7: until checks')
  assert.match(ci.message!, /1 of 2 checks failed: e2e/)
  assert.equal(ci.actor, 'system')
  assert.equal(all.find((e) => e.type === 'bot')!.message, null, "a bot's reply is its own message")
  assert.deepEqual(
    all.filter((e) => e.type === 'message').map((e) => [e.actor, e.message]),
    [['human', 'Reply to the PR, please'], ['bot', 'Drafted a reply. Post it?'], ['bot', 'Fixed e2e.']],
  )
}

// The trace has every moment.
{
  const ci = engine.children(id).find((e) => e.type === 'github')!
  assert.equal(ci.times.checks, 7)
  assert.ok(ci.times.added && ci.times.resolved && ci.times.firstChecked! < ci.times.lastChecked!)
  assert.ok(firstBot.times.firstRunStarted && firstBot.times.firstRunEnded)
  const trace = engine.trace(id)
  assert.ok(trace.some((t) => t.kind === 'viewed'))
  assert.ok(trace.some((t) => t.kind === 'action_called' && t.actor === 'bot' && t.data!.action === 'get_item' && t.runId))
  assert.ok(trace.some((t) => t.kind === 'action_called' && t.actor === 'human' && t.data!.action === 'github_pr_comment' && t.data!.ok))
  assert.ok(trace.every((t, i) => i === 0 || t.at >= trace[i - 1].at))
}

// ---- 6. Presets that wait on a PR found by its title; several blockers at once ----
checks = 'pending'
await engine.send(id, 'pr-checks', 'flaky test')
await engine.send(id, 'pr-merged', 'flaky test')
await settle()
assert.deepEqual(blockers(id), ['github', 'github'])
assert.equal(item(id).blockers[0].params.number, 7)
await engine.send(id, 'pr-merged', 'nothing like this')
await settle()
assert.equal(last(id, 'github').data!.outcome, 'not_found')
prState = 'MERGED'
checks = 'passed'
clock = Math.max(...item(id).blockers.map((b) => b.nextCheckAt!))
await tick()
assert.deepEqual(blockers(id), [], 'both waits resolved: checks passed, and merged')
await assert.rejects(engine.send(id, 'nope', 'x'), /No preset nope/)
await engine.close(id)
assert.ok(item(id).closed)
assert.equal(item(id).how, 'closed')
assert.match(engine.event(id)!.message!, /Answer the PR: closed/)

// ---- 7. Slack: a bot's message becomes an approval, and the wait takes the thread it posted ----
engine.settings.addSlackWorkspace({ name: 'Acme', team: 'Acme', user: 'alex', userId: 'UME', token: 'xoxp-good-1234' })
const chase = (await engine.create('Chase Sam'))!
behaviours.push(async () => ({ events: [{ type: 'slack', params: { channel: 'C1', message: 'Any update, Sam?' } }] }))
await engine.send(chase, 'bot', 'Ask Sam for an update')
await settle()
assert.deepEqual(blockers(chase), ['approval', 'after'])
assert.equal(item(chase).blockers[0].params.action, 'slack_post')
assert.equal(slackCalls.filter((c) => c.method === 'chat.postMessage').length, 0)
await engine.approve(item(chase).blockers[0].id, { channel: 'C1', text: 'Any update, Sam?' })
await settle()
assert.deepEqual(blockers(chase), ['slack'])
assert.equal(item(chase).blockers[0].params.thread_ts, '100.1')
await tick()
assert.deepEqual(blockers(chase), ['slack'], 'my own message is not a reply')
thread.push({ ts: '100.2', user: 'USAM', text: 'Done!' })
clock = item(chase).blockers[0].nextCheckAt!
await tick()
assert.deepEqual(blockers(chase), [])
assert.match(String(last(chase, 'slack').data!.detail), /Done!/)
await engine.close(chase)

// ---- 8. Items are events: a bot splits off a child item, which blocks its parent and forks its session ----
const parent = (await engine.create('Parent'))!
let parentSession = ''
behaviours.push(async ({ sessionId }) => {
  parentSession = sessionId
  return {
    events: [
      { type: 'item', ref: 'kid', params: { title: 'Child', events: [{ type: 'message', params: { text: 'Do the child' } }, { type: 'bot', params: {} }] } },
      { type: 'after', params: { event: 'kid', then: [{ type: 'bot', params: { instructions: 'Wrap up' } }] } },
    ],
  }
})
behaviours.push(async ({ prompt, forkFrom, resume, sessionId }) => {
  assert.equal(forkFrom, parentSession, "a child's first run forks its parent's session")
  assert.equal(resume, false)
  assert.notEqual(sessionId, parentSession)
  assert.match(prompt, /split off from the one above/)
  assert.match(prompt, /Do the child/)
  return { message: 'Child done.', close: true }
})
behaviours.push(async ({ prompt, resume, sessionId }) => {
  assert.equal(resume, true)
  assert.equal(sessionId, parentSession)
  assert.match(prompt, /Wrap up/)
  assert.match(prompt, /Child: closed/)
  return { message: 'All done.', close: true }
})
await engine.send(parent, 'bot', 'Split it')
await settle()
const child = engine.children(parent).find((e) => e.type === 'item')!.id
await settle()
assert.ok(item(child).closed)
assert.equal(item(child).parentId, parent)
assert.ok(item(child).sessionId && item(child).sessionId !== parentSession, "the child's session is its own, and kept")
assert.equal(engine.event(child)!.params.events, undefined, 'its first events are in it, not in its params')
assert.deepEqual(engine.children(child).map((e) => e.type), ['message', 'bot', 'message'])
assert.ok(item(parent).closed)

// A bot can also add an item that sits in no item, for unrelated work: it blocks nothing.
{
  const host = (await engine.create('Host'))!
  behaviours.push(async () => ({ events: [{ type: 'item', params: { title: 'Unrelated', detached: true } }] }))
  await engine.send(host, 'bot', 'x')
  await settle()
  const loose = engine.overview().items.find((i) => i.title === 'Unrelated')!
  assert.equal(loose.parentId, null)
  assert.ok(item(host).withHuman, 'a detached item blocks nothing')
  await engine.close(host)
  await engine.close(loose.id)
}

// ---- 9. A bot whose output is unusable leaves a message saying why ----
const bad = (await engine.create('Bad'))!
behaviours.push(async () => ({ events: [{ type: 'action', params: { name: 'github_pr_merge', inputs: {} } }] }) as BotOutput)
await engine.send(bad, 'bot', 'x')
await settle()
assert.match(last(bad, 'bot').message!, /^The bot failed: .*may not add a action/)
assert.ok(last(bad, 'bot').data!.error)
await engine.handledOutside(bad)

// ---- 10. Scripts reach the actions; a bot's script gets no unsafe ones ----
const scripted = (await engine.create('Scripted'))!
await engine.addEvents(scripted, [
  { type: 'message', ref: 'hello', params: { text: 'hello' } },
  { type: 'after', params: { event: 'hello', script: `async (input, actions) => { const all = await actions.list_items({}); console.log('saw', all.length); return { type: 'message', params: { text: 'Saw ' + all.length } } }` } },
])
await settle()
assert.match(String(last(scripted, 'message').params.text), /^Saw \d+$/)
assert.deepEqual((last(scripted, 'after').data!.logs as string[])[0].split(' ')[0], 'saw')
const sneaky = (await engine.create('Sneaky'))!
const merges = ghCalls.filter((c) => c[1] === 'merge').length
behaviours.push(async () => ({
  events: [
    { type: 'github', ref: 'w', params: { query: 'nothing like this', until: 'merged' } },
    { type: 'after', params: { event: 'w', script: `async (i, actions) => { await actions.github_pr_merge({ repo: 'o/r', number: 7, method: 'squash' }); return null }` } },
  ],
}))
await engine.send(sneaky, 'bot', 'x')
await settle()
assert.equal(ghCalls.filter((c) => c[1] === 'merge').length, merges)
assert.match(String(last(sneaky, 'after').data!.error), /unsafe/)

// ---- 10b. Script events run on their schedule, carry state between runs, and finish when they say so ----
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
  const at = (s: number) => engine.event(sid)!.nextCheckAt! - clock === s * 1000
  assert.ok(at(1), 'first run after a second')
  assert.equal(engine.event(sid)!.summary, 'Count to two')
  for (const gap of [1, 5, 10]) {
    clock = engine.event(sid)!.nextCheckAt!
    await tick()
    if (gap < 10) assert.ok(at([1, 5, 10][[1, 5, 10].indexOf(gap) + 1]), `then ${gap}`)
  }
  const done = engine.event(sid)!
  assert.ok(done.resolved)
  assert.deepEqual(done.data!.result, { n: 2, item: 'Counting' })
  assert.equal(done.message, 'Counted to two')
  assert.equal(done.data!.runs, 3)
  assert.match((done.data!.logs as string[])[0], /^run 2 items \d+$/)
  assert.ok(item(counting).withHuman, 'nothing blocks it once the script is done')

  // A script that returns something else, or throws, ends with the error.
  const [bad1] = await engine.addEvents(counting, [{ type: 'script', params: { source: 'async () => 42', schedule: [0] } }])
  const [bad2] = await engine.addEvents(counting, [{ type: 'script', params: { source: 'async () => { throw new Error("nope") }', schedule: [0] } }])
  await tick()
  assert.match(engine.event(bad1)!.message!, /The script failed: It returned 42/)
  assert.match(engine.event(bad2)!.message!, /The script failed: .*nope/)

  // Two looks for due checks at once run a script once: a run never starts while the last is going.
  const [slowpoke] = await engine.addEvents(counting, [{ type: 'script', params: { source: 'async () => null', schedule: [5] } }])
  clock = engine.event(slowpoke)!.nextCheckAt!
  await Promise.all([engine.checkDue(), engine.checkDue(), engine.checkDue()])
  assert.equal(engine.event(slowpoke)!.times.checks, 1)
  await engine.cancel(slowpoke)

  // A bot's script gets no unsafe actions.
  behaviours.push(async () => ({ events: [{ type: 'script', params: { source: `async (ctx, a) => { await a.github_pr_merge({ repo: 'o/r', number: 7, method: 'squash' }); return { done: true } }`, schedule: [0] } }] }))
  await engine.send(counting, 'bot', 'merge it')
  await tick()
  assert.match(last(counting, 'script').message!, /unsafe/)
}

// ---- 11. A wait with a timeout resolves as timed_out ----
checks = 'pending'
const slow = (await engine.create('Slow'))!
await engine.addEvents(slow, [{ type: 'github', params: { repo: 'o/r', number: 7, until: 'checks', timeout_minutes: 3 } }])
await tick()
clock += 3 * MINUTE
await tick()
assert.equal(last(slow, 'github').data!.outcome, 'timed_out')

// ---- 12. Sources create items once ----
await engine.pollSources(true)
await engine.pollSources(true)
const reviews = engine.overview().items.filter((i) => i.title.startsWith('Review '))
assert.equal(reviews.length, 1)
assert.match(String(engine.children(reviews[0].id)[0].params.text), /review is requested/)

// ---- 13. Cancelling a bot that is running: what it returns doesn't land ----
let release = () => {}
const held = new Promise<void>((r) => (release = r))
behaviours.push(async () => {
  await held
  return { message: 'late', close: true }
})
const taken = (await engine.create('Take me'))!
await engine.send(taken, 'bot', 'x')
while (!pool.status().some((w) => w.itemId === taken)) await new Promise((r) => setTimeout(r, 5))
await engine.cancel(last(taken, 'bot').id)
release()
await settle()
assert.ok(last(taken, 'bot').cancelled)
assert.equal(last(taken, 'bot').message, 'Cancelled: Bot')
assert.ok(!item(taken).closed, "a run that was cancelled doesn't land")
assert.ok(item(taken).withHuman)

// ---- 13b. Messages sent while a bot runs queue behind it: one more bot, which is told all of them ----
let releaseFirst = () => {}
const firstHeld = new Promise<void>((r) => (releaseFirst = r))
const prompts: string[] = []
behaviours.push(async ({ prompt }) => {
  prompts.push(prompt)
  await firstHeld
  return { message: 'first' }
})
behaviours.push(async ({ prompt }) => {
  prompts.push(prompt)
  return { message: 'second' }
})
const queue = (await engine.create('Queue me'))!
await engine.send(queue, 'bot', 'one')
while (!pool.status().some((w) => w.itemId === queue)) await new Promise((r) => setTimeout(r, 5))
await engine.send(queue, 'bot', 'two')
await engine.send(queue, 'bot', 'three')
await engine.addEvents(queue, [{ type: 'bot', params: {} }], 'bot', 'bot')
assert.equal(engine.children(queue).filter((e) => e.type === 'bot').length, 2, 'a running bot queues one more; a queued one takes no other')
assert.equal(pool.status().filter((w) => w.itemId === queue).length, 1, "the queued bot doesn't start while one runs")
await engine.addEvents(queue, [{ type: 'bot', params: { instructions: 'And tidy up' } }], 'bot', 'bot')
assert.equal(engine.children(queue).filter((e) => e.type === 'bot').length, 3, 'a bot with something new to ask is still added')
releaseFirst()
await settle()
assert.equal(prompts.length, 2, 'the queued bots are answered by one run')
assert.match(prompts[1], /two[\s\S]*three/)
assert.match(prompts[1], /And tidy up/)
assert.doesNotMatch(prompts[1], /\bone\b/)
assert.equal(last(queue, 'message').params.text, 'second')

// ---- 14. The unsafe rule: every action says which it is, and a bot's MCP offers none of the unsafe ones ----
assert.ok(engine.actions.every((a) => typeof a.unsafe === 'boolean'))
const probe = (await engine.create('Probe'))!
const runId = engine.startRun(item(probe), [], 9, 'probe')
const asBot = await connect(mcp.runUrl(runId))
assert.deepEqual((await asBot.client.listTools()).tools.map((t) => t.name), engine.actions.filter((a) => !a.unsafe).map((a) => a.name))
await asBot.client.close()
await engine.endRun(runId, { exitCode: 0, error: 'probe' })
for (const a of engine.actions.filter((a) => a.unsafe))
  await assert.rejects(engine.callAction(a.name, {}, { engine, author: 'bot', actor: 'script', itemId: probe, runId: null }), /unsafe/)

// ---- 15. The fixed endpoint, for any other agent: the same tools as a bot, and so no unsafe ones ----
const { url } = mcp.operator()
await assert.rejects(connect(url, { Authorization: 'Bearer wrong' }), (e: { code?: number }) => e.code === 401)
const outside = await connect(url, { Authorization: `Bearer ${engine.settings.mcpToken()}` })
assert.deepEqual((await outside.client.listTools()).tools.map((t) => t.name), engine.actions.filter((a) => !a.unsafe).map((a) => a.name))
await outside.call('add_events', { item_id: taken, events: [{ type: 'message', params: { text: 'From outside' } }] })
assert.equal(last(taken, 'message').createdBy, 'mcp')
await assert.rejects(outside.call('github_pr_comment', { repo: 'o/r', number: 7, body: 'Hi' }), /not found/i)
// An outside agent's Slack message gets an approval before it, as a bot's does.
await outside.call('add_events', { item_id: taken, events: [{ type: 'slack', params: { channel: 'C1', message: 'hi' } }] })
assert.ok(item(taken).blockers.some((b) => b.type === 'approval' && b.params.action === 'slack_post'))
// Items listed by what they wait on.
const onApproval = JSON.parse(await outside.call('items_waiting_on', { on: 'approval' }))
assert.ok(onApproval.some((i: { id: string; blockers: { type: string }[] }) => i.id === taken && i.blockers.every((b) => b.type === 'approval')))
const onHuman = JSON.parse(await outside.call('items_waiting_on', { on: 'human' }))
assert.deepEqual(onHuman.map((i: { id: string }) => i.id).sort(), engine.overview().items.filter((i) => !i.closed && i.withHuman).map((i) => i.id).sort())
await assert.rejects(outside.call('items_waiting_on', { on: 'nobody' }), /One of: human/)
assert.equal(JSON.parse(await outside.call('next_for_human'))?.id ?? null, engine.next())
assert.match(await outside.call('paradigm'), /An item is a thing to be done/)
assert.match(outside.client.getInstructions() ?? '', /blocker/i, 'the server says how it works as it connects')
const types = JSON.parse(await outside.call('event_types'))
assert.deepEqual(types.map((t: { type: string }) => t.type), engine.eventTypes.map((t) => t.type))
await outside.client.close()

// ---- 16. The history is there to look back on ----
const recent = engine.recent(7)
const first = recent.find((r) => r.id === id)!
assert.equal(first.botRuns, 2)
assert.equal(first.approvalsChanged, 1)
assert.ok(first.minutesBlockedBy.github >= 28, JSON.stringify(first.minutesBlockedBy))
assert.ok(recent.find((r) => r.id === bad)!.handledOutside)
assert.equal(recent.find((r) => r.id === bad)!.botFailures, 1)
assert.equal(recent.find((r) => r.id === slow)!.timedOut, 1)
assert.match(engine.transcript(engine.detail(id)!.runs[0].id)!, /Thinking out loud/)

// ---- 17. Integrations and the catalog ----
const admin = new Admin(engine, mcp, 2, fakeFetch)
assert.equal((await admin.integrations()).github.detail, 'Signed in as octocat')
await assert.rejects(admin.addSlack('nope'), /xoxp-/)
await assert.rejects(admin.addSlack('xoxp-bad'), /invalid_auth/)
await admin.addSlack('xoxp-good-1234')
const cat = catalog(engine)
assert.ok(cat.actions.find((a) => a.name === 'slack_post')!.unsafe)
assert.deepEqual(cat.presets.map((p) => p.id), ['message', 'bot', 'pr-merged', 'pr-checks'])
// A loop: a script that adds a bot on every run and never finishes, until it is cancelled.
{
  const looping = (await engine.create('Looping'))!
  const [loop] = await engine.addEvents(looping, [
    {
      type: 'script',
      params: {
        description: 'Every hour, a bot',
        schedule: [3600],
        source: `async (ctx, actions) => { await actions.add_events({ item_id: ctx.item.id, events: [{ type: 'bot', params: { instructions: 'Go round again' } }] }); return { done: false } }`,
      },
    },
  ])
  for (let round = 0; round < 2; round++) {
    behaviours.push(async ({ prompt }) => {
      assert.match(prompt, /\[instructions: Go round again\]/)
      return {}
    })
    clock = engine.event(loop)!.nextCheckAt!
    await tick()
    await settle()
  }
  assert.equal(engine.children(looping).filter((e) => e.type === 'bot').length, 2)
  assert.ok(!engine.event(loop)!.resolved, 'a loop runs until it is cancelled')
  await engine.cancel(loop)
  await engine.close(looping)
}
// Summarising: a cheap model, no tools, given the item; kept in the trace, not added to the item.
{
  const s = (await engine.create('Summed up'))!
  await engine.send(s, 'message', 'The plan is to ship on Friday')
  assert.equal(await engine.summarise(s), '- It is about the PR.')
  assert.equal(summaryArgs[summaryArgs.indexOf('--model') + 1], 'haiku')
  assert.equal(summaryArgs[summaryArgs.indexOf('--tools') + 1], '')
  assert.match(summaryArgs[1], /Task: Summed up/)
  assert.match(summaryArgs[1], /ship on Friday/)
  assert.equal(engine.children(s).length, 1)
  assert.ok(engine.trace(s).some((t) => t.kind === 'summarised'))
  await engine.close(s)
}
// Priority items come first in Next, the most recently active first; then the rest, the longest waiting first.
{
  for (const i of engine.overview().items.filter((i) => !i.closed)) await engine.close(i.id)
  const old = (await engine.create('Old'))!
  clock += 1000
  const p1 = (await engine.create('P1'))!
  clock += 1000
  const p2 = (await engine.create('P2'))!
  assert.equal(engine.next(), old, 'the longest waiting, with no priorities')
  await engine.setPriority(p1, true)
  clock += 1000
  await engine.setPriority(p2, true)
  assert.ok(item(p2).priority)
  assert.equal(engine.next(), p2, 'the most recently active priority')
  assert.equal(engine.next([p2]), p1)
  await engine.setPriority(p2, false)
  assert.ok(!item(p2).priority)
  assert.equal(engine.next(), p1)
  assert.deepEqual(engine.children(p2).map((e) => e.message), ['Marked as a priority', 'No longer a priority'])
  for (const i of [old, p1, p2]) await engine.close(i)
}
// The default preset only says what is typed; with nothing typed, it runs a bot.
{
  const quiet = (await engine.create('Quiet'))!
  await engine.send(quiet, 'message', 'Just noting this')
  assert.deepEqual(engine.children(quiet).map((e) => e.type), ['message'])
  assert.ok(item(quiet).withHuman, 'a message alone leaves it with the human')
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /^Just noting this$/m, 'the bot gets what was said before it')
    return { message: 'Noted.' }
  })
  await engine.send(quiet, 'message', '  ')
  assert.deepEqual(engine.children(quiet).map((e) => e.type), ['message', 'bot'], 'sent empty, it runs a bot')
  await settle()
  await engine.close(quiet)
}
assert.ok(!(botOutputSchema(engine.eventTypes).required))

// ---- 18. Attaching gathers any event in another item, and leaves it where it was ----
const collector = (await engine.create('Build the ideas'))!
const idea = (await engine.create('An idea'))!
const [said] = await engine.addEvents(idea, [{ type: 'message', params: { text: 'Do it this way' } }])
await engine.attach(idea, collector)
await engine.callAction('attach_event', { event_id: said, item_id: collector }, { engine, author: 'bot', actor: 'bot', itemId: collector, runId: null })
const gathered = engine.children(collector).filter((e) => e.type === 'attachment')
assert.deepEqual(gathered.map((e) => e.params), [
  { event: idea, title: 'An idea', item: idea },
  { event: said, title: 'Do it this way', item: idea },
])
assert.equal(gathered[0].message, 'Attached: An idea')
assert.ok(gathered.every((e) => e.resolved))
assert.equal(engine.event(idea)!.parentId, null)
assert.equal(engine.children(idea).length, 1)
await assert.rejects(engine.attach(collector, collector), /itself/)
await assert.rejects(engine.attach(idea, said), /No item/)
await engine.close(collector)
await assert.rejects(engine.attach(idea, collector), /closed/)

assert.equal(behaviours.length, 0)
mcp.close()
console.log('engine: ok')
