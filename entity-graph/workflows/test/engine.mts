// The engine, the MCP server and the bots' runner, end to end, against an
// in-memory store, stand-ins for gh and Slack, and stand-in bots that call the
// real MCP server over HTTP and return structured output.

import assert from 'node:assert/strict'
import { existsSync, mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import { Admin } from '../src/engine/admin.ts'
import type { EventSpec } from '../src/shared/types.ts'
import { BACKOFF_MINUTES, Engine, type BotOutput, type BotUpdate } from '../src/engine/engine.ts'
import { catalog, startMcp } from '../src/engine/mcp.ts'
import { botOutputSchema } from '../src/engine/schema.ts'
import { BotRunner, promptFor, type SpawnBot } from '../src/engine/workers.ts'

const MINUTE = 60_000
let clock = 1_000_000

// ---- gh ----
const ghCalls: string[][] = []
let checks: 'pending' | 'failed' | 'passed' = 'pending'
let prState = 'OPEN'
let summaryArgs: string[] = []
let generateArgs: string[] = []
const exec = async (cmd: string, args: string[]) => {
  if (cmd === 'claude' && args[0] === '-p' && args.includes('sonnet')) {
    generateArgs = args
    return '```js\nasync (context) => `Hello ${context.item.title}`\n```\n'
  }
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

const dataDir = mkdtempSync(join(tmpdir(), 'workflows-test-'))
const engine = new Engine({ exec, fetch: fakeFetch, now: () => clock, dataDir })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)

// ---- bots: each run plays the next behaviour over MCP and returns its output ----
type Call = (name: string, args?: Record<string, unknown>) => Promise<string>
type Run = { call: Call; prompt: string; system: string; schema: Record<string, unknown>; sessionId: string; resume: boolean; model: string; cwd: string }
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
const spawnFake: SpawnBot = ({ prompt, system, schema, mcpUrl, sessionId, resume, model, cwd, onOutput }) => {
  const behaviour = behaviours.shift()
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

// The script a bot leaves behind a CI wait: the bot again if it failed; nothing (so the human's turn) if it passed.
const route = `async ({ event }) => event.data.outcome === 'failed'
  ? [{ type: 'message', params: { text: 'Fix the failing checks: ' + event.data.failed.join(', ') } }, { type: 'assign', params: { to: 'threepio' } }]
  : null`
// The bots. Threepio does most things.
await assert.rejects(engine.send((await engine.create('Too soon'))!, 'message', '  '), /Nothing to send/)
await assert.rejects(engine.addEvents(null, [{ type: 'item', params: { title: 'x', events: [{ type: 'bot', params: {} }] } }]), /no bot to assign/)
const threepio = await engine.saveAgent({ name: 'Threepio', instructions: 'You speak six million languages.', provider: 'claude', model: 'opus', debounceMinutes: 0 })
const toBot = (id: string, text: string) => engine.send(id, `assign:${threepio}`, text)
for (const i of engine.overview().items) await engine.close(i.id)

const ciThenRoute: EventSpec[] = [
  { type: 'github', ref: 'ci', params: { repo: 'o/r', number: 7, until: 'checks' } },
  { type: 'after', params: { event: 'ci', script: route, description: 'A bot if CI failed' } },
]

// ---- 1. A new item has no events, so it is the human's turn ----
const id = (await engine.create('Answer the PR'))!
assert.deepEqual(engine.children(id), [])
assert.deepEqual(item(id).assignees, [], 'nobody is assigned, so it is the human\'s')
assert.ok(item(id).withHuman)
assert.equal(engine.next(), id)
assert.equal(engine.next([id]), null)

// ---- 2. The human hands the item to a bot with a message; the bot asks to post a reply and chains a CI wait behind it ----
let session = ''
behaviours.push(async ({ call, prompt, system, schema, sessionId, resume, model, cwd }) => {
  session = sessionId
  assert.equal(resume, false)
  assert.equal(model, 'opus')
  assert.match(system, /You are @Threepio/)
  assert.match(system, /six million languages/, "the bot's own instructions come with every run")
  assert.ok(cwd.startsWith(join(dataDir, 'bots', 'threepio-')) && existsSync(cwd), 'a folder of its own, under the data folder')
  assert.ok(prompt.startsWith(`[current item ID: ${id}]\n\n[new to you: Answer the PR]`))
  assert.match(prompt, /^Reply to the PR, please$/m, "the human's words as they are")
  assert.match(prompt, /^\[Assigned to @Threepio by human\]$/m)
  assert.doesNotMatch(prompt, /\d{4}-\d\d-\d\dT/, 'no timestamps')
  assert.ok(JSON.stringify(schema).includes('"after"'))
  assert.ok(JSON.stringify(schema).includes('"enum":["item"]'), 'a bot may add child items')
  assert.ok(!JSON.stringify(schema).includes('"enum":["agent"]'), 'a bot may not make bots')
  assert.equal(JSON.parse(await call('get_item', { item_id: id })).item.id, id)
  await assert.rejects(call('github_pr_comment', { repo: 'o/r', number: 7, body: 'x' }), /not found/i)
  // It has the same tools as anyone connected, so it may act on other items too, as itself.
  const other = (await engine.create('Elsewhere'))!
  await call('add_events', { item_id: other, events: [{ type: 'message', params: { text: 'from a bot' } }] })
  assert.equal(engine.children(other)[0].createdBy, `bot:${threepio}`)
  await engine.close(other)
  return out({
    item: id,
    message: 'Drafted a reply. Post it?',
    events: [
      { type: 'approval', ref: 'post', params: { action: 'github_pr_comment', inputs: { repo: 'o/r', number: 7, body: 'Thanks!' } } },
      { type: 'after', params: { event: 'post', then: ciThenRoute } },
    ],
  })
})
await toBot(id, 'Reply to the PR, please')
assert.deepEqual(engine.children(id).slice(0, 2).map((e) => [e.type, e.createdBy]), [['message', 'human'], ['assign', 'human']])
assert.deepEqual(item(id).assignees, [threepio])
assert.ok(!item(id).withHuman, "it is the bot's")
await settle()
assert.deepEqual(blockers(id), ['approval', 'after'])
assert.deepEqual(item(id).assignees, ['human'], 'a run that leaves the item waiting on the human hands it back')
assert.ok(item(id).withHuman, "an approval, and an after waiting on it, are the human's turn")
assert.equal(last(id, 'message').params.text, 'Drafted a reply. Post it?')
assert.equal(last(id, 'message').createdBy, `bot:${threepio}`)
assert.equal(engine.detail(id)!.runs.length, 1)
assert.equal(engine.detail(id)!.runs[0].agentId, threepio)

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

// ---- 4. CI fails: the after's script assigns the bot again, which resumes its session; it hands the item back while CI runs ----
behaviours.push(async ({ prompt, sessionId, resume }) => {
  assert.equal(resume, true)
  assert.equal(sessionId, session, 'one session for the bot, across its runs')
  assert.doesNotMatch(prompt, /new to you/)
  assert.match(prompt, /^\[o\/r#7: 1 of 2 checks failed: e2e\]$/m)
  assert.doesNotMatch(prompt, /: done\]/, 'an after that ran says nothing: what it added speaks for it')
  assert.match(prompt, /Fix the failing checks: e2e/)
  assert.doesNotMatch(prompt, /Reply to the PR, please/, 'what the session has seen is not sent again')
  checks = 'pending' // it pushed a fix, so CI runs again
  return out({ item: id, message: 'Fixed e2e.', events: ciThenRoute, assign: ['human'] })
})
checks = 'failed'
clock = item(id).blockers[0].nextCheckAt!
await tick()
assert.deepEqual(blockers(id), ['github', 'after'])
assert.deepEqual(item(id).assignees, ['human'], 'it said who has the item now')

// ---- 5. CI passes: the script adds nothing, so it is the human's turn, and no bot wakes ----
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
  assert.deepEqual(
    all.filter((e) => e.type === 'message').map((e) => [e.actor, e.message]),
    [
      ['human', 'Reply to the PR, please'],
      [`bot:${threepio}`, 'Drafted a reply. Post it?'],
      ['script:bot', 'Fix the failing checks: e2e'],
      [`bot:${threepio}`, 'Fixed e2e.'],
    ],
  )
  assert.equal(all.find((e) => e.type === 'assign')!.summary, 'Assigned to @Threepio')
}

// The trace has every moment: the bot's calls are traced in the bot, since a run may span items.
{
  const ci = engine.children(id).find((e) => e.type === 'github')!
  assert.equal(ci.times.checks, 7)
  assert.ok(ci.times.added && ci.times.resolved && ci.times.firstChecked! < ci.times.lastChecked!)
  const trace = engine.trace(id)
  assert.ok(trace.some((t) => t.kind === 'viewed'))
  assert.ok(trace.some((t) => t.kind === 'run_started' && t.actor === `bot:${threepio}`))
  assert.ok(engine.trace(threepio).some((t) => t.kind === 'action_called' && t.actor === `bot:${threepio}` && t.data!.action === 'get_item' && t.runId))
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
behaviours.push(async () => out({ item: chase, events: [{ type: 'slack', params: { channel: 'C1', message: 'Any update, Sam?' } }] }))
await toBot(chase, 'Ask Sam for an update')
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

// ---- 8. Items are events: a bot splits off a child item, which blocks its parent; the bot keeps the parent while it waits ----
const parent = (await engine.create('Parent'))!
behaviours.push(async () =>
  out({
    item: parent,
    events: [
      { type: 'item', ref: 'kid', params: { title: 'Child', events: [{ type: 'message', params: { text: 'Do the child' } }, { type: 'assign', params: { to: 'Threepio' } }] } },
      { type: 'after', params: { event: 'kid', then: [{ type: 'message', params: { text: 'Wrap up' } }] } },
    ],
  }),
)
behaviours.push(async ({ prompt, resume, sessionId }) => {
  assert.equal(resume, true)
  assert.equal(sessionId, session)
  assert.match(prompt, /\[new to you: Child\]/)
  assert.match(prompt, /Do the child/)
  assert.doesNotMatch(prompt, /Parent/, 'only what is new: the parent has nothing new')
  const kid = engine.children(parent).find((e) => e.type === 'item')!.id
  return out({ item: kid, message: 'Child done.', close: true })
})
behaviours.push(async ({ prompt }) => {
  assert.match(prompt, /\[item: Parent\]/)
  assert.match(prompt, /Wrap up/)
  assert.match(prompt, /\[Child: closed\]/, "a child closing is news for its parent, even when the bot closed it")
  return out({ item: parent, message: 'All done.', close: true })
})
await toBot(parent, 'Split it')
await settle()
const child = engine.children(parent).find((e) => e.type === 'item')!.id
assert.ok(item(child).closed)
assert.equal(item(child).parentId, parent)
assert.equal(engine.event(child)!.params.events, undefined, 'its first events are in it, not in its params')
assert.deepEqual(engine.children(child).map((e) => e.type), ['message', 'assign', 'message'])
assert.ok(item(parent).closed)

// A bot can also add an item that sits in no item, for unrelated work: it blocks nothing, and it is the human's.
{
  const host = (await engine.create('Host'))!
  behaviours.push(async () => out({ item: host, events: [{ type: 'item', params: { title: 'Unrelated', detached: true } }] }))
  await toBot(host, 'x')
  await settle()
  const loose = engine.overview().items.find((i) => i.title === 'Unrelated')!
  assert.equal(loose.parentId, null)
  assert.ok(loose.withHuman)
  assert.ok(item(host).withHuman, 'a detached item blocks nothing, and the run handed the host back')
  await engine.close(host)
  await engine.close(loose.id)
}

// ---- 9. A bot whose output is unusable leaves a message saying why, and hands the item back ----
const bad = (await engine.create('Bad'))!
behaviours.push(async () => out({ item: bad, events: [{ type: 'action', params: { name: 'github_pr_merge', inputs: {} } }] }))
await toBot(bad, 'x')
await settle()
assert.match(String(last(bad, 'message').params.text), /^@Threepio failed: The bot's output was not usable: .*may not add a action/)
assert.equal(last(bad, 'message').createdBy, 'system')
assert.ok(item(bad).withHuman)
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
behaviours.push(async () =>
  out({
    item: sneaky,
    assign: ['human'],
    events: [
      { type: 'github', ref: 'w', params: { query: 'nothing like this', until: 'merged' } },
      { type: 'after', params: { event: 'w', script: `async (i, actions) => { await actions.github_pr_merge({ repo: 'o/r', number: 7, method: 'squash' }); return null }` } },
    ],
  }),
)
await toBot(sneaky, 'x')
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
  behaviours.push(async () =>
    out({ item: counting, assign: ['human'], events: [{ type: 'script', params: { source: `async (ctx, a) => { await a.github_pr_merge({ repo: 'o/r', number: 7, method: 'squash' }); return { done: true } }`, schedule: [0] } }] }),
  )
  await toBot(counting, 'merge it')
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

// ---- 13. Deleting a bot while it runs: what it returns doesn't land, and its items go back to the human ----
const temp = await engine.saveAgent({ name: 'Temp', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 })
let release = () => {}
const held = new Promise<void>((r) => (release = r))
behaviours.push(async () => {
  await held
  return out({ item: taken, message: 'late', close: true })
})
const taken = (await engine.create('Take me'))!
await engine.send(taken, 'message', `[${temp}]() take this`)
assert.deepEqual(item(taken).assignees, [temp], 'an @ in a note hands the item to the bot')
while (!runner.busy().includes(temp)) await new Promise((r) => setTimeout(r, 5))
assert.equal(item(taken).bot, temp, 'the item shows which bot is on it')
await engine.cancel(temp)
release()
await settle()
assert.ok(engine.event(temp)!.cancelled)
assert.equal(engine.findAgent('Temp'), null)
assert.ok(!item(taken).closed, "a deleted bot's run doesn't land")
assert.deepEqual(item(taken).assignees, ['human'])
assert.ok(item(taken).withHuman)
await assert.rejects(engine.assign(taken, temp, true), /No bot/)

// ---- 13b. What happens while a bot runs wakes it again: one more run, told all of it ----
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
await engine.send(queue, 'message', 'two')
await engine.send(queue, 'message', 'three')
assert.equal(runner.busy().length, 1, 'a bot runs one thing at a time')
releaseFirst()
await settle()
assert.equal(prompts.length, 2, JSON.stringify(prompts))
assert.match(prompts[1], /two[\s\S]*three/)
assert.doesNotMatch(prompts[1], /\bone\b/)
assert.equal(last(queue, 'message').params.text, 'second')
assert.deepEqual(item(queue).assignees, ['human'], 'the second run, with nothing more coming in, hands it back')

// ---- 14. The unsafe rule: every action says which it is, and a bot's MCP offers none of the unsafe ones ----
assert.ok(engine.actions.every((a) => typeof a.unsafe === 'boolean'))
const probe = (await engine.create('Probe'))!
const runId = engine.startRun(threepio, [probe], 'probe')
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
// Bots, by name.
const bots = JSON.parse(await outside.call('list_bots'))
assert.deepEqual(bots.map((b: { name: string }) => b.name), ['Threepio'])
await outside.call('add_events', { item_id: probe, events: [{ type: 'assign', params: { to: 'THREEPIO', on: false } }] })
await assert.rejects(outside.call('add_events', { item_id: probe, events: [{ type: 'assign', params: { to: 'Nobody' } }] }), /No bot Nobody/)
await assert.rejects(outside.call('add_events', { events: [{ type: 'agent', params: { name: 'Sneaky' } }] }), /may not add a agent/)
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
assert.match((await engine.transcript(engine.detail(id)!.runs[0].id))!, /Thinking out loud/)

// ---- 17. Integrations and the catalog ----
const admin = new Admin(engine, mcp, fakeFetch)
assert.equal((await admin.integrations()).github.detail, 'Signed in as octocat')
assert.equal((await admin.integrations()).claude.bots, 1)
await assert.rejects(admin.addSlack('nope'), /xoxp-/)
await assert.rejects(admin.addSlack('xoxp-bad'), /invalid_auth/)
await admin.addSlack('xoxp-good-1234')
const cat = catalog(engine)
assert.ok(cat.actions.find((a) => a.name === 'slack_post')!.unsafe)
assert.deepEqual(cat.presets.map((p) => p.id), ['message', 'pr-merged', 'pr-checks'])
// A loop: a script that adds a message on every run and never finishes, until it is cancelled. The bot that has the item wakes each time.
{
  const looping = (await engine.create('Looping'))!
  await engine.assign(looping, threepio, true)
  const [loop] = await engine.addEvents(looping, [
    {
      type: 'script',
      params: {
        description: 'Every hour, a nudge',
        schedule: [3600],
        source: `async (ctx, actions) => { await actions.add_events({ item_id: ctx.item.id, events: [{ type: 'message', params: { text: 'Go round again' } }] }); return { done: false } }`,
      },
    },
  ])
  // Assigned with nothing to do yet: its first run keeps it, saying so.
  behaviours.push(async () => out({ item: looping, assign: ['Threepio'] }))
  await settle()
  for (let round = 0; round < 2; round++) {
    behaviours.push(async ({ prompt }) => {
      assert.match(prompt, /\[script: Go round again\]/)
      return out({ item: looping, assign: ['Threepio'] })
    })
    clock = engine.event(loop)!.nextCheckAt!
    await tick()
    await settle()
  }
  assert.equal(engine.children(looping).filter((e) => e.type === 'message').length, 2)
  assert.ok(!engine.event(loop)!.resolved, 'a loop runs until it is cancelled')
  await engine.cancel(loop)
  await engine.close(looping)
}
// Summarising: a summary event, answered by a cheap model with no tools, in ASD-STE100; said in the chat.
{
  const s = (await engine.create('Summed up'))!
  await engine.send(s, 'message', 'The plan is to ship on Friday')
  await engine.summarise(s)
  clock += 1000
  await tick()
  const sum = last(s, 'summary')
  assert.ok(sum.resolved)
  assert.equal(sum.message, '- It is about the PR.')
  assert.equal(summaryArgs[summaryArgs.indexOf('--model') + 1], 'haiku')
  assert.equal(summaryArgs[summaryArgs.indexOf('--tools') + 1], '')
  assert.match(summaryArgs[summaryArgs.indexOf('--system-prompt') + 1], /ASD-STE100/)
  assert.match(summaryArgs[1], /Task: Summed up/)
  assert.match(summaryArgs[1], /ship on Friday/)
  assert.ok(item(s).withHuman, "once written, it is the human's turn again")
  await engine.close(s)
}
// Custom actions: defined per event type, run on an event with its context and the safe actions, said in the chat.
{
  const ca = engine.customActions
  const task = (await engine.create('Custom'))!
  assert.throws(() => ca.save({ type: 'nope', name: 'x', description: '', notes: '', source: '', icon: 'zap', important: false }), /No event type/)
  const hello = ca.save({ type: 'item', name: 'Hello', description: 'Says hello', notes: '', source: '', icon: 'zap', important: true })
  await assert.rejects(ca.run(hello.id, task), /no code yet/)
  // Its code is written by a model from the description and notes, told the actions it may call.
  const source = await ca.generate({ type: 'item', name: 'Hello', description: 'Says hello to the task', notes: 'Use its title' })
  assert.equal(source, 'async (context) => `Hello ${context.item.title}`')
  assert.match(generateArgs[1], /Says hello to the task/)
  assert.match(generateArgs[1], /Use its title/)
  const system = generateArgs[generateArgs.indexOf('--system-prompt') + 1]
  assert.match(system, /add_events/)
  assert.doesNotMatch(system.split('The unsafe actions are:')[0], /github_pr_merge/, 'it is offered no unsafe actions to call')
  ca.save({ ...hello, source })
  await ca.run(hello.id, task)
  assert.equal(last(task, 'action').message, 'Hello Custom')
  // It may add events, and call actions; an unsafe one is refused.
  const flagger = ca.save({
    type: 'message', name: 'Flag it', description: '', notes: '', icon: 'flag', important: false,
    source: `async (context, actions) => { const all = await actions.list_items({}); return { message: 'Flagged ' + context.event.summary, events: [{ type: 'priority', params: { on: true } }] } }`,
  })
  await engine.send(task, 'message', 'This one')
  const said = last(task, 'message').id
  await assert.rejects(ca.run(flagger.id, task), /for message events/)
  await ca.run(flagger.id, said)
  assert.equal(last(task, 'action').message, 'Flagged This one')
  assert.ok(item(task).priority)
  const sneaky2 = ca.save({ type: 'item', name: 'Merge', description: '', notes: '', icon: 'zap', important: false, source: `async (c, actions) => actions.github_pr_merge({ repo: 'o/r', number: 7, method: 'squash' })` })
  await assert.rejects(ca.run(sneaky2.id, task), /unsafe/)
  assert.ok(engine.trace(task).some((t) => t.kind === 'custom_action_run'))
  assert.deepEqual(ca.list().filter((c) => c.type === 'item').map((c) => c.name), ['Hello', 'Merge'], 'important first')
  ca.remove(sneaky2.id)
  assert.equal(ca.get(sneaky2.id), null)
  await engine.close(task)
}
// Renaming: a rename event, whose title the item then reads by, everywhere.
{
  const r = (await engine.create('First name'))!
  await engine.rename(r, '  Second name ')
  assert.equal(item(r).title, 'Second name')
  assert.equal(engine.event(r)!.summary, 'Second name')
  assert.equal(last(r, 'rename').message, 'Renamed to “Second name”')
  await engine.rename(r, 'Second name')
  assert.equal(engine.children(r).filter((e) => e.type === 'rename').length, 1, 'the same title adds nothing')
  await engine.close(r)
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
// The default preset only says what is typed; "assign to @bot", sent empty, hands the item to the bot.
{
  const quiet = (await engine.create('Quiet'))!
  await engine.send(quiet, 'message', 'Just noting this')
  assert.deepEqual(engine.children(quiet).map((e) => e.type), ['message'])
  assert.ok(item(quiet).withHuman, 'a note alone leaves it with the human')
  await assert.rejects(engine.send(quiet, 'message', '  '), /Nothing to send/)
  await engine.assign(quiet, 'human', true)
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /\[new to you: Quiet\]/)
    assert.match(prompt, /^Just noting this$/m, 'the bot gets the whole history the first time it sees an item')
    return out({ item: quiet, message: 'Noted.' })
  })
  await engine.send(quiet, `assign:${threepio}`, '')
  assert.deepEqual(engine.children(quiet).map((e) => [e.type, e.params.to, e.params.on]), [
    ['message', undefined, undefined],
    ['assign', 'human', true],
    ['assign', threepio, true],
    ['assign', 'human', false],
  ], 'handing it to the bot takes it off the human')
  await settle()
  assert.deepEqual(item(quiet).assignees, ['human'])
  // Already the bot's, sent empty: it is woken anyway.
  await engine.assign(quiet, threepio, true)
  behaviours.push(async () => out({ item: quiet, assign: ['human', 'Threepio'] }))
  await settle()
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /\[nothing new\]/)
    return out({ item: quiet, assign: ['human'] })
  })
  await engine.send(quiet, `assign:${threepio}`, '')
  await settle()
  assert.deepEqual(item(quiet).assignees, ['human'])
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
// The finder's recent sections: the human's attaching links both items; having one on screen puts it first in viewed.
assert.deepEqual(engine.overview().linked.slice(0, 2), [collector, idea])
engine.viewed(collector)
assert.equal(engine.overview().viewed[0], collector)
// A new item suggests the human's recent attachments: what went where. The bot's is left out.
assert.deepEqual(engine.overview().attachments.slice(0, 1), [{ event: idea, title: 'An idea', item: idea, target: collector }])
assert.ok(!engine.overview().attachments.some((a) => a.event === said))
await assert.rejects(engine.attach(collector, collector), /itself/)
await assert.rejects(engine.attach(idea, said), /No item/)
await engine.close(collector)
await assert.rejects(engine.attach(idea, collector), /closed/)

// ---- 19. Directives sit in no item, hold until deleted, and agents read and add them ----
{
  const bot = { engine, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }
  const first = await engine.addDirective('# Shipping the release\nNothing else this week.')
  clock += 1000
  const host = (await engine.create('Feedback'))!
  // A bot adding one in its own item puts it in none, so it blocks nothing.
  const [second] = await engine.addEvents(host, [{ type: 'directive', params: { text: 'Keep PRs small' } }], 'bot', 'bot')
  assert.equal(engine.event(second)!.parentId, null)
  assert.ok(item(host).withHuman, 'a directive blocks nothing')
  assert.equal(engine.event(first)!.summary, 'Shipping the release')
  const listed = (await engine.callAction('directives', {}, bot)) as { id: string; text: string; createdBy: string }[]
  assert.deepEqual(listed.map((d) => [d.id, d.createdBy]), [[second, 'bot'], [first, 'human']], 'the newest first')
  assert.deepEqual(await engine.callAction('list_items', { type: 'directive' }, bot), listed, 'directives is list_items for the type')
  const { ids: [third] } = (await engine.callAction('add_events', { events: [{ type: 'directive', params: { text: 'From feedback' } }] }, bot)) as { ids: string[] }
  assert.equal(engine.directives()[0].id, third)
  await assert.rejects(engine.addDirective('  '), /needs some text/)
  // Deleting one cancels it: it is kept, and no longer listed.
  await engine.cancel(second)
  assert.deepEqual(engine.directives().map((d) => d.id), [third, first])
  assert.ok(engine.event(second)!.cancelled)
  // Editing one replaces it: the new one is added, the old one cancelled, pointing at it.
  clock += 1000
  const edited = await engine.editDirective(first, 'Shipping the release, then the docs')
  assert.deepEqual(engine.directives().map((d) => d.id), [edited, third])
  assert.deepEqual(engine.event(first)!.data, { cancelled: true, replacedBy: edited })
  await assert.rejects(engine.editDirective(first, 'again'), /No directive .* holds/)
  // A bot edits one the same way, through add_events.
  const { ids: [byBot] } = (await engine.callAction('add_events', { events: [{ type: 'directive', params: { text: 'Keep PRs tiny', replaces: third } }] }, bot)) as { ids: string[] }
  assert.deepEqual(engine.directives().map((d) => d.id), [byBot, edited])
  assert.throws(() => engine.add(null, [{ type: 'message', params: { text: 'x' } }], 'human'), /Only an item, a bot or a directive/)
  for (const d of [byBot, edited]) await engine.cancel(d)
  await engine.close(host)
}

// ---- 20. A snooze puts a task away until its time, checked every minute; waking it cancels the snooze ----
{
  for (const i of engine.overview().items.filter((i) => !i.closed)) await engine.close(i.id)
  const sleepy = (await engine.create('Sleepy'))!
  await engine.snooze(sleepy, clock + 3 * MINUTE)
  await tick()
  assert.ok(!item(sleepy).withHuman, "a snoozed task is not the human's turn")
  assert.equal(item(sleepy).snoozedUntil, clock + 3 * MINUTE)
  assert.equal(engine.next(), null)
  const [z] = item(sleepy).blockers
  assert.equal(z.type, 'snooze')
  for (let i = 0; i < 3; i++) {
    assert.ok(!engine.event(z.id)!.resolved, 'still asleep')
    assert.equal(engine.event(z.id)!.nextCheckAt! - clock, MINUTE, 'checked every minute')
    clock = engine.event(z.id)!.nextCheckAt!
    await tick()
  }
  assert.ok(engine.event(z.id)!.resolved, 'awake once its time comes')
  assert.equal(engine.event(z.id)!.message, 'Woke up')
  assert.ok(item(sleepy).withHuman)
  assert.equal(engine.next(), sleepy)
  // Snoozing again replaces the snooze; waking it cancels it at once.
  await engine.snooze(sleepy, clock + 60 * MINUTE)
  await engine.snooze(sleepy, clock + 120 * MINUTE)
  assert.equal(item(sleepy).blockers.length, 1)
  assert.equal(item(sleepy).snoozedUntil, clock + 120 * MINUTE)
  await engine.unsnooze(sleepy)
  assert.ok(item(sleepy).withHuman)
  assert.equal(item(sleepy).snoozedUntil, null)
  await assert.rejects(engine.snooze(sleepy, clock - 1), /to come/)
  // A bot may snooze its item too, with an ISO time; anything else is refused.
  await engine.addEvents(sleepy, [{ type: 'snooze', params: { until: new Date(clock + MINUTE).toISOString() } }], 'bot', 'bot')
  assert.ok(!item(sleepy).withHuman)
  await assert.rejects(engine.addEvents(sleepy, [{ type: 'snooze', params: { until: 'soon' } }], 'bot', 'bot'))
  await engine.close(sleepy)
}

// ---- 21. Bots: settings as configure events, a debounce that gathers a burst of work into one run, watching, waking by hand ----
{
  for (const i of engine.overview().items.filter((i) => !i.closed)) await engine.close(i.id)
  await assert.rejects(engine.saveAgent({ name: 'threepio', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 }), /already/)
  await assert.rejects(engine.saveAgent({ name: 'two words', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 }), /name of letters/)
  await assert.rejects(engine.saveAgent({ name: 'human', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 }), /stands for you/)
  const r2 = await engine.saveAgent({ name: 'R2', instructions: 'Beep.', provider: 'claude', model: '', debounceMinutes: 0 })
  await engine.saveAgent({ id: r2, name: 'R2', instructions: 'Beep boop.', provider: 'claude', model: 'haiku', debounceMinutes: 5 })
  assert.deepEqual(engine.children(r2).map((e) => [e.type, Object.keys(e.params).sort().join()]), [['configure', 'debounce_minutes,instructions,model']], 'only what changed')
  const bot = engine.agent(r2)!
  assert.deepEqual([bot.instructions, bot.model, bot.debounceMinutes], ['Beep boop.', 'haiku', 5])
  assert.equal(engine.event(r2)!.summary, '@R2')

  // A burst of work for a bot with a debounce: one run, for all of it, once the last of it is 5 minutes old.
  const a = (await engine.create('A'))!
  const b = (await engine.create('B'))!
  await engine.send(a, 'message', `[${r2}]() this`)
  clock += 2 * MINUTE
  await engine.send(b, 'message', `and [${r2}]() this`)
  await settle()
  assert.equal(engine.agent(r2)!.wakesAt, clock + 5 * MINUTE)
  assert.deepEqual(engine.agent(r2)!.assigned, [a, b])
  clock += 5 * MINUTE - 1
  await settle()
  assert.equal(engine.agent(r2)!.runId, null, 'not yet')
  const seen: string[] = []
  behaviours.push(async ({ prompt }) => {
    seen.push(prompt)
    return out({ item: a, message: 'Did A.' }, { item: b, message: 'Did B.', close: true })
  })
  clock += 1
  await settle()
  assert.equal(seen.length, 1)
  assert.match(seen[0], new RegExp(`current item ID: ${a}[\\s\\S]*---[\\s\\S]*current item ID: ${b}`), 'both items, in one prompt')
  assert.ok(item(b).closed)
  assert.deepEqual(item(a).assignees, ['human'])

  // Watching: a bot wakes for an item it watches, without having it; the item stays the human's.
  await engine.addEvents(a, [{ type: 'watch', params: { agent: 'R2' } }])
  assert.deepEqual(engine.agent(r2)!.watching, [a])
  await engine.send(a, 'message', 'FYI')
  // Woken by hand, it runs now, whatever its debounce says.
  engine.wakeAgent(r2)
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /^FYI$/m)
    return out()
  })
  await settle()
  assert.deepEqual(item(a).assignees, ['human'])
  assert.ok(item(a).withHuman)

  // Recently assigned, by assignee; and what the human may have lost track of.
  assert.deepEqual(engine.assignments(r2, 1).map((x) => [x.itemId, x.on]), [[a, false], [b, true], [a, true]].map(([i, on]) => [i, on]).slice(0, 3))
  clock += 30 * 60 * MINUTE
  const stale = (await engine.create('Recent'))!
  assert.deepEqual(engine.stale(24).map((i) => i.id), [a])
  assert.ok(!engine.stale(24).some((i) => i.id === stale))

  // An old bot event (a script from before bots were their own items) is an assign to the default bot.
  await engine.addEvents(stale, [{ type: 'bot', params: { instructions: 'Old habits' } }])
  assert.deepEqual(engine.children(stale).map((e) => e.type), ['message', 'assign'])
  assert.equal(item(stale).assignees[0], threepio)
  behaviours.push(async ({ prompt }) => {
    assert.match(prompt, /Old habits/)
    return out({ item: stale, close: true })
  })
  await settle()

  // Deleting a bot hands what it had back to the human.
  await engine.assign(a, r2, true)
  await engine.deleteAgent(r2)
  assert.deepEqual(item(a).assignees, ['human'])
  assert.equal(engine.agents().length, 1)
  await engine.close(a)
}

// The prompt, item by item.
assert.equal(promptFor([]), '')

assert.equal(behaviours.length, 0)
mcp.close()
console.log('engine: ok')
