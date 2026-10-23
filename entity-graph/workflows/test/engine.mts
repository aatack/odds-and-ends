// The engine, the MCP server and the worker pool, end to end, against an
// in-memory node:sqlite store, stand-ins for gh and Slack, and stand-in bots
// that call the real MCP server over HTTP and return structured output.

import assert from 'node:assert/strict'
import { DatabaseSync } from 'node:sqlite'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import { Admin } from '../src/engine/admin.ts'
import { Engine, type BotOutput } from '../src/engine/engine.ts'
import { catalog, startMcp } from '../src/engine/mcp.ts'
import { botOutputSchema } from '../src/engine/schema.ts'
import { WorkerPool, type SpawnBot } from '../src/engine/workers.ts'

const MINUTE = 60_000
let clock = 1_000_000

// ---- gh ----
const ghCalls: string[][] = []
let checks: 'pending' | 'failed' | 'passed' = 'pending'
const exec = async (cmd: string, args: string[]) => {
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
  if (args[0] === 'pr' && ['comment', 'merge'].includes(args[1])) return 'https://github.com/o/r/pull/7\n'
  if (args[0] === 'search')
    return JSON.stringify([{ number: 7, title: 'Fix it', url: 'https://github.com/o/r/pull/7', repository: { nameWithOwner: 'o/r' } }])
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
type Behaviour = (call: Call, prompt: string, schema: Record<string, unknown>) => Promise<BotOutput | null>
const behaviours: Behaviour[] = []
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
const spawnFake: SpawnBot = ({ prompt, schema, mcpUrl, onOutput }) => {
  const behaviour = behaviours.shift()
  assert.ok(behaviour, `a bot ran that no test expected:\n${prompt}`)
  const done = (async () => {
    const { client, call } = await connect(mcpUrl)
    const out = await behaviour(call, prompt, schema)
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
    if (pool.status().every((w) => w.itemId === null) && !engine.overview().items.some((i) => i.current?.type === 'bot')) {
      await engine.idle()
      return
    }
  }
  throw new Error(`still busy: ${JSON.stringify(pool.status())}`)
}
const current = (id: string) => engine.get(id)!.current
const tick = async () => {
  await engine.tick()
  await settle()
}

// The script a bot leaves behind a CI wait: fix on failure, ask to merge on success.
const route = `async ({ previous }) => previous.resolution.outcome === 'failed'
  ? { type: 'bot', params: { instructions: 'Fix the failing checks: ' + previous.resolution.failed.join(', ') } }
  : { type: 'human', params: { needed: 'Merge o/r#7?', approval: { action: 'github_pr_merge', inputs: { repo: 'o/r', number: 7, method: 'squash' } } } }`

// ---- 1. A new item goes to the human, and Next finds it ----
const id = (await engine.create('Answer the PR'))!
assert.equal(current(id)!.type, 'human')
assert.equal(engine.next(), id)
assert.equal(engine.next([id]), null)

// ---- 2. Human → bot. The bot has no unsafe actions and can't hand off itself; it returns a chain ----
behaviours.push(async (call, prompt, schema) => {
  assert.match(prompt, /Your instructions: Reply on the PR/)
  assert.ok(JSON.stringify(schema).includes('"needed"'))
  const item = JSON.parse(await call('get_item', { item_id: id }))
  assert.equal(item.item.id, id)
  await assert.rejects(call('github_pr_comment', { repo: 'o/r', number: 7, body: 'x' }), /not found/i)
  const unsafe = JSON.parse(await call('unsafe_actions')).map((a: { name: string }) => a.name)
  assert.deepEqual(unsafe.sort(), ['github_pr_comment', 'github_pr_merge', 'github_pr_review', 'slack_post'])
  await assert.rejects(call('hand_off', { item_id: id, next: [] }), /returns its next handoffs/)
  await assert.rejects(call('set_state', { item_id: 'someone-else', state: 'x' }), /only change the item/)
  await call('note', { item_id: id, text: 'Read the thread.' })
  return {
    state: 'Reply drafted.',
    next: [
      { type: 'human', params: { needed: 'Post this reply?', approval: { action: 'github_pr_comment', inputs: { repo: 'o/r', number: 7, body: 'Thanks!' } } } },
      { type: 'github', params: { repo: 'o/r', number: 7, until: 'checks' } },
      { type: 'script', params: { source: route, description: 'Fix on failure, merge on success' } },
    ],
  }
})
await engine.handOff(id, { next: [{ type: 'bot', params: { instructions: 'Reply on the PR' } }] })
await settle()
assert.equal(engine.get(id)!.state, 'Reply drafted.')
assert.equal(current(id)!.type, 'human')
assert.equal(current(id)!.params.needed, 'Post this reply?')
assert.equal(engine.get(id)!.queued, 2)
const botDone = engine.handoffs(id).find((h) => h.type === 'bot')!
assert.equal(botDone.resolution!.output, 'Thinking out loud.')
assert.equal(botDone.resolution!.state, 'Reply drafted.')
assert.equal(botDone.stateAtStart, '')

// ---- 3. The human changes the reply and approves it; the chain goes on to the CI wait ----
await engine.approve(id, { repo: 'o/r', number: 7, body: 'Thanks a lot!' })
assert.deepEqual(ghCalls.at(-1), ['pr', 'comment', '7', '-R', 'o/r', '--body', 'Thanks a lot!'])
const approved = engine.handoffs(id).find((h) => h.resolution?.decision === 'approved')!
assert.deepEqual((approved.resolution!.action as { changed: string[] }).changed, ['body'])
assert.equal(current(id)!.type, 'github')
assert.equal(current(id)!.stateAtStart, 'Reply drafted.')

// The wait is checked at once, then backs off: 1, 2, 5, 10, 10 minutes.
await tick()
for (const minutes of [1, 2, 5, 10, 10]) {
  const due = current(id)!.nextCheckAt!
  assert.equal(due, clock + minutes * MINUTE)
  clock = due - 1
  await tick()
  assert.equal(current(id)!.nextCheckAt, due, 'not checked before it is due')
  clock = due
  await tick()
}

// ---- 4. Checks fail: the script hands to a bot, which fixes and chains the same again ----
behaviours.push(async (_call, prompt) => {
  assert.match(prompt, /Fix the failing checks: e2e/)
  assert.match(prompt, /"outcome":"failed"/)
  return {
    state: 'Fixed e2e; CI running again.',
    next: [
      { type: 'github', params: { repo: 'o/r', number: 7, until: 'checks' } },
      { type: 'script', params: { source: route } },
    ],
  }
})
checks = 'failed'
clock = current(id)!.nextCheckAt!
await tick()
assert.equal(current(id)!.type, 'github')
assert.equal(engine.get(id)!.state, 'Fixed e2e; CI running again.')

// ---- 5. Checks pass: the script asks the human to merge; after that, the queue is empty ----
checks = 'passed'
await tick()
assert.equal(current(id)!.type, 'human')
assert.equal(current(id)!.params.needed, 'Merge o/r#7?')
await engine.approve(id, { repo: 'o/r', number: 7, method: 'squash' })
assert.deepEqual(ghCalls.at(-1), ['pr', 'merge', '7', '-R', 'o/r', '--squash'])
assert.match(String(current(id)!.params.needed), /approved\. What next\?/)
await engine.close(id, { state: 'Merged.' })
assert.ok(engine.get(id)!.closed)
assert.equal(current(id), null)
assert.deepEqual(
  engine.handoffs(id).map((h) => h.type),
  ['human', 'bot', 'human', 'github', 'script', 'bot', 'github', 'script', 'human', 'human'],
)

// ---- 6. Slack: a bot's message goes to the human first, then the wait takes its thread ----
engine.settings.addSlackWorkspace({ name: 'Acme', team: 'Acme', user: 'alex', userId: 'UME', token: 'xoxp-good-1234' })
behaviours.push(async () => ({ state: 'Asking Sam.', next: [{ type: 'slack', params: { channel: 'C1', message: 'Any update, Sam?' } }] }))
const chase = (await engine.create('Chase Sam', { next: [{ type: 'bot', params: { instructions: 'Ask Sam for an update' } }] }))!
await settle()
assert.equal(current(chase)!.type, 'human')
assert.equal((current(chase)!.params.approval as { action: string }).action, 'slack_post')
assert.equal(slackCalls.filter((c) => c.method === 'chat.postMessage').length, 0)
await engine.approve(chase, { channel: 'C1', text: 'Any update, Sam?' })
assert.equal(current(chase)!.type, 'slack')
assert.equal(current(chase)!.params.thread_ts, '100.1')
await tick()
assert.equal(current(chase)!.type, 'slack', 'my own message is not a reply')
thread.push({ ts: '100.2', user: 'USAM', text: 'Done!' })
clock = current(chase)!.nextCheckAt!
await tick()
assert.match(String(current(chase)!.params.needed), /1 reply\(s\) on Slack/)
const replied = engine.handoffs(chase).find((h) => h.type === 'slack')!
assert.deepEqual((replied.resolution!.replies as { text: string }[]).map((r) => r.text), ['Done!'])
await engine.close(chase, {})

// ---- 7. Items: a bot splits off a sub-item and waits on it, then wraps up ----
let child = ''
behaviours.push(async (call) => {
  child = JSON.parse(await call('create_item', { title: 'Child', next: [{ type: 'bot', params: { instructions: 'Do the child' } }] })).id
  return { state: 'Waiting on the child.', next: [{ type: 'items', params: { item_ids: [child] } }, { type: 'bot', params: { instructions: 'Wrap up' } }] }
})
behaviours.push(async () => ({ state: 'Child done.', next: [], close: true }))
behaviours.push(async (_call, prompt) => {
  assert.match(prompt, /Child done/)
  return { state: 'All done.', next: [], close: true }
})
const parent = (await engine.create('Parent', { next: [{ type: 'bot', params: { instructions: 'Split it' } }] }))!
await settle()
await settle()
assert.ok(engine.get(child)!.closed)
assert.ok(engine.get(parent)!.closed)
assert.equal(engine.get(parent)!.state, 'All done.')

// ---- 8. A bot whose output is unusable hands the item to the human, saying why ----
behaviours.push(async () => ({ state: 's', next: [] }))
const bad = (await engine.create('Bad', { next: [{ type: 'bot', params: { instructions: 'x' } }] }))!
await settle()
assert.match(String(current(bad)!.params.needed), /no handoffs and did not close/)
behaviours.push(async () => ({ state: 's', next: [{ type: 'human', params: { question: 'wrong field' } }] }))
const typo = (await engine.create('Typo', { next: [{ type: 'bot', params: { instructions: 'x' } }] }))!
await settle()
assert.match(String(current(typo)!.params.needed), /needed: Required/)
await engine.handledOutside(typo, 'Did it by hand')

// ---- 9. Scripts reach the actions; a bot's script gets no unsafe ones ----
const scripted = (await engine.create('Scripted', {
  next: [
    {
      type: 'script',
      params: {
        source: `async (input, actions) => { const all = await actions.list_items({}); console.log('saw', all.length); return { type: 'human', params: { needed: 'Saw ' + all.length + ' items' } } }`,
      },
    },
  ],
}))!
assert.match(String(current(scripted)!.params.needed), /^Saw \d+ items$/)
assert.equal((engine.handoffs(scripted)[0].resolution!.logs as string[])[0].split(' ')[0], 'saw')
const merges = ghCalls.filter((c) => c[1] === 'merge').length
behaviours.push(async () => ({
  state: 's',
  next: [{ type: 'script', params: { source: `async (i, actions) => { await actions.github_pr_merge({ repo: 'o/r', number: 7, method: 'squash' }); return null }` } }],
}))
const sneaky = (await engine.create('Sneaky', { next: [{ type: 'bot', params: { instructions: 'x' } }] }))!
await settle()
assert.equal(ghCalls.filter((c) => c[1] === 'merge').length, merges)
assert.match(String(current(sneaky)!.params.needed), /script handoff failed: .*unsafe/)

// ---- 10. A wait with a timeout resolves as timed_out ----
checks = 'pending'
const slow = (await engine.create('Slow', { next: [{ type: 'github', params: { repo: 'o/r', number: 7, until: 'checks', timeout_minutes: 3 } }] }))!
await tick()
clock += 3 * MINUTE
await tick()
assert.match(String(current(slow)!.params.needed), /Nothing after 3 minutes/)

// ---- 11. Sources create items once ----
await engine.pollSources(true)
await engine.pollSources(true)
assert.equal(engine.overview().items.filter((i) => i.source === 'github.review-requests').length, 1)

// ---- 12. Taking over a bot's item brings it back to the human ----
let release = () => {}
const held = new Promise<void>((r) => (release = r))
behaviours.push(async () => {
  await held
  return { state: 'late', next: [], close: true }
})
const taken = (await engine.create('Take me', { next: [{ type: 'bot', params: { instructions: 'x' } }] }))!
while (!pool.status().some((w) => w.itemId === taken)) await new Promise((r) => setTimeout(r, 5))
await engine.takeOver(taken)
release()
await settle()
assert.equal(current(taken)!.type, 'human')
assert.equal(engine.get(taken)!.closed, false, "a run that was taken over doesn't land")

// ---- 13. The rule on unsafe actions: every action says which it is, and a bot's MCP offers none of the unsafe ones ----
assert.ok(engine.actions.every((a) => typeof a.unsafe === 'boolean'))
const runId = engine.startRun(engine.get(taken)!, engine.get(taken)!.current!, 9, 'probe')
const asBot = await connect(mcp.runUrl(runId))
const botTools = (await asBot.client.listTools()).tools.map((t) => t.name)
assert.deepEqual(botTools, engine.actions.filter((a) => !a.unsafe).map((a) => a.name))
await asBot.client.close()
await engine.endRun(runId, { exitCode: 0, error: 'probe' })
for (const a of engine.actions.filter((a) => a.unsafe))
  await assert.rejects(engine.callAction(a.name, {}, { engine, author: 'bot', actor: 'script', itemId: taken, runId: null }), /unsafe/)

// ---- 14. The operator's endpoint: every action, as the human ----
const { url } = mcp.operator()
await assert.rejects(connect(url, { Authorization: 'Bearer wrong' }), (e: { code?: number }) => e.code === 401)
const outside = await connect(url, { Authorization: `Bearer ${engine.settings.mcpToken()}` })
const names = (await outside.client.listTools()).tools.map((t) => t.name)
assert.deepEqual(names, engine.actions.map((a) => a.name))
await outside.call('set_state', { item_id: taken, state: 'Set from outside.' })
await outside.call('hand_off', { item_id: taken, next: [{ type: 'human', params: { needed: 'Still there?' } }] })
assert.equal(current(taken)!.params.needed, 'Still there?')
assert.match(await outside.call('github_pr_comment', { repo: 'o/r', number: 7, body: 'From outside' }), /pull\/7/)
const types = JSON.parse(await outside.call('handoff_types'))
assert.deepEqual(types.map((t: { type: string }) => t.type), ['human', 'bot', 'github', 'slack', 'items', 'script'])
await outside.client.close()

// ---- 15. The history is there to look back on ----
const recent = engine.recent(7)
const first = recent.find((r) => r.id === id)!
assert.equal(first.botRuns, 2)
assert.equal(first.approvalsChanged, 1)
assert.ok(first.minutesOn.github >= 28, JSON.stringify(first.minutesOn))
assert.deepEqual(recent.find((r) => r.id === typo)!.handledOutside, ['Did it by hand'])
assert.equal(recent.find((r) => r.id === typo)!.botFailures, 1)
assert.equal(recent.find((r) => r.id === slow)!.timedOut, 1)
assert.match(engine.transcript(engine.detail(id)!.runs[0].id)!, /Thinking out loud/)

// ---- 16. Integrations and the catalog ----
const admin = new Admin(engine, mcp, 2, fakeFetch)
assert.equal((await admin.integrations()).github.detail, 'Signed in as octocat')
await assert.rejects(admin.addSlack('', 'nope'), /xoxp-/)
await assert.rejects(admin.addSlack('', 'xoxp-bad'), /invalid_auth/)
const cat = catalog(engine)
assert.ok(cat.actions.find((a) => a.name === 'slack_post')!.unsafe)
assert.ok(!cat.actions.find((a) => a.name === 'slack_thread')!.unsafe)
assert.ok(cat.handoffs.find((h) => h.type === 'human')!.params.required!.includes('needed'))
assert.equal((botOutputSchema(engine.handoffTypes).required as string[]).join(), 'state,next')

assert.equal(behaviours.length, 0)
mcp.close()
console.log('engine: ok')
