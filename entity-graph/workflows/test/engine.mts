// The engine, the MCP server and the worker pool, end to end, against an
// in-memory node:sqlite store, a stand-in for gh and stand-in agents that call
// the real MCP server over HTTP.

import assert from 'node:assert/strict'
import { DatabaseSync } from 'node:sqlite'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import { BACKOFF_MINUTES, Engine, defaultRegistry } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { WorkerPool, type SpawnAgent } from '../src/engine/workers.ts'

const MINUTE = 60_000
let clock = 1_000_000
const ghCalls: string[][] = []
let checksPending = true
const exec = async (cmd: string, args: string[]) => {
  assert.equal(cmd, 'gh')
  ghCalls.push(args)
  if (args[0] === 'pr' && args[1] === 'view' && args.includes('statusCheckRollup'))
    return JSON.stringify({
      statusCheckRollup: [{ __typename: 'CheckRun', name: 'ci', status: checksPending ? 'IN_PROGRESS' : 'COMPLETED', conclusion: checksPending ? '' : 'SUCCESS' }],
    })
  if (args[0] === 'pr' && args[1] === 'comment') return 'https://github.com/o/r/pull/7#comment\n'
  if (args[0] === 'search')
    return JSON.stringify([{ number: 7, title: 'Fix it', url: 'https://github.com/o/r/pull/7', repository: { nameWithOwner: 'o/r' } }])
  throw new Error(`unexpected gh ${args.join(' ')}`)
}

const engine = new Engine(new DatabaseSync(':memory:'), defaultRegistry, exec, () => clock)
const mcp = await startMcp(engine)

// Each agent run takes the next behaviour off this queue and plays it over MCP.
type Call = (name: string, args?: Record<string, unknown>) => Promise<string>
const behaviours: ((call: Call, prompt: string) => Promise<void>)[] = []
const prompts: string[] = []
const spawnFake: SpawnAgent = ({ prompt, mcpUrl, onOutput }) => {
  prompts.push(prompt)
  const behaviour = behaviours.shift()
  assert.ok(behaviour, 'an agent ran that no test expected')
  const done = (async () => {
    const client = new Client({ name: 'fake-agent', version: '0' })
    await client.connect(new StreamableHTTPClientTransport(new URL(mcpUrl)))
    const call: Call = async (name, args = {}) => {
      const r = (await client.callTool({ name, arguments: args })) as { content: { text: string }[]; isError?: boolean }
      if (r.isError) throw new Error(r.content[0].text)
      return r.content[0].text
    }
    await behaviour(call, prompt)
    await client.close()
    onOutput(JSON.stringify({ type: 'result', total_cost_usd: 0.01, num_turns: 3, result: 'finished' }) + '\n')
    return { exitCode: 0 }
  })()
  return { done, kill: () => {} }
}
const pool = new WorkerPool(engine, mcp.url, 2, spawnFake)

const until = async (what: string, ok: () => boolean) => {
  for (let i = 0; i < 200; i++) {
    if (ok() && pool.status().every((w) => w.itemId === null)) return
    await new Promise((r) => setTimeout(r, 10))
  }
  throw new Error(`timed out waiting for ${what}`)
}
const blocker = (id: string) => engine.get(id)!.blocker

// A new item waits for the operator, and Next finds it.
const id = engine.create('Answer the PR')!
assert.equal(blocker(id), 'operator')
assert.equal(engine.next(), id)
assert.equal(engine.next([id]), null)

// Dispatched, an agent reads it, records state and asks for an approval.
behaviours.push(async (call) => {
  const item = JSON.parse(await call('get_item'))
  assert.equal(item.item.instruction, 'Reply on the PR')
  await call('set_state', { state: 'Drafted a reply.' })
  await call('request_approval', {
    state: 'Reply drafted; needs posting.',
    action: 'github.pr.comment',
    inputs: { repo: 'o/r', number: '7', body: 'Thanks!' },
    reason: 'Post the reply.',
  })
  // A second ending is refused.
  await assert.rejects(call('close_item', { state: 'x' }), /already recorded/)
})
engine.dispatch(id, 'Reply on the PR')
await until('the approval', () => blocker(id) === 'operator')
const asked = engine.get(id)!
assert.equal(asked.state, 'Reply drafted; needs posting.')
assert.equal(asked.request?.approval?.action, 'github.pr.comment')
assert.equal(asked.request?.approval?.inputs.body, 'Thanks!')

// The operator changes the body and runs it; the change is recorded, and the item goes back to an agent.
behaviours.push(async (call, prompt) => {
  assert.match(prompt, /Ran Comment on a pull request/)
  await call('wait_for', { state: 'Posted; waiting for CI.', kind: 'github.pr.checks', params: { repo: 'o/r', number: 7 } })
})
await engine.execute(id, { repo: 'o/r', number: '7', body: 'Thanks a lot!' })
assert.deepEqual(ghCalls.at(-1), ['pr', 'comment', '7', '-R', 'o/r', '--body', 'Thanks a lot!'])
const run = engine.events(id).find((e) => e.kind === 'action_run')!
assert.deepEqual(run.data?.changed, ['body'])
await until('the wait', () => blocker(id) === 'external')

// The wait backs off: 1, 2, 5, 10, 10 minutes between checks.
for (const step of [1, 2, 3, 3]) {
  const wait = engine.wait(id)!
  clock = wait.nextCheckAt - 1
  await engine.tick()
  assert.equal(engine.wait(id)!.step, wait.step, 'not checked before it is due')
  clock = wait.nextCheckAt
  await engine.tick()
  assert.equal(engine.wait(id)!.step, step)
  assert.equal(engine.wait(id)!.nextCheckAt, clock + BACKOFF_MINUTES[step] * MINUTE)
}

// Once the checks finish, it goes back to an agent, which splits off a sub-item and waits on it.
let child = ''
behaviours.push(async (call, prompt) => {
  assert.match(prompt, /all 1 passed/)
  child = await call('create_item', { title: 'Follow-up', instruction: 'Do the follow-up' })
  await call('wait_for_items', { state: 'Waiting on the follow-up.', item_ids: [child] })
})
behaviours.push(async (call) => call('close_item', { state: 'Follow-up done.' }).then(() => {}))
behaviours.push(async (call, prompt) => {
  assert.match(prompt, /have closed/)
  await call('close_item', { state: 'All done.' })
})
checksPending = false
clock = engine.wait(id)!.nextCheckAt
await engine.tick()
await until('everything to close', () => blocker(id) === 'closed')
assert.equal(blocker(child), 'closed')
assert.equal(engine.get(id)!.state, 'All done.')

// An agent that stops without saying what blocks the item hands it to the operator.
const quiet = engine.create('Quiet one', { instruction: 'Do something' })!
behaviours.push(async () => {})
await until('the quiet run', () => blocker(quiet) === 'operator')
assert.match(engine.get(quiet)!.request!.question, /stopped without saying/)
engine.handledOutside(quiet, 'Did it by hand in the GitHub UI')

// A wait with a deadline comes back to the operator once it passes.
const slow = engine.create('Slow', { instruction: 'Wait for CI' })!
checksPending = true
behaviours.push(async (call) =>
  call('wait_for', { state: 's', kind: 'github.pr.checks', params: { repo: 'o/r', number: 7 }, timeout_minutes: 3 }).then(() => {}),
)
await until('the slow wait', () => blocker(slow) === 'external')
clock += 3 * MINUTE
await engine.tick()
assert.equal(blocker(slow), 'operator')
assert.match(engine.get(slow)!.request!.question, /Gave up/)

// Sources create items once.
await engine.pollSources(true)
await engine.pollSources(true)
const reviews = engine.overview().items.filter((i) => i.source === 'github.review-requests')
assert.equal(reviews.length, 1)
assert.equal(reviews[0].blocker, 'operator')

// The history is there to look back on: time per blocker, runs, changes, work done outside.
const recent = engine.recent(7)
const first = recent.find((r) => r.id === id)!
assert.equal(first.agentRuns, 4)
assert.equal(first.approvalsChanged, 1)
assert.ok(first.minutesOn.external >= 18, JSON.stringify(first.minutesOn))
assert.deepEqual(recent.find((r) => r.id === quiet)!.handledOutside, ['Did it by hand in the GitHub UI'])
const firstRun = engine.detail(id)!.runs[0]
assert.match(engine.transcript(firstRun.id)!, /"total_cost_usd":0.01/)

// And an agent can read it all over MCP.
const reviewer = engine.create('Review the week', { instruction: 'Look back' })!
behaviours.push(async (call) => {
  const items = JSON.parse(await call('recent_items', { days: 7 }))
  assert.ok(items.some((i: { handledOutside: string[] }) => i.handledOutside.length))
  assert.match(await call('run_transcript', { run_id: firstRun.id }), /PROMPT:/)
  assert.match(await call('item_history', { item_id: id }), /action_run/)
  await call('close_item', { state: 'Reviewed.' })
})
await until('the review', () => blocker(reviewer) === 'closed')

// Taking an item over brings it back to the operator.
const taken = engine.create('Take me', { instruction: 'x' })!
engine.takeOver(taken)
assert.equal(blocker(taken), 'operator')
assert.equal(behaviours.length, 0)

mcp.server.close()
console.log('engine: ok')
