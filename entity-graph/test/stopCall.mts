// Stopping a call that is still running: the activity log's Stop.
//
// Stopping settles the call as cancelled at once, aborts its signal
// so the tool can let go of what it holds, and ignores whatever the tool hands
// back afterwards.
//
//   npm test

import assert from 'node:assert/strict'

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})
Object.defineProperty(globalThis, 'window', { value: { entityGraph: {} } })

const { userToolsAtom } = await import('../src/renderer/src/tools/userTools')
const { callToolByName, stopCall } = await import('../src/renderer/src/tools/call')
const { runningCallsAtom } = await import('../src/renderer/src/state/store')

const context = () => ({
  values: {},
  path: [],
  groupId: null,
  tabId: null,
  frameId: null,
  startedAt: Date.now(),
})

/** A tool that answers only when told to, and says whether it was aborted. */
let finish: (value: unknown) => void = () => undefined
let aborted = false
userToolsAtom.set([
  {
    id: 'test.slow',
    label: 'Slow',
    scope: 'app',
    reach: 'external',
    run: (_args, call) => {
      call.signal.addEventListener('abort', () => (aborted = true))
      return new Promise((resolve) => (finish = (data) => resolve({ data })))
    },
  },
])

let passed = 0
async function test(name: string, fn: () => Promise<void>): Promise<void> {
  await fn()
  console.log(`  ok  ${name}`)
  passed++
}

const tick = () => new Promise((r) => setTimeout(r, 0))

await test('settles a stopped call as cancelled, and aborts its signal', async () => {
  aborted = false
  const result = callToolByName('slow', [], context())
  await tick()
  const [id] = runningCallsAtom.get()
  assert.ok(id)

  stopCall(id)
  assert.equal(aborted, true)
  assert.deepEqual(runningCallsAtom.get(), [])
  // A script waiting on it is told, rather than handed nothing.
  await assert.rejects(result, /stopped/)

  // Whatever the tool says afterwards changes nothing.
  finish('late')
  await tick()
  assert.deepEqual(runningCallsAtom.get(), [])
})

await test('leaves a call that has answered alone', async () => {
  const result = callToolByName('slow', [], context())
  await tick()
  const [id] = runningCallsAtom.get()
  finish('done')
  assert.equal(await result, 'done')
  stopCall(id)
  assert.deepEqual(runningCallsAtom.get(), [])
})

console.log(`${passed} passed`)
