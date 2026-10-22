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

const { setSourceTransport } = await import('../src/renderer/src/source/transport')
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

/** A store that answers only when told to, so a read through it is slow. */
let finish: (value: unknown) => void = () => undefined
setSourceTransport({
  call: () => new Promise((resolve) => (finish = resolve)),
  user: 'test',
  sourceId: 'slow',
})

let passed = 0
async function test(name: string, fn: () => Promise<void>): Promise<void> {
  await fn()
  console.log(`  ok  ${name}`)
  passed++
}

const tick = () => new Promise((r) => setTimeout(r, 0))

await test('settles a stopped call as cancelled', async () => {
  const result = callToolByName('getEntity', ['a'], context())
  await tick()
  const [id] = runningCallsAtom.get()
  assert.ok(id)

  stopCall(id)
  assert.deepEqual(runningCallsAtom.get(), [])
  // A script waiting on it is told, rather than handed nothing.
  await assert.rejects(result, /stopped/)

  // Whatever the tool says afterwards changes nothing.
  finish({})
  await tick()
  assert.deepEqual(runningCallsAtom.get(), [])
})

await test('leaves a call that has answered alone', async () => {
  const result = callToolByName('getEntity', ['a'], context())
  await tick()
  const [id] = runningCallsAtom.get()
  finish({ a: { id: 'a', values: { text: 'done' }, outboundLinks: [], inboundLinks: [] } })
  assert.equal(((await result) as { values: { text: string } }).values.text, 'done')
  stopCall(id)
  assert.deepEqual(runningCallsAtom.get(), [])
})

console.log(`${passed} passed`)
