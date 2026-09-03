// The pensive layer, end to end and with no database under it.
//
// What is worth checking here is the part the sources page is made of: that
// several pensives read as one and are written to exactly one of them, that a
// bearer token *is* an identity rather than a label, that a paused node refuses
// everything with a sentence saying so, that a loop is refused before it can be
// drawn, and that a broadcast and a connection are two ends of the same
// interface — the round trip goes out over real HTTP and comes back as a
// pensive.
//
//   npm test

import assert from 'node:assert/strict'
import { CombinedPensive } from '../src/core/pensive/combined'
import { ConnectPensive } from '../src/core/pensive/connect'
import { AttributedPensive } from '../src/core/pensive/attributed'
import { PausedPensive } from '../src/core/pensive/paused'
import { PensiveServer } from '../src/main/pensive/http'
import { PensiveRegistry, wouldCycle } from '../src/main/pensive/registry'
import type { GraphDb } from '../src/main/pensive/graph'
import type { NodeConfig, SourceNode } from '../src/core/client'
import { MemorySource } from './source.mjs'

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

/** The text of one entity, as the store rolls it up. */
async function text(pensive: { callTool: (t: string, a: unknown) => Promise<unknown> }, id: string) {
  const read = (await pensive.callTool('readEntities', { entityIds: [id] })) as Record<
    string,
    { values: Record<string, unknown> }
  >
  return read[id]?.values.text
}

/** The author of every event a store holds. */
const authors = (store: MemorySource): string[] => store.events.map((e) => e.author)

// --- One pensive ------------------------------------------------------------

test('answers the tools its interface implies, and nothing that reaches outside', async () => {
  const store = new MemorySource()
  const ids = (await store.listTools()).map((t) => t.id)
  for (const wanted of ['readEvents', 'writeValue', 'query', 'popEvents', 'writeResource']) {
    assert.ok(ids.includes(wanted), `missing ${wanted}`)
  }
  for (const gone of ['httpRequest', 'runCommand']) {
    assert.ok(!ids.includes(gone), `${gone} should not be a tool of a pensive`)
  }
})

test('round-trips a value through its own tools', async () => {
  const store = new MemorySource()
  await store.callTool('writeValue', { entityId: 'a', key: 'text', value: 'hello' })
  assert.equal(await text(store, 'a'), 'hello')
})

// --- Several, read as one ---------------------------------------------------

test('reads its inputs as one store', async () => {
  const left = new MemorySource()
  const right = new MemorySource()
  left.values({ shared: { text: 'from the left' } })
  right.tree({ shared: ['below'] })
  const combined = new CombinedPensive('c', 'Both', [left, right], left)
  const read = (await combined.callTool('readEntities', { entityIds: ['shared'] })) as Record<
    string,
    { values: Record<string, unknown>; outboundLinks: string[] }
  >
  assert.equal(read.shared.values.text, 'from the left')
  assert.deepEqual(read.shared.outboundLinks, ['below'])
})

test('writes only to the input it was told to write to', async () => {
  const left = new MemorySource()
  const right = new MemorySource()
  const combined = new CombinedPensive('c', 'Both', [left, right], right)
  await combined.callTool('writeValue', { entityId: 'a', key: 'text', value: 'written' })
  assert.equal(left.events.length, 0)
  assert.equal(right.events.length, 1)
  // And it reads back through the union, so where it went is not the point.
  assert.equal(await text(combined, 'a'), 'written')
})

test('says what is missing when it has no write source', async () => {
  const combined = new CombinedPensive('c', 'Both', [new MemorySource()], null)
  await assert.rejects(
    () => combined.callTool('writeValue', { entityId: 'a', key: 'text', value: 'x' }),
    /no write source/,
  )
})

test('finds a resource in whichever input holds it', async () => {
  const left = new MemorySource()
  const right = new MemorySource()
  await right.writeResource({
    id: 'file',
    timestamp: 1,
    author: 'me',
    mimeType: 'text/plain',
    name: null,
    data: 'aGk=',
  })
  const combined = new CombinedPensive('c', 'Both', [left, right], left)
  const found = await combined.readResource('file')
  assert.equal(found?.data, 'aGk=')
})

// --- A token is an identity -------------------------------------------------

test('records every write as the person the token was issued to', async () => {
  const store = new MemorySource()
  const asKim = new AttributedPensive(store, 'kim')
  // Naming somebody else is the case that matters: a client that could name its
  // own author could name anybody's.
  await asKim.callTool('writeValue', { entityId: 'a', key: 'text', value: 'x', author: 'someone' })
  await asKim.callTool('writeLink', { sourceId: 'a', destinationId: 'b', action: 0 })
  assert.deepEqual(authors(store), ['kim', 'kim'])
})

test('takes only one person\'s events off, when it is told whose', async () => {
  const store = new MemorySource()
  const now = Date.now()
  store.given(
    { type: 'value', entityId: 'a', key: 'text', value: 'kim wrote this', author: 'kim', timestamp: now },
    { type: 'value', entityId: 'b', key: 'text', value: 'sam wrote this', author: 'sam', timestamp: now },
  )
  const popped = (await store.callTool('popEvents', { author: 'kim' })) as { author: string }[]
  assert.deepEqual(
    popped.map((e) => e.author),
    ['kim'],
  )
  // Sam's is still there, at the same timestamp, so it was the author that
  // decided and not the clock.
  assert.deepEqual(authors(store), ['sam'])
})

test('refuses to undo somebody else\'s edit, however the client asks', async () => {
  const store = new MemorySource()
  const now = Date.now()
  store.given({
    type: 'value',
    entityId: 'a',
    key: 'text',
    value: 'not yours',
    author: 'sam',
    timestamp: now,
  })
  const asKim = new AttributedPensive(store, 'kim')
  // Naming Sam is exactly the request that has to come back empty.
  assert.deepEqual(await asKim.callTool('popEvents', { author: 'sam' }), [])
  assert.deepEqual(authors(store), ['sam'])
})

// --- Paused -----------------------------------------------------------------

test('refuses everything while it is paused, and says who is paused', async () => {
  const paused = new PausedPensive('n', 'Notes')
  await assert.rejects(() => paused.readEvents(), /"Notes" is paused/)
  await assert.rejects(() => paused.callTool('query', { path: '@index' }), /"Notes" is paused/)
  assert.deepEqual(await paused.listTools(), [])
})

test('reads around an input that is paused, rather than going down with it', async () => {
  const live = new MemorySource()
  live.values({ a: { text: 'here' } })
  const combined = new CombinedPensive(
    'c',
    'Both',
    [live, new PausedPensive('p', 'Archive')],
    live,
  )
  // Pausing one store of two is something somebody did on purpose: it takes that
  // store's notes out of the outline and leaves the rest readable.
  assert.equal(await text(combined, 'a'), 'here')
  // Writing has one place to go, so a paused write source is still a refusal.
  const toArchive = new CombinedPensive(
    'c2',
    'Both',
    [live, new PausedPensive('p', 'Archive')],
    new PausedPensive('p', 'Archive'),
  )
  await assert.rejects(
    () => toArchive.callTool('writeValue', { entityId: 'b', key: 'text', value: 'x' }),
    /"Archive" is paused/,
  )
})

// --- Loops ------------------------------------------------------------------

test('refuses an edge that would put a node downstream of itself', async () => {
  const edges = [
    { from: 'a', to: 'b' },
    { from: 'b', to: 'c' },
  ]
  assert.equal(wouldCycle(edges, 'c', 'a'), true, 'closing the ring')
  assert.equal(wouldCycle(edges, 'a', 'a'), true, 'onto itself')
  assert.equal(wouldCycle(edges, 'a', 'c'), false, 'a second path forwards is fine')
})

// --- Building the drawing ---------------------------------------------------

/**
 * A graph without a database under it. The registry only ever asks two things of
 * one, and a real `GraphDb` wants SQLite — which in this install is built for
 * Electron, so a test cannot open one.
 *
 * Every node here is a `connect` pointing at a port nothing is on, so building
 * one succeeds and reading it fails at once: what is under test is the building.
 */
function stubGraph(shape: Record<string, { config: NodeConfig; inputs?: string[] }>): GraphDb {
  return {
    node: (id: string): SourceNode | undefined =>
      shape[id] && { id, label: id, x: 0, y: 0, paused: false, config: shape[id].config },
    inputs: (id: string): string[] => shape[id]?.inputs ?? [],
  } as unknown as GraphDb
}

const registryOver = (graph: GraphDb): PensiveRegistry =>
  new PensiveRegistry(graph, { storeRoot: '/tmp', author: () => 'test' })

const nowhere: NodeConfig = { kind: 'connect', url: 'http://127.0.0.1:1', token: 't' }

test('two callers asking for the same node at once is not a loop', async () => {
  const registry = registryOver(
    stubGraph({
      store: { config: nowhere },
      everything: { config: { kind: 'combined', writeTo: null }, inputs: ['store'] },
      desktop: { config: { kind: 'desktop' }, inputs: ['everything'] },
    }),
  )
  // The page reading the graph while the window reads a note. Asking twice at
  // once used to look exactly like a node being downstream of itself.
  const [a, b] = await Promise.all([registry.get('desktop'), registry.get('desktop')])
  assert.equal(a, b, 'one build, shared')
})

test('still refuses a node that really is downstream of itself', async () => {
  const registry = registryOver(
    stubGraph({
      left: { config: { kind: 'combined', writeTo: null }, inputs: ['right'] },
      right: { config: { kind: 'combined', writeTo: null }, inputs: ['left'] },
    }),
  )
  await assert.rejects(() => registry.get('left'), /downstream of itself/)
})

test('builds a combiner from the inputs that work, and says what it left out', async () => {
  const registry = registryOver(
    stubGraph({
      good: { config: nowhere },
      // No URL, so it cannot be built at all — the case a typo makes.
      broken: { config: { kind: 'connect', url: '', token: '' } },
      everything: {
        config: { kind: 'combined', writeTo: null },
        inputs: ['good', 'broken'],
      },
    }),
  )
  await registry.get('everything')
  assert.match(registry.problem('everything') ?? '', /without 1 of its inputs/)
})

// --- Broadcast and connect, over real HTTP ----------------------------------

/** A server over one store, with one token issued to `kim`. */
function serve(store: MemorySource, port: number, paused = { paused: false }) {
  return new PensiveServer({
    kind: 'broadcast',
    port,
    node: () => ({ label: 'Broadcast', paused: paused.paused }),
    authorOf: (token) => (token === 'good' ? 'kim' : null),
    pensive: async () => ({ pensive: store }),
  })
}

/** A port nothing else in this file is using. */
let nextPort = 47311

test('publishes what the store itself calls a tool, not only the built-ins', async () => {
  const store = new MemorySource()
  store.tree({ '@tools': ['greet'] })
  store.values({
    greet: {
      type: 'tool',
      text: 'greet',
      description: 'Say hello',
      arguments: [{ name: 'who' }],
    },
  })
  const port = nextPort++
  const server = serve(store, port)
  await server.start()
  try {
    const connected = new ConnectPensive('r', 'Remote', `http://127.0.0.1:${port}`, 'good')
    const tools = (await connected.listTools()).map((t) => t.id)
    assert.ok(tools.includes('greet'), tools.join(', '))
  } finally {
    await server.stop()
  }
})

test('publishes a pensive over HTTP, and reads it back as one', async () => {
  const store = new MemorySource()
  store.values({ a: { text: 'over the wire' } })
  const port = nextPort++
  const server = serve(store, port)
  await server.start()
  try {
    assert.equal(server.problem, null, server.problem ?? '')
    const connected = new ConnectPensive('r', 'Remote', `http://127.0.0.1:${port}`, 'good')
    const tools = (await connected.listTools()).map((t) => t.id)
    assert.ok(tools.includes('query'))
    assert.equal(await text(connected, 'a'), 'over the wire')

    // And a write through it is recorded as whoever the token belongs to.
    await connected.callTool('writeValue', { entityId: 'b', key: 'text', value: 'mine' })
    assert.deepEqual(authors(store).slice(-1), ['kim'])
  } finally {
    await server.stop()
  }
})

test('refuses a call with the wrong token, and one to a paused node', async () => {
  const store = new MemorySource()
  const port = nextPort++
  const paused = { paused: false }
  const server = serve(store, port, paused)
  await server.start()
  try {
    const wrong = new ConnectPensive('r', 'Remote', `http://127.0.0.1:${port}`, 'bad')
    await assert.rejects(() => wrong.listTools(), /401/)

    paused.paused = true
    const right = new ConnectPensive('r', 'Remote', `http://127.0.0.1:${port}`, 'good')
    await assert.rejects(() => right.callTool('query', { path: '@index' }), /paused/)
  } finally {
    await server.stop()
  }
})

test('ignores a path in front of its own routes, so a pasted URL still works', async () => {
  const store = new MemorySource()
  const port = nextPort++
  const server = serve(store, port)
  await server.start()
  try {
    // The phone client appends a source id to its base URL; one server serves
    // one pensive, so the segment is noise rather than an error.
    const res = await fetch(`http://127.0.0.1:${port}/anything/tools`, {
      headers: { authorization: 'Bearer good' },
    })
    assert.equal(res.status, 200)
    assert.ok(Array.isArray(await res.json()))
  } finally {
    await server.stop()
  }
})

// --- Run --------------------------------------------------------------------

let failed = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
