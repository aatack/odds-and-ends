// What an agent sees of a pensive.
//
// Three things are worth a test here, and they are all about the channel rather
// than the store. A client truncates the server's instructions at 2KB and each
// tool's description at 2KB apiece, and writing past a cut is writing nothing —
// so the lengths are asserted rather than hoped for. The tool list is derived
// from what the pensive can actually do, so a store that only reads offers a
// model nothing it will fail at. And a write that arrives this way is recorded
// as `<author>:mcp`, which is how history tells an agent's edit from a
// keystroke.
//
//   npm test

import assert from 'node:assert/strict'
import { INSTRUCTIONS, MCP_TOOLS, toolsFor } from '../src/main/pensive/mcpServer'
import type { Pensive } from '../src/core/pensive/types'
import { MemorySource } from './source.mjs'

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

/** The 2KB a client will show of any one of these channels. */
const CUT = 2000

/** A pensive that answers only the tools named — a narrowed or broken store. */
const offering = (ids: string[]): Pensive =>
  ({
    id: 'x',
    label: 'X',
    listTools: async () => ids.map((id) => ({ id, name: id, description: '', safety: 'pure', args: {} })),
  }) as unknown as Pensive

test('keeps the instructions inside what a client will show', async () => {
  assert.ok(
    INSTRUCTIONS.length < CUT,
    `the instructions are ${INSTRUCTIONS.length} characters; a client shows ${CUT}`,
  )
})

test('keeps every tool description inside what a client will show', async () => {
  for (const tool of MCP_TOOLS) {
    assert.ok(
      tool.description.length < CUT,
      `${tool.name} is ${tool.description.length} characters; a client shows ${CUT}`,
    )
  }
})

test('offers a model only the tools the pensive can serve', async () => {
  const whole = (await toolsFor(new MemorySource())).map((t) => t.name)
  assert.deepEqual(whole, ['query', 'get_details', 'create', 'set_value', 'add_link', 'remove_link'])

  const readOnly = (await toolsFor(offering(['query', 'readEntities']))).map((t) => t.name)
  assert.deepEqual(readOnly, ['query', 'get_details'])
})

test('reads an outline with an id in front of every line', async () => {
  const store = new MemorySource()
  store.tree({ '@index': ['a'], a: ['b'] })
  store.values({ a: { text: 'above' }, b: { text: 'below', section: true } })
  const query = (await toolsFor(store)).find((t) => t.name === 'query')!
  const out = await query.run(store, { path: '@index' })
  assert.match(out, /^a\s+- above$/m)
  assert.match(out, /^b\s+- ### below$/m)
  assert.match(out, /that is everything under this path/)
})

test('records a write as the author it belongs to, marked as having come over MCP', async () => {
  const store = new MemorySource()
  const tools = await toolsFor(store)
  const create = tools.find((t) => t.name === 'create')!
  const said = await create.run(store, { parentId: '@index', text: 'a note', open: true })
  assert.match(said, /^Created .* under @index\.$/)
  // `test` is the store's default author; the suffix says what did the writing.
  assert.deepEqual(new Set(store.events.map((e) => e.author)), new Set(['test:mcp']))
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
