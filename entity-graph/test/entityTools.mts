// The entity tools that don't need a frame under them — which today means the
// one read a script can rely on.
//
// `entity.get` goes to the store rather than to the cache, and the reason is the
// whole test: a script has nowhere to put "not yet". Everything on screen reads
// the cache, which answers with whatever it has and fills in behind; a script
// gets one answer and has to be able to use it.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource } from './source.mjs'

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
const { callToolByName } = await import('../src/renderer/src/tools/call')
const { entitiesAtom } = await import('../src/core/cache')

// --- Harness ----------------------------------------------------------------

let source: MemorySource

function open(): void {
  source = new MemorySource()
  entitiesAtom.set({})
  setSourceTransport({ call: (t, a) => source.call(t, a), user: 'test', sourceId: 'memory' })
}

const context = () => ({
  values: {},
  path: [],
  groupId: null,
  tabId: null,
  frameId: null,
  startedAt: Date.now(),
})

const get = (id: unknown): Promise<any> => callToolByName('getEntity', [id], context())

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

// --- Tests ------------------------------------------------------------------

test('rolls an entity up out of the store, though nothing has read it', async () => {
  open()
  source.tree({ root: ['a'] })
  source.values({ a: { text: 'first', colour: 'blue' } })
  source.values({ a: { text: 'second' } })
  // Nothing has been rendered, so the cache is empty and cannot be the source of
  // this answer — which is exactly the case a script is in.
  assert.deepEqual(entitiesAtom.get(), {})

  const entity = await get('a')
  // Rolled up, not handed back as events: the later write of `text` wins, and the
  // key it didn't touch is still there.
  assert.equal(entity.values.text, 'second')
  assert.equal(entity.values.colour, 'blue')
  assert.equal(entity.id, 'a')
})

test('hands back the links either way, as part of the rollup', async () => {
  open()
  source.tree({ root: ['a', 'b'] })
  source.values({ a: { text: 'a' } })

  const root = await get('root')
  assert.deepEqual(root.outboundLinks, ['a', 'b'])
  assert.deepEqual((await get('a')).inboundLinks, ['root'])
})

test('answers for an id nothing was ever written to, rather than failing', async () => {
  open()
  const entity = await get('never')
  assert.deepEqual(entity.values, {})
  assert.deepEqual(entity.outboundLinks, [])
})

test('refuses a blank id rather than rolling up nothing in particular', async () => {
  open()
  await assert.rejects(() => get('  '), /Entity id is required/)
})

test('is reachable by its id as well as by the camel case of its label', async () => {
  const { findToolByName } = await import('../src/renderer/src/tools/registry')
  assert.equal(findToolByName('entity.get')?.id, 'entity.get')
  assert.equal(findToolByName('getEntity')?.id, 'entity.get')
})

test('stays out of the call log, being a read', async () => {
  const { callsAtom } = await import('../src/renderer/src/state/store')
  open()
  callsAtom.set([])
  source.values({ a: { text: 'a' } })
  await get('a')
  // A script walking a subtree would otherwise fill the log on its own.
  assert.deepEqual(callsAtom.get(), [])
})

// --- What a tool written in the store needs of these ------------------------

// The changeset tools live under `@tools` rather than in this repo, so what is
// left to test here is the ground they stand on: an id to hang a reply off, one
// write instead of seven, and a way to read a branch nothing has looked at.

const call = (name: string, args: unknown[]): Promise<any> =>
  callToolByName(name, args, context())

test('hands back the id of the entity it created', async () => {
  open()
  const id = await call('createChildOfEntity', ['parent', 'a note'])
  // Without this a script can add a note and then have no idea where it went,
  // which is the whole difficulty with hanging a reply under a question.
  assert.equal(typeof id, 'string')
  assert.ok(id)
  const made = await get(id)
  assert.equal(made.values.text, 'a note')
  const parent = await get('parent')
  assert.deepEqual(parent.outboundLinks, [id])
})

test('takes the rest of the values in the same write', async () => {
  open()
  const id = await call('createChildOfEntity', [
    '@changesets',
    'Fix the login flow',
    { type: 'changeset', branch: 'aB3xY9' },
  ])
  const made = await get(id)
  assert.equal(made.values.type, 'changeset')
  assert.equal(made.values.branch, 'aB3xY9')
  // Named twice, the palette's answer wins: it is the one that was asked for.
  assert.equal(made.values.text, 'Fix the login flow')
})

test('reads a branch as markdown, out of the store rather than the screen', async () => {
  open()
  source.tree({ rules: ['first', 'second'], second: ['detail'] })
  source.values({
    rules: { text: 'Agent instructions', section: true },
    first: { text: 'Commit in reasonable chunks' },
    second: { text: 'Work independently' },
    detail: { text: 'For anything easily reversible' },
  })
  // Nothing has been rendered, so a frame's rows could not answer this.
  assert.deepEqual(entitiesAtom.get(), {})

  const markdown = await call('readEntityOutline', ['rules'])
  assert.equal(
    markdown,
    ['# Agent instructions', '- Commit in reasonable chunks', '- Work independently', '  - For anything easily reversible'].join('\n'),
  )
})

test('reads an outline of nothing without complaining', async () => {
  open()
  assert.equal(await call('readEntityOutline', ['never-written-to']), '')
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
