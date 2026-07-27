// Drives the frontend cache with no browser at all: a localStorage stub, an
// in-memory source built from the real default tools, and then nothing but the
// `state/` and `source/` layers.
//
// That this runs is itself the assertion the architecture makes — those layers
// hold no React and touch no DOM. What it checks beyond that is the part of the
// cache worth checking: that reading is answered immediately and improves, that
// the overscan means one round trip rather than one per level, that a type's
// values stand behind an entity's own, that an `events` script may speak for
// entities other than its own, and that a write shows before it lands.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource } from './source.mjs'

// --- The browser bits the state layer expects -------------------------------

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})

// Imported after the stub: the persistent atoms read localStorage as they load.
const {
  entitiesAtom,
  getEntity,
  refreshEntities,
  setCodeEvaluator,
  setEntityFetcher,
} = await import('../src/core/cache')
const { rowsOf } = await import('../src/renderer/src/state/query')
const { layoutAtom } = await import('../src/renderer/src/state/store')
const { defaultLayout } = await import('../src/renderer/src/state/types')
const A = await import('../src/renderer/src/state/actions')
const { popEvents, scanEvents, setWriteObserver, writeValue } = await import(
  '../src/renderer/src/source/entity'
)
const { setSourceTransport } = await import('../src/renderer/src/source/transport')
const { applyEvents, removeEvents } = await import('../src/core/cache')

// --- Harness ----------------------------------------------------------------

/** Let the microtask flush, the fetch, and the reconcile that follows all land. */
async function settle(rounds = 12): Promise<void> {
  for (let i = 0; i < rounds; i++) await new Promise((r) => setTimeout(r, 0))
}

let source: MemorySource
let unmount = (): void => {}

/**
 * Set to hold the next scan open after it has read the store but before it
 * answers — which is how a read gets overtaken by a write.
 */
let holdNextScan = false
let releaseScan = (): void => {}

function open(): void {
  source = new MemorySource()
  holdNextScan = false
  setSourceTransport({
    call: async (tool, args) => {
      const result = await source.call(tool, args)
      if (tool === 'scanEvents' && holdNextScan) {
        holdNextScan = false
        await new Promise<void>((resolve) => (releaseScan = resolve))
      }
      return result
    },
    user: 'test',
    sourceId: 'memory',
  })
  setEntityFetcher(scanEvents)
  setWriteObserver({ applied: applyEvents, removed: removeEvents })
  setCodeEvaluator(null)
  layoutAtom.set(defaultLayout('root'))
  // Stand in for the mounted frame. Reading is what asks the source for
  // anything missing, so without something re-reading when the cache changes
  // the tree would stop growing after the first answer — which is precisely
  // what the re-render does in the app.
  unmount()
  unmount = entitiesAtom.subscribe(() => void rowsOf(frameId()))
}

/** The focused frame, which every test drives. */
const frameId = (): string => Object.keys(layoutAtom.get().frames)[0]

const texts = (): (string | undefined)[] =>
  rowsOf(frameId()).rows.map((r) => (r.kind === 'entity' ? r.text : '<input>'))

const shape = (): string[] =>
  rowsOf(frameId()).rows.flatMap((r) => (r.kind === 'entity' ? [r.path.join('/')] : []))

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

// --- Reading ----------------------------------------------------------------

test('answers a read before anything has loaded, and improves on it', async () => {
  open()
  source.values({ root: { text: 'Root' }, a: { text: 'A' } })
  source.tree({ root: ['a'] })

  // Synchronous, on the very first read: one row, empty, no waiting.
  assert.deepEqual(shape(), ['root'])
  assert.equal(rowsOf(frameId()).loading, true)

  await settle()
  assert.deepEqual(texts(), ['Root', 'A'])
  assert.equal(rowsOf(frameId()).loading, false)
  assert.equal(rowsOf(frameId()).complete, true)
})

test('walks three levels in one round trip, by overscanning', async () => {
  open()
  source.tree({ root: ['a'], a: ['b'], b: ['c'] })
  source.values({ root: { text: 'Root' }, a: { text: 'A' }, b: { text: 'B' }, c: { text: 'C' } })

  rowsOf(frameId())
  await settle()
  assert.deepEqual(shape(), ['root', 'root/a', 'root/a/b', 'root/a/b/c'])
  // Three levels for one trip — the scan read `root` and two layers of children
  // ahead of it — and the fourth cost a second trip, which asked only for the
  // one entity that was actually missing.
  assert.deepEqual(source.scans, [['root'], ['c']])
})

test('folds and caps depth without asking the source anything', async () => {
  open()
  source.tree({ root: ['a', 'b'], a: ['a1'] })
  rowsOf(frameId())
  await settle()
  assert.deepEqual(shape(), ['root', 'root/a', 'root/a/a1', 'root/b'])

  const before = source.calls
  A.toggleCollapse(layoutAtom.get().frames[frameId()].tabId, 'a')
  assert.deepEqual(shape(), ['root', 'root/a', 'root/b'])
  A.toggleCollapse(layoutAtom.get().frames[frameId()].tabId, 'a')

  // A depth cap is the frame's, not the query's: the whole map is honoured here.
  A.setMaxDepth(frameId(), 'root', 1)
  assert.deepEqual(shape(), ['root', 'root/a', 'root/b'])
  // A cap set nearer overrides one set further away, including by lifting it.
  A.setMaxDepth(frameId(), 'a', null)
  assert.deepEqual(shape(), ['root', 'root/a', 'root/a/a1', 'root/b'])
  A.setMaxDepth(frameId(), 'root', null)

  assert.equal(source.calls, before, 'folding is a derivation, not a query')
})

test('filters rows without changing what is loaded', async () => {
  open()
  source.tree({ root: ['a', 'b'] })
  source.values({ root: { text: 'Root' }, a: { text: 'apple' }, b: { text: 'pear' } })
  rowsOf(frameId())
  await settle()

  A.setFind(frameId(), 'app')
  assert.deepEqual(texts(), ['Root', 'apple'], 'matches keep their ancestors')
  A.setFind(frameId(), null)

  source.values({ s: { text: 'Section', section: true } })
  source.tree({ root: ['s'] })
  refreshEntities()
  await settle()
  A.setSectionsOnly(frameId(), true)
  assert.deepEqual(texts(), ['Root', 'Section'])
  A.setSectionsOnly(frameId(), false)
})

// --- Type defaults ----------------------------------------------------------

test('lays a type’s values in behind an entity’s own', async () => {
  open()
  source.tree({ root: ['a', 'b'] })
  source.values({
    task: { text: 'Task', open: true, colour: 'blue' },
    a: { type: 'task' },
    b: { type: 'task', text: 'Write it', colour: null },
  })
  rowsOf(frameId())
  await settle()

  // `a` says nothing of its own, so it is the type all the way down.
  assert.equal(getEntity('a').values.text, 'Task')
  assert.equal(getEntity('a').values.colour, 'blue')
  // `b` overrides one key and clears another: null is a value, not an absence,
  // so it is how an entity opts out of a default.
  assert.equal(getEntity('b').values.text, 'Write it')
  assert.equal(getEntity('b').values.colour, null)
  // The type was never asked for by name — reading the entity fetched it.
  assert.equal(getEntity('task').values.text, 'Task')
})

test('reaches every entity of a type when the type arrives late', async () => {
  open()
  source.tree({ root: ['a'] })
  source.values({ a: { type: 'far' }, far: { flavour: 'vanilla' } })

  rowsOf(frameId())
  await settle(2)
  // `far` is not linked from anything, so the overscan can't have reached it:
  // the default only turns up once the type has been fetched on its own.
  await settle()
  assert.equal(getEntity('a').values.flavour, 'vanilla')
})

test('re-reads a type after a write, though nothing has a row for it', async () => {
  open()
  source.tree({ root: ['a'] })
  source.values({ a: { type: 'task' }, task: { colour: 'blue' } })
  rowsOf(frameId())
  await settle()
  assert.equal(getEntity('a').values.colour, 'blue')

  source.given({
    type: 'value',
    entityId: 'task',
    key: 'colour',
    value: 'green',
    timestamp: Date.now(),
  })
  refreshEntities()
  await settle()
  assert.equal(getEntity('a').values.colour, 'green')
})

// --- Derived events ---------------------------------------------------------

test('runs an entity’s events script once, and lets it speak for others', async () => {
  open()
  source.tree({ root: ['repo'], repo: ['branch'] })
  source.values({ repo: { text: 'Repo', events: 'branches()' } })

  let ran = 0
  setCodeEvaluator(async (entityId, code) => {
    ran++
    assert.equal(entityId, 'repo')
    assert.equal(code, 'branches()')
    return [
      { key: 'text', value: 'Repo (live)' },
      { entityId: 'branch', key: 'text', value: 'main' },
    ]
  })

  rowsOf(frameId())
  await settle()
  assert.equal(ran, 1)
  // Timestamp 0 by default, so a derived value loses to a real one — `repo`
  // already has text, and keeps it. `branch` has none, so it takes what it was given.
  assert.equal(getEntity('repo').values.text, 'Repo')
  assert.equal(getEntity('branch').values.text, 'main')

  // A write invalidates the read events; the script is not run again for it.
  refreshEntities()
  await settle()
  assert.equal(ran, 1)
  assert.equal(getEntity('branch').values.text, 'main')
})

test('waits for the type before running a script the type could supply', async () => {
  open()
  source.tree({ root: ['x'] })
  source.values({ x: { type: 'scripted' }, scripted: { events: 'fromTheType()' } })

  const seen: string[] = []
  setCodeEvaluator(async (entityId, code) => {
    seen.push(`${entityId}:${code}`)
    return { key: 'note', value: 'inherited' }
  })

  rowsOf(frameId())
  await settle()
  // Both the type and the entity have the script — the type by writing it, the
  // entity by inheriting it — and both are run against themselves.
  assert.deepEqual(seen.sort(), ['scripted:fromTheType()', 'x:fromTheType()'])
  assert.equal(getEntity('x').values.note, 'inherited')
})

// --- Writing and undoing ----------------------------------------------------

test('shows a write before the round trip finishes', async () => {
  open()
  source.tree({ root: ['a'] })
  source.values({ a: { text: 'before' } })
  rowsOf(frameId())
  await settle()

  const writing = writeValue('a', 'text', 'after')
  // Not awaited: the event went into the cache on its way out.
  assert.deepEqual(texts(), [undefined, 'after'])
  await writing
  refreshEntities()
  await settle()
  assert.deepEqual(texts(), [undefined, 'after'], 'and the store agrees once it has caught up')
})

test('takes undone events back out of the cache', async () => {
  open()
  source.tree({ root: ['a'] })
  source.values({ a: { text: 'before' } })
  rowsOf(frameId())
  await settle()

  await writeValue('a', 'text', 'after')
  assert.equal(getEntity('a').values.text, 'after')

  const popped = await popEvents()
  assert.equal(popped.length, 1)
  // No refetch: the rows are right the instant the events come off.
  assert.equal(getEntity('a').values.text, 'before')
})

test('does not let a read already in flight undo a write', async () => {
  open()
  source.tree({ root: ['a'] })
  source.values({ a: { text: 'before' } })
  rowsOf(frameId())
  await settle()

  // A read that saw the store before the write and answers after it. What it
  // says about `a` is the truth as it was, so it must not be believed.
  holdNextScan = true
  refreshEntities()
  await settle()

  await writeValue('a', 'text', 'after')
  releaseScan()
  await settle()
  assert.equal(getEntity('a').values.text, 'after')
})

test('keeps showing what it has while a refresh is in flight', async () => {
  open()
  source.values({ root: { text: 'Root' } })
  rowsOf(frameId())
  await settle()

  refreshEntities()
  assert.deepEqual(texts(), ['Root'], 'stale, but there')
  assert.equal(rowsOf(frameId()).loading, true)
  await settle()
  assert.equal(rowsOf(frameId()).loading, false)
})

test('does not ask again for an entity that failed', async () => {
  open()
  setEntityFetcher(async () => {
    throw new Error('no')
  })
  rowsOf(frameId())
  await settle()
  assert.equal(rowsOf(frameId()).error, 'no')

  let asked = 0
  setEntityFetcher(async (ids) => {
    asked++
    return scanEvents(ids)
  })
  // Pointing at a source clears the cache, so this is a fresh start rather than
  // a retry — the error only stands while the fetcher that produced it does.
  rowsOf(frameId())
  await settle()
  assert.equal(asked, 1)
  assert.equal(rowsOf(frameId()).error, null)
})

// --- Run --------------------------------------------------------------------

let failed = 0
for (const [name, run] of tests) {
  entitiesAtom.set({})
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
