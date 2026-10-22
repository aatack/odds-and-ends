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
/** Counted, because how often the mirror is written is itself worth asserting. */
let writes = 0
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => {
      writes++
      store.set(k, v)
    },
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})

// Imported after the stub: the persistent atoms read localStorage as they load.
const {
  entitiesAtom,
  getEntity,
  refreshEntities,
  setEntityFetcher,
} = await import('../src/core/cache')
const { PAGE_SIZE, loadMore, rowsOf } = await import('../src/renderer/src/state/query')
const { setQueryObserver } = await import('../src/renderer/src/state/derive')
const { flushPersisted } = await import('../src/renderer/src/state/atom')
const { layoutAtom } = await import('../src/renderer/src/state/store')
const { defaultLayout } = await import('../src/renderer/src/state/types')
const A = await import('../src/renderer/src/state/actions')
const { createEntity, popEvents, scanEvents, setWriteObserver, writeValue } = await import(
  '../src/renderer/src/source/entity'
)
const { setSourceTransport } = await import('../src/renderer/src/source/transport')
const { applyEvents, invalidateEntities, removeEvents } = await import('../src/core/cache')

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
  setWriteObserver({
    applied: applyEvents,
    removed: removeEvents,
    touched: invalidateEntities,
  })
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

/** How far in each row reads, which is not always how deep it sits. */
const depths = (): number[] => rowsOf(frameId()).rows.map((r) => r.depth)

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

test('does not re-resolve the query when the selection moves', async () => {
  open()
  // A great many siblings under one parent: the shape that made holding a
  // movement key down crawl, because every press re-walked the lot. Under the
  // page ceiling, so the walk finishes and the selection resolves against real
  // rows rather than being left alone as a page still outstanding.
  const kids = Array.from({ length: 150 }, (_, i) => `k${i}`)
  source.tree({ root: kids })
  source.values(Object.fromEntries(kids.map((id) => [id, { text: id }])))
  rowsOf(frameId())
  await settle()
  assert.equal(rowsOf(frameId()).rows.length, 151)
  assert.equal(rowsOf(frameId()).complete, true)

  let resolved = 0
  setQueryObserver(() => void resolved++)

  // Every read of the rows here goes through the memo — the render's and the
  // one a movement tool makes to find out which row comes next.
  for (const id of kids.slice(0, 20)) {
    A.selectPath(frameId(), ['root', id])
    assert.deepEqual(rowsOf(frameId()).selectedPath, ['root', id])
  }
  assert.equal(resolved, 0, 'the cursor moved twenty times and the tree stood still')

  // And the rows the cursor left behind are the same objects, which is what lets a
  // memoised row component sit out a keypress. Two change: the one being left and
  // the one arrived at.
  const before = rowsOf(frameId())
  A.selectPath(frameId(), ['root', 'k99'])
  const after = rowsOf(frameId())
  assert.equal(after.selectedIndex, 100, 'found by looking the key up, not by scanning')
  assert.equal(after.rows[100].kind === 'entity' && after.rows[100].selected, true)
  const changed = after.rows.filter((row, i) => row !== before.rows[i])
  assert.equal(changed.length, 2, 'only the two rows whose selection changed are new')
  assert.equal(after.keys, before.keys, 'the keys are the same array, so offsets stand')

  // What does shape the tree still re-resolves it, or nothing would ever change.
  A.setFind(frameId(), 'k1')
  rowsOf(frameId())
  assert.equal(resolved, 1)
  A.setFind(frameId(), null)
  setQueryObserver(null)
})

test('writes the layout mirror once for a burst of cursor moves, not once each', async () => {
  open()
  source.tree({ root: ['a', 'b', 'c'] })
  source.values({ a: { text: 'A' }, b: { text: 'B' }, c: { text: 'C' } })
  rowsOf(frameId())
  await settle()

  // The layout carries the selection, so every move writes one. Serialising it
  // into localStorage inside the keystroke is what made holding a key down cost
  // a synchronous disk write per press.
  flushPersisted()
  const before = writes
  for (const id of ['a', 'b', 'c']) A.selectPath(frameId(), ['root', id])
  assert.equal(writes, before, 'nothing is written while the keys are coming in')
  flushPersisted()
  assert.equal(writes, before + 1, 'one write for the burst, holding the last value')
  assert.deepEqual(
    JSON.parse(store.get('entity-graph.layout.v2')!).frames[frameId()].selectedPath,
    ['root', 'c'],
  )
})

test('keeps the indices straight when the box for a new child appears', async () => {
  open()
  source.tree({ root: ['a', 'b'], a: ['a1'] })
  source.values({ root: { text: 'Root' }, a: { text: 'A' }, a1: { text: 'A1' }, b: { text: 'B' } })
  rowsOf(frameId())
  await settle()
  assert.deepEqual(shape(), ['root', 'root/a', 'root/a/a1', 'root/b'])

  A.selectPath(frameId(), ['root', 'b'])
  assert.equal(rowsOf(frameId()).selectedIndex, 3)

  // The box lands after `a`'s whole subtree, which is above `b` — so `b` moves
  // down one and the selection has to move with it.
  A.startCreate(frameId(), ['root', 'a'])
  const rows = rowsOf(frameId())
  assert.equal(rows.editIndex, 3)
  assert.equal(rows.rows[3].kind, 'input')
  assert.equal(rows.selectedIndex, 4)
  assert.equal(rows.rows[4].kind === 'entity' && rows.rows[4].text, 'B')
  assert.equal(rows.rows[4].kind === 'entity' && rows.rows[4].selected, true)
  // The keys stay index-aligned with the rows, since a view reads them together.
  assert.equal(rows.keys.length, rows.rows.length)
  A.setEdit(frameId(), null)
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

test('closes the gap a filter leaves, so no row jumps more than one level in', async () => {
  open()
  source.tree({ root: ['mid'], mid: ['deep'], deep: ['leaf'] })
  source.values({
    root: { text: 'Root' },
    mid: { text: 'Middle' },
    deep: { text: 'Buried heading', section: true },
    leaf: { text: 'Leaf' },
  })
  rowsOf(frameId())
  await settle()
  assert.deepEqual(depths(), [0, 1, 2, 3], 'unfiltered, depth is where a row really sits')

  // `deep` sits two levels below the root, and sections-only takes the row in
  // between. Indenting it by two would put it under a row that isn't there.
  A.setSectionsOnly(frameId(), true)
  assert.deepEqual(texts(), ['Root', 'Buried heading'])
  assert.deepEqual(depths(), [0, 1])
  A.setSectionsOnly(frameId(), false)

  // Find keeps every ancestor of a match, so there is no gap to close and the
  // depths are the real ones.
  A.setFind(frameId(), 'leaf')
  assert.deepEqual(depths(), [0, 1, 2, 3])
  A.setFind(frameId(), null)
})

test('doubles what it unrolls, and starts over when the query changes', async () => {
  open()
  const ids = Array.from({ length: PAGE_SIZE + 50 }, (_, i) => `c${i}`)
  source.tree({ root: ids })
  source.values(Object.fromEntries([['root', { text: 'Root' }], ...ids.map((id) => [id, { text: id }])]))
  rowsOf(frameId())
  await settle()

  // The limit is on the walk, so the first page is a page — and there is more.
  assert.equal(rowsOf(frameId()).rows.length, PAGE_SIZE)
  assert.equal(rowsOf(frameId()).complete, false)

  // One ask doubles it rather than adding a page, so a frame that has to unroll
  // its way to a screenful gets there in a few walks rather than one per page.
  loadMore(frameId())
  await settle()
  assert.equal(rowsOf(frameId()).rows.length, ids.length + 1)
  assert.equal(rowsOf(frameId()).complete, true)

  // A budget belongs to the query it was raised for. Filtering is a different
  // one, so it starts at a page again rather than walking everything the last
  // one had unrolled.
  A.setFind(frameId(), 'nothing matches this')
  assert.deepEqual(texts(), [])
  assert.equal(rowsOf(frameId()).complete, false, 'the walk is back to one page')
  A.setFind(frameId(), null)
  assert.equal(rowsOf(frameId()).rows.length, ids.length + 1, 'and the old budget is still there')
})

test('searches the outline when both filters are on, rather than either of them', async () => {
  open()
  source.tree({ root: ['fruit', 'veg'], fruit: ['leaf'] })
  source.values({
    root: { text: 'Root' },
    fruit: { text: 'Fruit', section: true },
    leaf: { text: 'apple' },
    veg: { text: 'Apple varieties', section: true },
  })
  rowsOf(frameId())
  await settle()

  A.setFind(frameId(), 'apple')
  assert.deepEqual(texts(), ['Root', 'Fruit', 'apple', 'Apple varieties'])

  // The two narrow together: "Fruit" says nothing about apples — it merely has
  // one written under it — so an outline searched for apples doesn't list it.
  A.setSectionsOnly(frameId(), true)
  assert.deepEqual(texts(), ['Root', 'Apple varieties'])
  A.setFind(frameId(), null)
  A.setSectionsOnly(frameId(), false)
})

test('reads an outline three levels down, unless the frame says otherwise', async () => {
  open()
  source.tree({ root: ['l1'], l1: ['l2'], l2: ['l3'], l3: ['l4'] })
  source.values({
    root: { text: 'Root' },
    l1: { text: 'One', section: true },
    l2: { text: 'Two', section: true },
    l3: { text: 'Three', section: true },
    l4: { text: 'Four', section: true },
  })
  rowsOf(frameId())
  await settle()
  assert.deepEqual(texts(), ['Root', 'One', 'Two', 'Three', 'Four'])

  // A table of contents is a shape, not a search: the fourth level is below what
  // the outline is for, and walking to it costs the whole subtree under it.
  A.setSectionsOnly(frameId(), true)
  assert.deepEqual(texts(), ['Root', 'One', 'Two', 'Three'])

  // An explicit cap on the root is the user's, including the one that lifts it.
  A.setMaxDepth(frameId(), 'root', null)
  await settle()
  assert.deepEqual(texts(), ['Root', 'One', 'Two', 'Three', 'Four'])
  A.setMaxDepth(frameId(), 'root', 1)
  assert.deepEqual(texts(), ['Root', 'One'])

  A.setSectionsOnly(frameId(), false)
})

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

test('keeps showing what it has while a refresh is in flight, and says nothing', async () => {
  open()
  source.values({ root: { text: 'Root' } })
  rowsOf(frameId())
  await settle()

  refreshEntities()
  assert.deepEqual(texts(), ['Root'], 'stale, but there')
  // Stale is not waiting. A row that is entirely here does not go back to its
  // loading state because something might have changed behind it — that flash,
  // on every row, on every keystroke, was the whole complaint.
  assert.equal(rowsOf(frameId()).loading, false)
  await settle()
  assert.equal(rowsOf(frameId()).loading, false)
  assert.deepEqual(texts(), ['Root'])
})

test('reads nothing again when a write says what it changed', async () => {
  open()
  source.tree({ root: ['a', 'b'] })
  source.values({ root: { text: 'Root' }, a: { text: 'A' }, b: { text: 'B' } })
  rowsOf(frameId())
  await settle()

  const before = source.scans.length
  await writeValue('a', 'text', 'A again')
  await settle()
  assert.deepEqual(texts(), ['Root', 'A again', 'B'])
  // The event went into the cache on its way out, so there is nothing to read:
  // not `a`, whose events are known, and certainly not `b`.
  assert.deepEqual(source.scans.slice(before), [])
})

test('reads back only what a write it could not state touched', async () => {
  open()
  source.tree({ root: ['a', 'b'] })
  source.values({ root: { text: 'Root' }, a: { text: 'A' }, b: { text: 'B' } })
  rowsOf(frameId())
  await settle()

  const before = source.scans.length
  // `createEntity` mints its id on the server, so there are no events to apply —
  // only the two entities it moved.
  await createEntity({ text: 'C' }, 'a')
  await settle()
  assert.deepEqual(texts(), ['Root', 'A', 'C', 'B'])
  // One read, of the parent whose links changed — the new entity arrives on the
  // back of it, since a scan reads a couple of layers past what it was asked for.
  assert.deepEqual(source.scans.slice(before), [['a']])
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
