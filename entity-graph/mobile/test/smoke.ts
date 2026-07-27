// Drives the app with no browser at all: a stub for localStorage, an in-memory
// source over HTTP, and then nothing but the state and tools layers.
//
// That this runs is itself the assertion the architecture makes — `state/` and
// `tools/` hold no React and touch no DOM, so the app can be driven headlessly. What
// it checks beyond that is the logic worth checking: where a new line lands among its
// siblings, that a chained entry keeps its order, that indent and outdent move a row
// where they say, and that one undo takes a whole action back off rather than half of
// it.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource, serve } from './source'

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

// Imported after the stub: the persistent atoms read localStorage as they are built.
const { connectionAtom, capabilitiesAtom } = await import('../src/source/connection')
const { startQueryEngine, refreshQueries, viewRows } = await import('../src/state/query')
const { viewAtom } = await import('../src/state/store')
const { defaultView } = await import('../src/state/types')
const A = await import('../src/state/actions')
const { runTool } = await import('../src/tools/dispatch')
const { toastsAtom } = await import('../src/state/toast')

// --- The source -------------------------------------------------------------

const source = new MemorySource()
const at = 1_000_000
const seed = (
  id: string,
  text: string,
  parent: string | null,
  values: Record<string, unknown> = {},
): void => {
  source.events.push({ type: 'value', timestamp: at, author: 'seed', entityId: id, key: 'text', value: text })
  for (const [key, value] of Object.entries(values)) {
    source.events.push({ type: 'value', timestamp: at, author: 'seed', entityId: id, key, value })
  }
  if (parent) {
    source.events.push({
      type: 'link',
      timestamp: at,
      author: 'seed',
      sourceId: parent,
      destinationId: id,
      action: 0,
    })
  }
}

seed('@index', 'Index', null)
seed('a', 'Apples', '@index')
seed('b', 'Bananas', '@index')
seed('c', 'Cherries', '@index')

const harness = await serve(source, 'tok')

connectionAtom.set({ baseUrl: harness.baseUrl, sourceId: 'demo', token: 'tok', author: 'phone' })
capabilitiesAtom.set(['scanEvents', 'writeValue', 'writeLink', 'writeEvents', 'popEvents'])
viewAtom.set(defaultView())
startQueryEngine()

// --- Helpers ----------------------------------------------------------------

/** Wait until nothing is in flight, so the rows on screen are the store's. */
async function settle(): Promise<void> {
  for (let i = 0; i < 200; i++) {
    await new Promise((r) => setTimeout(r, 10))
    // Reading is what asks for anything missing, so this both waits and drives.
    const { loading, complete } = viewRows()
    if (!loading && complete) return
  }
  throw new Error('The cache never settled')
}

/** The rows on screen, as `depth:text`. */
const shape = (): string[] =>
  viewRows()
    .rows.map((r) => `${'  '.repeat(r.depth)}${r.kind === 'input' ? '⟨input⟩' : (r.text ?? '')}`)

/** The ordered children of an entity, by text, straight from the store. */
const childTexts = (id: string): string[] =>
  source.entity(id).outboundLinks.map((child) => String(source.entity(child).values.text ?? ''))

const errors = (): string[] =>
  toastsAtom
    .get()
    .filter((t) => t.kind === 'error')
    .map((t) => t.message)

/** Type into whatever box is open, then run a tool over it. */
async function type(text: string, tool: 'edit.commit' | 'edit.commitAndNext'): Promise<void> {
  A.setDraft(text)
  await runTool(tool)
  await settle()
}

/**
 * Long enough that the next write is a separate *action*.
 *
 * Undo takes the most recent event and anything within 100ms of it, which is what
 * makes one user action one step — a create writes its values and its link at the
 * same instant and they must come off together. The flip side is that two actions in
 * quick succession collapse into one, which is correct and only surprising if you
 * drive the app faster than a person can. A test does exactly that.
 */
const pause = (ms = 150): Promise<void> => new Promise((r) => setTimeout(r, ms))

let checks = 0
const check = (what: string, fn: () => void): void => {
  fn()
  checks++
  console.log(`  ✓ ${what}`)
}

// --- The run ----------------------------------------------------------------

console.log('\nheadless drive of the mobile client\n')

await settle()
check('the seeded tree arrives', () => {
  assert.deepEqual(shape(), ['Index', '  Apples', '  Bananas', '  Cherries'])
  assert.deepEqual(errors(), [])
})

// A line added below the selected row lands there, not at the end of the list —
// which is the point of `forwardSteps`.
A.selectPath(['@index', 'b'])
await runTool('create.sibling')
check('the input row appears under the row it will follow', () => {
  assert.deepEqual(shape(), ['Index', '  Apples', '  Bananas', '  ⟨input⟩', '  Cherries'])
})
await type('Blackcurrants', 'edit.commit')
check('a new line lands directly below the one it was added under', () => {
  assert.deepEqual(childTexts('@index'), ['Apples', 'Bananas', 'Blackcurrants', 'Cherries'])
})

// The events go into the cache on their way out, so the line is on screen before
// the write has been answered — which is the whole point of the cache on a phone.
A.selectPath(['@index', 'c'])
await runTool('create.sibling')
A.setDraft('Dates')
const committing = runTool('edit.commit')
check('a new line is on screen before the write comes back', () => {
  assert.deepEqual(shape(), [
    'Index',
    '  Apples',
    '  Bananas',
    '  Blackcurrants',
    '  Cherries',
    '  Dates',
  ])
})
await committing
await settle()

// Chained entry: commit and keep going, twice, and the order has to hold.
A.selectPath(['@index', 'a'])
await runTool('create.sibling')
await type('Apricots', 'edit.commitAndNext')
await type('Avocados', 'edit.commitAndNext')
await runTool('edit.cancel')
check('chained entry keeps each line after the last', () => {
  assert.deepEqual(childTexts('@index'), [
    'Apples',
    'Apricots',
    'Avocados',
    'Bananas',
    'Blackcurrants',
    'Cherries',
    'Dates',
  ])
})

// A child, then indent and outdent it.
A.selectPath(['@index', 'b'])
await runTool('create.child')
await type('Plantain', 'edit.commit')
check('a child lands under its parent', () => {
  assert.deepEqual(childTexts('b'), ['Plantain'])
})

const plantain = source.entity('b').outboundLinks[0]
A.selectPath(['@index', 'b'])
await runTool('create.child')
await type('Green', 'edit.commit')
const green = source.entity('b').outboundLinks[1]
A.selectPath(['@index', 'b', green])
await runTool('entity.indent')
await settle()
check('indent moves a row under the one above it', () => {
  assert.deepEqual(childTexts('b'), ['Plantain'])
  assert.deepEqual(childTexts(plantain), ['Green'])
})

A.selectPath(['@index', 'b', plantain, green])
await runTool('entity.outdent')
await settle()
check('outdent puts it back, directly after the parent it came out of', () => {
  assert.deepEqual(childTexts('b'), ['Plantain', 'Green'])
})

// Reordering.
A.selectPath(['@index', 'b', green])
await runTool('entity.moveUp')
await settle()
check('move up swaps a row with its previous sibling', () => {
  assert.deepEqual(childTexts('b'), ['Green', 'Plantain'])
})

// Checkboxes cycle: plain → open → ticked → plain.
A.selectPath(['@index', 'a'])
await runTool('toggle.checkbox')
await settle()
check('ticking an untouched row gives it an open box', () => {
  assert.equal(source.entity('a').values.open, true)
})
await pause()
await runTool('toggle.checkbox')
await settle()
check('again, and it is ticked', () => {
  assert.equal(source.entity('a').values.open, false)
})

// Undo takes the whole of the last action back, and redo puts it back verbatim.
await pause()
const beforeUndo = source.events.length
await runTool('app.undo')
await settle()
check('undo removes the events of the last action', () => {
  assert.ok(source.events.length < beforeUndo)
  assert.equal(source.entity('a').values.open, true)
})
await runTool('app.redo')
await settle()
check('redo writes them back', () => {
  assert.equal(source.entity('a').values.open, false)
  assert.equal(source.events.length, beforeUndo)
})

// A create is several events at one instant, so one undo takes all of it.
await pause()
A.selectPath(['@index', 'c'])
await runTool('create.sibling')
await type('Damsons', 'edit.commit')
check('the new line is there', () => {
  assert.ok(childTexts('@index').includes('Damsons'))
})
await runTool('app.undo')
await settle()
check('one undo takes a whole create back, values and link together', () => {
  assert.ok(!childTexts('@index').includes('Damsons'))
})

// Removing a row from its parent leaves the selection where the eye is.
A.selectPath(['@index', 'a'])
await runTool('entity.unlink')
await settle()
check('unlink removes a row from its parent', () => {
  assert.ok(!childTexts('@index').includes('Apples'))
  assert.deepEqual(viewRows().selectedPath, ['@index'])
})

// Filters are over the rows, not the query.
A.setFind('an')
check('find keeps matching rows and their ancestors', () => {
  const rows = shape()
  assert.ok(rows.some((r) => r.includes('Bananas')))
  assert.ok(rows.some((r) => r.includes('Plantain')))
  assert.ok(!rows.some((r) => r.includes('Cherries')))
})
A.setFind(null)

// Folding is a derivation over the cache, so it takes effect with no round trip.
const callsBefore = source.calls
A.toggleCollapse('b')
check('folding hides a subtree with no round trip', () => {
  assert.ok(!shape().some((r) => r.includes('Plantain')))
  assert.equal(source.calls, callsBefore)
})
A.toggleCollapse('b')

// Nothing should have gone wrong along the way.
check('no tool reported an error', () => {
  assert.deepEqual(errors(), [])
})

refreshQueries()
await settle()

await harness.close()
console.log(`\n${checks} checks passed\n`)
process.exit(0)
