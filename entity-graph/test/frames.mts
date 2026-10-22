// A tab's stack of frames, as the layout holds it: a chain run down from the
// tab's top through each frame's `below`, and the history a pop leaves to undo.
// And the URL entities a link peeks at, which the store supplies unwritten.
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

const R = await import('../src/renderer/src/state/reducers')
const { defaultLayout, isLayoutState, stackOf } = await import('../src/renderer/src/state/types')
const { builtinEvents, urlEntityId } = await import('../src/core/builtins')

type Layout = ReturnType<typeof defaultLayout>

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

const tabOf = (s: Layout): string => Object.keys(s.tabs)[0]
const roots = (s: Layout, tabId: string): string[] =>
  stackOf(s, tabId).map((id) => s.frames[id].rootId)

/** A tab with a frame per root pushed in turn, on a layout rooted at `root`. */
function stacked(...ids: string[]): { s: Layout; tabId: string } {
  let s = defaultLayout('root')
  const tabId = tabOf(s)
  for (const id of ids) s = R.pushFrame(s, tabId, id)
  return { s, tabId }
}

test('pushes focus frames onto the one below, and reads them bottom first', () => {
  const { s, tabId } = stacked('a', 'b')
  assert.deepEqual(roots(s, tabId), ['root', 'a', 'b'])
  const top = s.frames[s.tabs[tabId].top]
  assert.equal(top.type, 'focus')
  assert.equal(s.frames[top.below!].rootId, 'a')
  assert.ok(isLayoutState(s))
})

test('pops to the frame below, never past the bottom, and undoes the pop', () => {
  const { s, tabId } = stacked('a')
  const popped = R.popFrame(s, tabId)
  assert.deepEqual(roots(popped, tabId), ['root'])
  assert.equal(R.popFrame(popped, tabId), popped)
  assert.deepEqual(roots(R.undoPop(popped, tabId), tabId), ['root', 'a'])
})

test('pops back to a crumb, and undoes out through the frames in the order opened', () => {
  const { s, tabId } = stacked('a', 'b', 'c')
  const bottom = stackOf(s, tabId)[0]
  let t = R.popToFrame(s, tabId, bottom)
  assert.deepEqual(roots(t, tabId), ['root'])
  t = R.undoPop(t, tabId)
  assert.deepEqual(roots(t, tabId), ['root', 'a'])
  t = R.undoPop(R.undoPop(t, tabId), tabId)
  assert.deepEqual(roots(t, tabId), ['root', 'a', 'b', 'c'])
})

test('a push drops the frames left to redo', () => {
  const { s, tabId } = stacked('a')
  const gone = s.tabs[tabId].top
  const t = R.pushFrame(R.popFrame(s, tabId), tabId, 'b')
  assert.equal(t.frames[gone], undefined)
  assert.deepEqual(roots(R.undoPop(t, tabId), tabId), ['root', 'b'])
})

test('moves the top frame into a tab of its own, at the bottom of its stack', () => {
  const { s, tabId } = stacked('a')
  const groupId = s.groupOrder[0]
  const t = R.popIntoNewTab(s, groupId, tabId)
  const other = t.groups[groupId].activeTabId!
  assert.notEqual(other, tabId)
  assert.deepEqual(roots(t, tabId), ['root'])
  assert.deepEqual(roots(t, other), ['a'])
  assert.equal(t.frames[t.tabs[other].top].below, null)
})

test('an undone pop lands on whatever is on top now', () => {
  // root, a, b; pop b; move a into its own tab; undo: b goes back onto root.
  const { s, tabId } = stacked('a', 'b')
  const t = R.popIntoNewTab(R.popFrame(s, tabId), s.groupOrder[0], tabId)
  assert.deepEqual(roots(R.undoPop(t, tabId), tabId), ['root', 'b'])
})

test('pushes a detail frame with nothing of the focus view about it', () => {
  const { s, tabId } = stacked()
  const t = R.pushFrame(s, tabId, 'a', 'detail')
  const top = t.frames[t.tabs[tabId].top]
  assert.deepEqual(Object.keys(top).sort(), ['below', 'draft', 'id', 'rootId', 'tabId', 'type'])
  assert.equal(top.type, 'detail')
  assert.deepEqual(roots(R.popFrame(t, tabId), tabId), ['root'])
  assert.ok(isLayoutState(t))
})

test('reads a layout saved before frames were chained as no layout at all', () => {
  const old = { groups: {}, groupOrder: [], tabs: { t: { id: 't', frameIds: ['f'] } }, frames: {} }
  assert.equal(isLayoutState(old), false)
})

test('supplies a URL entity for any address, typed url, unwritten', () => {
  const id = urlEntityId('https://example.com/a')
  const values = Object.fromEntries(builtinEvents([id]).map((e) => [(e as { key: string }).key, (e as { value: unknown }).value]))
  assert.deepEqual(values, { type: 'url', url: 'https://example.com/a' })
  assert.deepEqual(builtinEvents(['url:']), [])
  assert.deepEqual(builtinEvents(['plain']), [])
})

let failed = 0
for (const [name, run] of tests) {
  try {
    run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.log(`fail  ${name}\n      ${(e as Error).message.split('\n').join('\n      ')}`)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
if (failed) process.exit(1)
