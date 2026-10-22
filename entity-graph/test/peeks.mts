// Peeks, as the layout holds them: which survive a hover moving on, which the
// keys keep, and where the keys go while one has them.
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

const P = await import('../src/renderer/src/state/peeks')
const { defaultLayout, peeksOf } = await import('../src/renderer/src/state/types')
const { focusOf } = await import('../src/renderer/src/state/store')

const AT = { x: 10, y: 10, width: 40, height: 16 }
const entity = (id: string): string => id

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('gives an entity peek a tab and a frame of its own, in no group', () => {
  const s = P.showPeek(defaultLayout('root'), entity('a'), AT, null)
  const [peek] = peeksOf(s)
  const tab = s.tabs[peek.tabId!]
  assert.equal(s.frames[tab.top].rootId, 'a')
  assert.ok(Object.values(s.groups).every((g) => !g.tabIds.includes(tab.id)))
})

test('replaces a transient peek with the next hover, frames and all', () => {
  const one = P.showPeek(defaultLayout('root'), entity('a'), AT, null)
  const two = P.showPeek(one, entity('b'), AT, null)
  assert.deepEqual(peeksOf(two).map((p) => p.entityId), ['b'])
  assert.equal(two.tabs[peeksOf(one)[0].tabId!], undefined)
  assert.equal(Object.keys(two.frames).length, Object.keys(one.frames).length)
})

test('stacks a peek opened from inside another, and closes both from the bottom', () => {
  const one = P.showPeek(defaultLayout('root'), entity('a'), AT, null)
  const parent = peeksOf(one)[0].key
  const two = P.showPeek(one, entity('b'), AT, parent)
  assert.equal(peeksOf(two).length, 2)
  // The pointer is back in the first: the one stacked on it goes.
  assert.equal(peeksOf(P.keepHovered(two, parent)).length, 1)
  // And in neither.
  assert.equal(peeksOf(P.keepHovered(two, null)).length, 0)
  assert.equal(peeksOf(P.closePeek(two, parent)).length, 0)
})

test('keeps a pinned peek whatever the pointer does', () => {
  const one = P.showPeek(defaultLayout('root'), entity('a'), AT, null)
  const pinned = P.placePeek(one, peeksOf(one)[0].key, { x: 0, y: 0, width: 300, height: 200 })
  assert.equal(peeksOf(P.keepHovered(pinned, null)).length, 1)
  assert.equal(peeksOf(P.showPeek(pinned, entity('b'), AT, null)).length, 2)
  // And it is all that survives a reload.
  assert.equal(peeksOf(P.settlePeeks(P.showPeek(pinned, entity('b'), AT, null))).length, 1)
})

test('points the keys at the peek holding them, and keeps it open while they are there', () => {
  const base = defaultLayout('root')
  const one = P.showPeek(base, entity('a'), AT, null)
  const [peek] = peeksOf(one)
  const focused = P.focusPeek(one, peek.key)
  const at = focusOf(focused)
  assert.equal(at.groupId, null, 'a peek belongs to no group')
  assert.equal(at.tabId, peek.tabId)
  assert.equal(focused.frames[at.frameId!].rootId, 'a')
  assert.equal(peeksOf(P.keepHovered(focused, null)).length, 1)
  // Back to the main view once they leave.
  const back = P.focusPeek(focused, null)
  assert.deepEqual(focusOf(back), focusOf(base))
})

let failed = 0
for (const [name, run] of tests) {
  try {
    run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
