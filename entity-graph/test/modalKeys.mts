// A dialog owns the keyboard: the router hands nothing to the frames behind it,
// and Escape closes the innermost dialog only.
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
// The router asks whether the key came from a text field; nothing here is one.
Object.defineProperty(globalThis, 'HTMLElement', { value: class {} })

const { handleKey } = await import('../src/renderer/src/tools/dispatch')
const { openModal } = await import('../src/renderer/src/state/ui')
const { layoutAtom } = await import('../src/renderer/src/state/store')
const { defaultLayout } = await import('../src/renderer/src/state/types')

const key = (k: string) =>
  ({ key: k, ctrlKey: false, metaKey: false, altKey: false, shiftKey: false, target: null }) as unknown as KeyboardEvent

let passed = 0
const test = (name: string, run: () => void): void => {
  run()
  console.log(`  ok  ${name}`)
  passed++
}

test('leaves every key but Escape to an open dialog', () => {
  layoutAtom.set(defaultLayout('root'))
  const before = JSON.stringify(layoutAtom.get())
  const release = openModal(() => undefined)
  assert.equal(handleKey(key('s')), false, 'a frame key is not consumed')
  assert.equal(handleKey(key('Backspace')), false)
  assert.equal(JSON.stringify(layoutAtom.get()), before, 'and nothing behind moved')
  release()
})

test('closes only the innermost dialog on Escape', () => {
  const closed: string[] = []
  const outer = openModal(() => closed.push('outer'))
  const inner = openModal(() => closed.push('inner'))
  assert.equal(handleKey(key('Escape')), true)
  assert.deepEqual(closed, ['inner'])
  inner()
  outer()
})

console.log(`\n${passed} passed`)
