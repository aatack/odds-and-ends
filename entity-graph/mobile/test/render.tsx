// Renders the whole view layer to a string, against the in-memory source.
//
// Not a substitute for holding a phone, but it catches the things that break a build
// silently at the edges — a component that doesn't exist, an icon that isn't exported,
// a hook called in the wrong order, a screen that throws on empty state — and it
// asserts that the rows, the crumb trail and the bar under the thumb all say what they
// should before anyone opens it on a device.
//
//   npm test

import assert from 'node:assert/strict'
import React from 'react'
import { renderToString } from 'react-dom/server'
import { MemorySource, serve } from './source'

// --- The browser bits the view layer expects --------------------------------
//
// Effects don't run in a server render, so this is a much shorter list than a real
// DOM: what's needed is what runs while *rendering*.

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})

// React warns that useLayoutEffect does nothing on the server. True, and not
// interesting here — the outline's growing textarea is the only user of it.
const warn = console.error
console.error = (...args: unknown[]): void => {
  if (typeof args[0] === 'string' && args[0].includes('useLayoutEffect')) return
  warn(...args)
}

const { connectionAtom, capabilitiesAtom } = await import('../src/source/connection')
const { startQueryEngine, viewRows } = await import('../src/state/query')
const { viewAtom } = await import('../src/state/store')
const { defaultView } = await import('../src/state/types')
const A = await import('../src/state/actions')
const { openSheet } = await import('../src/state/ui')
const { App } = await import('../src/views/App')

// --- A tree to draw ---------------------------------------------------------

const source = new MemorySource()
const at = 1_000_000
const seed = (id: string, text: string, parent: string | null, values: Record<string, unknown> = {}): void => {
  source.events.push({ type: 'value', timestamp: at, author: 'seed', entityId: id, key: 'text', value: text })
  for (const [key, value] of Object.entries(values)) {
    source.events.push({ type: 'value', timestamp: at, author: 'seed', entityId: id, key, value })
  }
  if (parent) {
    source.events.push({ type: 'link', timestamp: at, author: 'seed', sourceId: parent, destinationId: id, action: 0 })
  }
}

seed('@index', 'Index', null)
seed('shopping', 'Shopping', '@index', { section: true })
seed('coffee', 'Coffee', 'shopping', { open: true })
seed('oats', 'Oats', 'shopping', { open: false })
seed('ideas', 'Ideas', '@index')
seed('snippet', 'const x = 2', 'ideas', { type: 'code' })
seed('note', 'Some **bold** and [a link](https://example.com)', 'ideas')

const harness = await serve(source, 'tok')

let checks = 0
const check = (what: string, fn: () => void): void => {
  fn()
  checks++
  console.log(`  ✓ ${what}`)
}

const html = (): string => renderToString(<App />)

const settle = async (): Promise<void> => {
  for (let i = 0; i < 200; i++) {
    await new Promise((r) => setTimeout(r, 10))
    // Reading is what asks for anything missing, so this both waits and drives.
    const { loading, complete } = viewRows()
    if (!loading && complete) return
  }
  throw new Error('The cache never settled')
}

console.log('\nserver render of the mobile client\n')

// --- Unconfigured -----------------------------------------------------------

connectionAtom.set(null)
viewAtom.set(defaultView())
check('with no source, the setup screen is what you get', () => {
  const out = html()
  assert.match(out, /Entity Graph/)
  assert.match(out, /Server/)
  assert.match(out, /Token/)
  assert.doesNotMatch(out, /Nothing here yet/)
})

// --- Connected --------------------------------------------------------------

connectionAtom.set({ baseUrl: harness.baseUrl, sourceId: 'demo', token: 'tok', author: 'phone' })
capabilitiesAtom.set(['scanEvents', 'writeValue', 'writeLink', 'writeEvents', 'popEvents'])
startQueryEngine()
await settle()

check('the outline draws the tree, checkboxes and code and all', () => {
  const out = html()
  assert.match(out, /Shopping/)
  assert.match(out, /Coffee/)
  assert.match(out, /Oats/)
  assert.match(out, /const x = 2/)
})

check('entity text is rendered, and its links leave the app alone', () => {
  const out = html()
  assert.match(out, /<strong>bold<\/strong>/)
  assert.match(out, /href="https:\/\/example\.com"/)
  // A link that navigated this tab would replace the app itself, once installed.
  assert.match(out, /target="_blank"/)
})

check('the bar under the thumb offers the four writes and the menu', () => {
  const out = html()
  for (const caption of ['Open', 'Below', 'Child', 'Edit', 'More']) {
    assert.ok(out.includes(caption), `the bar should offer "${caption}"`)
  }
})

check('undo is offered, because this source can pop events', () => {
  assert.match(html(), /aria-label="Undo"/)
})

check('and is not offered by a source that cannot', () => {
  capabilitiesAtom.set(['scanEvents', 'writeValue'])
  assert.doesNotMatch(html(), /aria-label="Undo"/)
  capabilitiesAtom.set(['scanEvents', 'writeValue', 'writeLink', 'writeEvents', 'popEvents'])
})

// --- Drilling in ------------------------------------------------------------

A.pushLevel('shopping')
await settle()
check('drilling in retitles the header and shows the trail behind it', () => {
  const out = html()
  assert.match(out, /Shopping/)
  assert.match(out, /Index/)
})

// --- Editing ----------------------------------------------------------------

A.selectPath(['shopping', 'coffee'])
A.startEdit(['shopping', 'coffee'], 'Coffee')
check('the bar becomes the editing bar while a box is open', () => {
  const out = html()
  assert.match(out, /Done/)
  assert.match(out, /Another/)
  assert.match(out, /Cancel/)
})
A.setEdit(null)

// --- Filters ----------------------------------------------------------------

A.setFind('oat')
check('find puts a field on screen and filters the rows behind it', () => {
  const out = html()
  assert.match(out, /Find in these rows/)
  assert.match(out, /Oats/)
  assert.doesNotMatch(out, />Coffee</)
})
A.setFind(null)

A.setSectionsOnly(true)
check('a filter in force says so, with a pill that clears it', () => {
  assert.match(html(), /Sections only/)
})
A.setSectionsOnly(false)

// --- Sheets -----------------------------------------------------------------

openSheet({ kind: 'actions' })
check('the action sheet lists what applies to the selection', () => {
  const out = html()
  assert.match(out, /Search actions/)
  assert.match(out, /Tick \/ untick/)
  assert.match(out, /Copy as markdown/)
})

check('and leads with adding, which is what the app is mostly for', () => {
  const out = html()
  assert.ok(out.indexOf('Create') < out.indexOf('Navigate'), 'Create should come first')
  assert.ok(out.indexOf('Add a child') < out.indexOf('Edit text'), 'adding before editing')
})

openSheet({ kind: 'settings' })
check('settings shows what it is connected to', () => {
  const out = html()
  assert.match(out, /Settings/)
  assert.match(out, /Disconnect/)
  assert.ok(out.includes('demo'))
})

openSheet({ kind: 'args', toolId: 'entity.value.set', args: { entityId: 'coffee' } })
check('a tool that needs typing opens a form with every argument on it', () => {
  const out = html()
  assert.match(out, /Set a value/)
  assert.match(out, /Key/)
  assert.match(out, /Value \(JSON\)/)
})

openSheet({ kind: 'pick', toolId: 'entity.move', args: {}, argName: 'toParentId', prompt: 'Tap the new parent' })
check('picking leaves the outline up, with a banner over it', () => {
  const out = html()
  assert.match(out, /Tap the new parent/)
  // Still readable underneath: that is the whole point of picking in place.
  assert.match(out, /Coffee/)
})

await harness.close()
console.log(`\n${checks} checks passed\n`)
process.exit(0)
