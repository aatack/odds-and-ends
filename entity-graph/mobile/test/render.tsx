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
import { BUILTIN_VALUES, TYPE_ID } from '../../src/core/builtins'
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
seed('snippet', 'A snippet:\n\n```\nconst x = 2\n```', 'ideas')
seed('note', 'Some **bold** and [a link](https://example.com)', 'ideas')
// A link whose protocol is not one a tap may follow: it draws as a pill, but with no
// href to go to. react-markdown sanitises it too; the pill's own gate is the belt.
seed('danger', 'A [bad link](javascript:alert(1)) in a note', 'ideas')
// A url entity, as the store serves one: its id is `url:<address>`, `type: 'url'`,
// the address in its `url` value, and nothing written as text — so it must draw its
// address rather than "Empty".
seed('url:https://example.org/page', '', 'ideas', {
  type: 'url',
  url: 'https://example.org/page',
})
// A custom type and one instance of it, as the store serves them: the type says
// `type: type` and carries a schema; the detail form is drawn from that schema.
seed('recipe', 'Recipe', '@index', {
  type: 'type',
  schema: {
    type: 'object',
    properties: {
      source: { type: 'string', title: 'Source', description: 'Where it came from' },
      servings: { type: 'integer', description: 'How many it feeds' },
      vegetarian: { type: 'boolean', description: 'Meat-free?' },
      nutrition: { type: 'object', properties: { calories: { type: 'integer' } } },
    },
    required: ['source'],
  },
})
seed('pancakes', 'Pancakes', '@index', {
  type: 'recipe',
  source: 'Grandma',
  servings: 4,
  vegetarian: true,
  nutrition: { calories: 300 },
})
// The store-served type of types, seeded here as the real store would serve it
// (`core/builtins` BUILTIN_VALUES): its schema names `text`, `schema` and `onLoad`,
// so its own detail form draws an `onLoad` code box and a read-only `schema`.
seed(TYPE_ID, String(BUILTIN_VALUES[TYPE_ID].text ?? 'Type'), '@index', {
  type: BUILTIN_VALUES[TYPE_ID].type,
  schema: BUILTIN_VALUES[TYPE_ID].schema,
})

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

check('the outline draws the tree, checkboxes and a fenced block and all', () => {
  const out = html()
  assert.match(out, /Shopping/)
  assert.match(out, /Coffee/)
  assert.match(out, /Oats/)
  // A fenced code block in an ordinary note — the `code` type is gone, so this is
  // how code reads now: plain monospace through the markdown, not a special row.
  assert.match(out, /<pre><code/)
  assert.match(out, /const x = 2/)
})

check('entity text is rendered, and a link is a pill that leaves the app alone', () => {
  const out = html()
  assert.match(out, /<strong>bold<\/strong>/)
  // Drawn as a pill: an anchor with its own surface, not a bare underlined link.
  assert.match(
    out,
    /<a href="https:\/\/example\.com" target="_blank" rel="noreferrer" class="[^"]*inline-flex[^"]*"/,
  )
  // A link that navigated this tab would replace the app itself, once installed.
  assert.match(out, /target="_blank"/)
})

check('a dangerous link draws a pill with no javascript: href', () => {
  const out = html()
  // The pill is there (its words survive); the protocol never becomes something tappable.
  assert.match(out, /bad link/)
  assert.doesNotMatch(out, /href="javascript:/)
})

check('a url row draws its address rather than "Empty"', () => {
  const out = html()
  assert.match(out, /https:\/\/example\.org\/page/)
  // The regression this guards: a url is textless, and used to fall through to the
  // note placeholder. The url row is the only textless one seeded, so no "Empty"
  // anywhere means the url drew its address instead.
  assert.doesNotMatch(out, /Empty/)
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

openSheet({ kind: 'details', entityId: 'pancakes' })
check('the detail form draws a field per schema property, by widget', () => {
  const out = html()
  assert.match(out, /Pancakes/) // the sheet's title is the entity's label
  assert.match(out, /Recipe/) // and its type's name beneath
  assert.match(out, /Source/) // a property's `title`, with its description
  assert.match(out, /Where it came from/)
  assert.match(out, /servings/) // a property with no title shows its key
  assert.match(out, /vegetarian/)
  assert.match(out, /Yes/) // the boolean, true, as a word
  assert.match(out, /nutrition/)
  assert.match(out, /calories/) // the object property, as read-only JSON
  assert.match(out, /desktop/) // and the note that it is edited there
})

openSheet({ kind: 'details', entityId: 'type' })
check('a type entity edits onLoad as a monospace code box and schema read-only', () => {
  const out = html()
  assert.match(out, /onLoad/) // the field label
  assert.match(out, /<textarea/) // drawn as a growing multi-line editor
  assert.match(out, /font-mono/) // in monospace (x-monospace)
  assert.match(out, /json-schema\.org/) // the schema field shown as read-only JSON
  assert.match(out, /Edited on the desktop app/)
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
