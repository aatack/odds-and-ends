// What each type of entity is drawn with: which have a focus of their own, and
// what a card is edited with.
//
//   npm test

import assert from 'node:assert/strict'
import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'

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
const warn = console.error
console.error = (...args: unknown[]) => {
  if (String(args[0]).includes('useLayoutEffect does nothing on the server')) return
  warn(...args)
}

const { viewsFor, TypeCard } = await import('../src/renderer/src/components/types')
const { OWN_FOCUS_TYPES, defaultLayout } = await import('../src/renderer/src/state/types')
const { layoutAtom } = await import('../src/renderer/src/state/store')
const R = await import('../src/renderer/src/state/reducers')
const { DetailView } = await import('../src/renderer/src/layout/DetailView')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])
const noop = (): void => undefined

test('names in OWN_FOCUS_TYPES exactly the types that draw a focus of their own', () => {
  for (const type of ['file', 'url', 'note', 'recipe', 'type']) {
    assert.equal(OWN_FOCUS_TYPES.includes(type), !!viewsFor(type).Focus, type)
  }
})

const row = (over: Record<string, unknown>) =>
  ({
    kind: 'entity',
    id: 'a',
    depth: 0,
    path: ['a'],
    parentId: null,
    hasChildren: false,
    collapsed: false,
    hidesChildren: false,
    loading: false,
    selected: false,
    editing: false,
    ...over,
  }) as never

test('gives files and pages a focus of their own, and every other type the default', () => {
  assert.ok(viewsFor('file').Focus)
  assert.ok(viewsFor('url').Focus)
  assert.equal(viewsFor(undefined).Focus, undefined, 'a note is its outline')
  // A custom type has no views of its own: it is a note, outline and all.
  assert.equal(viewsFor('recipe'), viewsFor(undefined))
})

test('edits a card in a text box over its draft, whatever its type draws', () => {
  for (const type of [undefined, 'file', 'url']) {
    const html = renderToStaticMarkup(
      h(TypeCard, {
        row: row({ type, editing: true, draft: 'typing', mimeType: 'image/png' }),
        onDraft: noop,
        onCommit: noop,
      }),
    )
    assert.match(html, /<textarea[^>]*>typing<\/textarea>/, `type ${type ?? 'none'}`)
  }
})

test('draws a detail frame as a form, a text box for an entity with no schema', () => {
  let s = defaultLayout('root')
  const tabId = Object.keys(s.tabs)[0]
  s = R.pushFrame(s, tabId, 'a', 'detail')
  const frameId = s.tabs[tabId].top
  layoutAtom.set(R.setDetailDraft(s, frameId, { text: 'typing' }))
  const html = renderToStaticMarkup(h(DetailView, { frameId }))
  assert.match(html, /<textarea[^>]*>typing<\/textarea>/)
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
