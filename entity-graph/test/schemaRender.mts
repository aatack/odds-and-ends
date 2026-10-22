// The schema builder and form, rendered to markup with no DOM: enough to see
// that nested schemas draw nested controls.
//
//   npm test

import assert from 'node:assert/strict'
import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'

const { SchemaBuilder } = await import('../src/renderer/src/components/schema/SchemaBuilder')
const { SchemaForm } = await import('../src/renderer/src/components/schema/SchemaForm')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])
const noop = (): void => undefined

const NESTED = {
  type: 'object',
  properties: {
    a: { type: 'array', items: { type: 'object', properties: { b: { type: 'string' } } } },
  },
}

test('draws a field inside a list inside an object', () => {
  const html = renderToStaticMarkup(h(SchemaBuilder, { value: NESTED, setValue: noop }))
  assert.match(html, /value="a"/)
  assert.match(html, /value="b"/)
})

test('draws a union option by option, and a literal with its value', () => {
  const html = renderToStaticMarkup(
    h(SchemaBuilder, { value: { anyOf: [{ const: 'main' }, { type: 'integer' }] }, setValue: noop }),
  )
  assert.match(html, /Option 1/)
  assert.match(html, /value="main"/)
  assert.match(html, />integer</)
})

test('reads an enum as a union of literals', () => {
  const html = renderToStaticMarkup(h(SchemaBuilder, { value: { enum: ['a', 'b'] }, setValue: noop }))
  assert.match(html, />union</)
  assert.match(html, /value="b"/)
})

test('draws a union of literals in the form as one choice', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, { schema: { anyOf: [{ const: 'x' }, { const: 'y' }] }, value: 'y', setValue: noop }),
  )
  assert.match(html, /<option value="1" selected="">y<\/option>/)
})

test('picks the union option a value fits', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, { schema: { anyOf: [{ type: 'string' }, { type: 'integer' }] }, value: 4, setValue: noop }),
  )
  assert.match(html, /<option value="1" selected="">integer<\/option>/)
  assert.match(html, /value="4"/)
})

test('puts the kind in the name box, and an extra under it once ticked', () => {
  const html = renderToStaticMarkup(
    h(SchemaBuilder, {
      value: { type: 'object', properties: { n: { type: 'number', minimum: 2, description: 'How many' } } },
      setValue: noop,
    }),
  )
  assert.match(html, /value="n"/)
  assert.match(html, />number</)
  assert.match(html, /Minimum/)
  assert.match(html, /value="2"/)
  assert.match(html, /value="How many"/)
  // Not required, so the chip says so.
  assert.match(html, />\?</)
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
