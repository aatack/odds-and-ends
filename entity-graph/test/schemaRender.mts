// The schema builder and form, rendered to markup with no DOM: enough to see
// that nested schemas draw nested controls.
//
//   npm test

import assert from 'node:assert/strict'
import { createElement as h } from 'react'
import { renderToStaticMarkup } from 'react-dom/server'

// Rendering to a string runs no layout effects, and React says so for each one.
const warn = console.error
console.error = (...args: unknown[]) => {
  if (String(args[0]).includes('useLayoutEffect does nothing on the server')) return
  warn(...args)
}

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
  assert.match(html, />Integer</)
})

test('reads an enum as a union of literals', () => {
  const html = renderToStaticMarkup(h(SchemaBuilder, { value: { enum: ['a', 'b'] }, setValue: noop }))
  assert.match(html, />Union</)
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
  assert.match(html, /<option value="1" selected="">Integer<\/option>/)
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
  assert.match(html, />Number</)
  assert.match(html, /Minimum/)
  assert.match(html, /value="2"/)
  assert.match(html, /value="How many"/)
  // Not required, so the chip says so.
  assert.match(html, />\?</)
})

test('folds a schema that holds others, and only that', () => {
  const html = renderToStaticMarkup(h(SchemaBuilder, { value: NESTED, setValue: noop }))
  // The root, the list and the object in it hold schemas; the string `b` does not.
  assert.equal((html.match(/title="Fold"/g) ?? []).length, 3)
})

test('draws the form as one box of lines, nested content inside its line', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, {
      schema: {
        type: 'object',
        properties: {
          title: { type: 'string' },
          ingredients: { type: 'array', items: { type: 'object', properties: { name: { type: 'string' } } } },
        },
        required: ['title'],
      },
      value: { title: 'Soup', ingredients: [{ name: 'leek' }] },
      setValue: noop,
    }),
  )
  // One bordered surface, however deep the value goes.
  assert.equal((html.match(/border-gray-200/g) ?? []).length, 1)
  assert.match(html, /value="Soup"/)
  assert.match(html, /value="leek"/)
  // No summary lines: a list is its items and the line for adding one.
  assert.doesNotMatch(html, />\d+ (items?|fields?)</)
  assert.match(html, /> Add</)
  // Labels inside another line take the narrower column.
  assert.equal((html.match(/w-36/g) ?? []).length, 2)
  assert.ok((html.match(/w-24/g) ?? []).length >= 2)
})

test('marks a line whose value breaks its schema, and draws a multi-line string as a text box', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, {
      schema: {
        type: 'object',
        properties: {
          servings: { type: 'integer', minimum: 1 },
          method: { type: 'string', 'x-multiline': true },
        },
      },
      value: { servings: 0, method: 'Boil.' },
      setValue: noop,
    }),
  )
  assert.match(html, /At least 1/)
  assert.match(html, /<textarea[^>]*>Boil\.<\/textarea>/)
})

test('draws a schema-valued field with the builder, and code in monospace', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, {
      schema: {
        type: 'object',
        properties: {
          shape: { $ref: 'https://json-schema.org/draft/2020-12/schema' },
          code: { type: 'string', 'x-monospace': true },
        },
      },
      value: { shape: { type: 'object', properties: { inner: { type: 'boolean' } } }, code: 'x = 1' },
      setValue: noop,
    }),
  )
  // The builder's own name box for the inner field, and its kind.
  assert.match(html, /value="inner"/)
  assert.match(html, />Boolean</)
  assert.match(html, /font-mono[^"]*" value="x = 1"/)
})

test('offers no Any, and asks for a type where a schema says nothing', () => {
  const html = renderToStaticMarkup(h(SchemaBuilder, { value: {}, setValue: noop }))
  assert.match(html, />Choose type</)
  assert.doesNotMatch(html, />any</i)
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
