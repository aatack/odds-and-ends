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
  assert.match(html, /placeholder="main"/)
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
  assert.match(html, /<span class="min-w-0 flex-1 truncate text-gray-900">y<\/span>/)
})

test('picks the union option a value fits', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, { schema: { anyOf: [{ type: 'string' }, { type: 'integer' }] }, value: 4, setValue: noop }),
  )
  assert.match(html, /<span class="truncate">Integer<\/span>/)
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
  // The description is the form's own multi-line box.
  assert.match(html, /<textarea[^>]*>How many<\/textarea>/)
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
  // Each run of lines is a grid whose label column fits its longest label.
  assert.match(html, /grid-cols-\[max-content_minmax\(0,1fr\)\]/)
  assert.match(html, /grid-cols-subgrid/)
  // An item's remove is a cross in its label, beside its number.
  assert.match(html, /<span class="truncate">1<\/span>(<[^>]*>)*?<button title="Remove"/)
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

test('reads a schema with no type as JSON', () => {
  const html = renderToStaticMarkup(h(SchemaBuilder, { value: {}, setValue: noop }))
  assert.match(html, />JSON</)
})

test('folds a field\'s option lines with it', () => {
  const html = renderToStaticMarkup(
    h(SchemaBuilder, { value: { type: 'number', description: 'Count' }, setValue: noop }),
  )
  // A field with option lines folds, though it holds no schemas.
  assert.match(html, /title="Fold"/)
  assert.match(html, />Count</)
})

test('draws a multi-line box as a block, with no gap under its last line', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, { schema: { type: 'string', 'x-multiline': true }, value: 'a', setValue: noop }),
  )
  assert.match(html, /<textarea[^>]*class="[^"]*\bblock\b/)
})

test('lets a union option be named in the builder, showing its default until it is', () => {
  const html = renderToStaticMarkup(
    h(SchemaBuilder, {
      value: { anyOf: [{ type: 'string' }, { type: 'string', title: 'Code' }, { const: 'none' }] },
      setValue: noop,
    }),
  )
  assert.match(html, /placeholder="String 1"/)
  assert.match(html, /value="Code"/)
  assert.match(html, /placeholder="none"/)
})

test('puts a union picker where its option needs it', () => {
  const schema = { anyOf: [{ type: 'integer' }, { const: 'none' }, { type: 'array', items: { type: 'string' } }] }
  const one = renderToStaticMarkup(h(SchemaForm, { schema, value: 4, setValue: noop }))
  // A number: its box first, the chip at the end of the line.
  assert.match(one, /value="4"[\s\S]*<span class="truncate">Integer<\/span>/)
  const literal = renderToStaticMarkup(h(SchemaForm, { schema, value: 'none', setValue: noop }))
  // A literal: the picker is the line, and the value isn't drawn again.
  assert.match(literal, /text-gray-900">none<\/span>/)
  assert.doesNotMatch(literal, /&quot;none&quot;/)
  const list = renderToStaticMarkup(h(SchemaForm, { schema, value: ['a'], setValue: noop }))
  // A list: the picker's line, then the list's own lines.
  assert.match(list, /text-gray-900">Array<\/span>[\s\S]*value="a"/)
})

test('draws a builder as lines, with a border only when it is outermost', () => {
  const schema = { type: 'object', properties: { shape: { $ref: 'https://json-schema.org/draft/2020-12/schema' } } }
  const value = { shape: { type: 'object', properties: { inner: { type: 'string' } } } }
  const html = renderToStaticMarkup(h(SchemaForm, { schema, value, setValue: noop }))
  // The form's border, and no other: the builder's rows are lines of it.
  assert.equal((html.match(/border-gray-200/g) ?? []).length, 1)
  assert.match(html, /value="inner"/)
  assert.match(html, /> Add field</)
  // On its own, the builder draws the same lines, inside one border of its own.
  const alone = renderToStaticMarkup(h(SchemaBuilder, { value: value.shape, setValue: noop }))
  assert.equal((alone.match(/border-gray-200/g) ?? []).length, 1)
})

test('draws a lone value with no label column', () => {
  const html = renderToStaticMarkup(h(SchemaForm, { schema: { type: 'string' }, value: 'x', setValue: noop }))
  assert.match(html, /value="x"/)
  assert.doesNotMatch(html, /border-r border-gray-100/)
  assert.doesNotMatch(html, />Value</)
})

test('draws a popped-out value as one line that opens it', () => {
  const html = renderToStaticMarkup(
    h(SchemaForm, {
      schema: {
        type: 'object',
        properties: {
          ingredients: { type: 'array', 'x-popout': true, items: { type: 'string' } },
        },
      },
      value: { ingredients: ['leek', 'potato'] },
      setValue: noop,
    }),
  )
  assert.match(html, />2 items</)
  // Its items are in the dialog, not on the page.
  assert.doesNotMatch(html, /value="leek"/)
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
