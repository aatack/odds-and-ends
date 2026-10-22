// What the schema form and builder agree a schema means.
//
//   npm test

import assert from 'node:assert/strict'
import { defaultFor, optionNames, problemOf, propertiesOf, typeOf } from '../src/renderer/src/components/schema/schema'

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('reads one type out of a schema, nullable or implied', () => {
  assert.equal(typeOf({ type: 'string' }), 'string')
  assert.equal(typeOf({ type: ['integer', 'null'] }), 'integer')
  assert.equal(typeOf({ properties: {} }), 'object')
  assert.equal(typeOf({ items: {} }), 'array')
  assert.equal(typeOf({ type: ['string', 'number'] }), null, 'a union has no one control')
  assert.equal(typeOf({}), null)
})

test('makes a fresh value with its required fields filled', () => {
  assert.deepEqual(
    defaultFor({
      type: 'object',
      properties: { a: { type: 'string' }, b: { type: 'integer' }, c: { enum: ['x', 'y'] } },
      required: ['a', 'c'],
    }),
    { a: '', c: 'x' },
  )
  assert.equal(defaultFor({ type: 'number', default: 3 }), 3)
  assert.deepEqual(defaultFor({ type: 'array' }), [])
})

test('keeps properties in the order written', () => {
  const keys = propertiesOf({ properties: { z: {}, a: {}, m: {} } }).map(([k]) => k)
  assert.deepEqual(keys, ['z', 'a', 'm'])
})

test('says why a value does not fit, and says nothing when it does', () => {
  assert.equal(problemOf({ type: 'number', minimum: 2, maximum: 5 }, 1), 'At least 2')
  assert.equal(problemOf({ type: 'number', minimum: 2, maximum: 5 }, 9), 'At most 5')
  assert.equal(problemOf({ type: 'number', minimum: 2, maximum: 5 }, 3), null)
  assert.equal(problemOf({ type: 'integer' }, 1.5), 'Not a whole number')
  assert.equal(problemOf({ type: 'string', minLength: 3 }, 'ab'), 'At least 3 characters')
  assert.equal(problemOf({ type: 'string', pattern: '^a' }, 'ba'), "Doesn't match ^a")
  assert.equal(problemOf({ type: 'string', format: 'email' }, 'nope'), 'Not an email')
  assert.equal(problemOf({ type: 'array', maxItems: 1 }, [1, 2]), 'At most 1 item')
  assert.equal(problemOf({ type: 'array', uniqueItems: true }, [1, 1]), 'Items repeat')
  assert.equal(problemOf({ anyOf: [{ const: 'a' }, { const: 'b' }] }, 'c'), 'Fits none of the options')
  assert.equal(problemOf({ type: 'string' }, 3), 'Not a string')
  // Absent is fine unless it is required.
  assert.equal(problemOf({ type: 'string' }, undefined), null)
  assert.equal(problemOf({ type: 'string' }, undefined, true), 'Required')
})

test('names union options by title, then literal value, then numbered type', () => {
  assert.deepEqual(
    optionNames([
      { const: 'main' },
      { const: 3 },
      { type: 'string' },
      { type: 'string' },
      { type: 'integer' },
      { type: 'string', title: 'Code' },
    ]),
    ['main', '3', 'String 1', 'String 2', 'Integer', 'Code'],
  )
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
