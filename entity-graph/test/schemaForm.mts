// What the schema form and builder agree a schema means.
//
//   npm test

import assert from 'node:assert/strict'
import { defaultFor, propertiesOf, typeOf } from '../src/renderer/src/components/schema/schema'

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
