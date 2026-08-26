// What a type says about its instances, read: the fields a schema declares, the
// shape each one is labelled with, and why a value doesn't fit one. All of it is
// pure, so this needs nothing but the module.
//
// What it checks: that the field order is the schema's, that a check is *soft* and
// answers rather than throws, that null is never wrong, and that the entities the
// store serves come back as events anything can roll up.
//
//   npm test

import assert from 'node:assert/strict'

const { actionsOf, checkValue, fieldsOf, isTextual, schemaOf, typeLabel } = await import(
  '../src/core/schema'
)
const { TOOL_ID, TYPE_ID, builtinEvents } = await import('../src/core/builtins')
const { rollupEntity } = await import('../src/core/entity')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

const SCHEMA = {
  type: 'object',
  properties: {
    worktree: { type: 'string', description: 'The full path on this machine' },
    depth: { type: 'integer', minimum: 1 },
    state: { enum: ['open', 'merged'] },
    labels: { type: 'array', items: { type: 'string' } },
  },
  required: ['worktree'],
}

test('reads the fields in the order the schema wrote them', () => {
  const fields = fieldsOf(SCHEMA)
  assert.deepEqual(
    fields.map((f) => f.key),
    ['worktree', 'depth', 'state', 'labels'],
  )
  assert.equal(fields[0].required, true)
  assert.equal(fields[1].required, false)
  assert.equal(fields[0].description, 'The full path on this machine')
})

test('labels a field with its shape rather than the word "string"', () => {
  assert.equal(typeLabel({ type: 'string' }), 'string')
  assert.equal(typeLabel({ enum: ['open', 'merged'] }), '"open" | "merged"')
  assert.equal(typeLabel({ type: 'array', items: { type: 'string' } }), 'string[]')
  assert.equal(typeLabel({}), 'any')
})

test('takes a choice between strings as text, not as JSON to be quoted', () => {
  assert.equal(isTextual({ type: 'string' }), true)
  assert.equal(isTextual({ enum: ['open', 'merged'] }), true)
  assert.equal(isTextual({ type: 'integer' }), false)
  assert.equal(isTextual(undefined), false)
})

test('says why a value does not fit, and never refuses one', () => {
  const fields = Object.fromEntries(fieldsOf(SCHEMA).map((f) => [f.key, f.schema]))
  assert.equal(checkValue('~/work', fields.worktree), null)
  assert.equal(checkValue(3, fields.worktree), 'expected string')
  assert.equal(checkValue(0, fields.depth), 'at least 1')
  assert.equal(checkValue(1.5, fields.depth), 'expected integer')
  assert.equal(checkValue('closed', fields.state), 'must be one of "open", "merged"')
  assert.equal(checkValue(['bug'], fields.labels), null)
  assert.equal(checkValue(['bug', 2], fields.labels), 'item 2 expected string')
})

test('never faults an absent value, however required the field', () => {
  const worktree = fieldsOf(SCHEMA)[0].schema
  // Null is how a value comes *off* an entity in an append-only store, so a
  // required field that has been cleared is an empty field, not a wrong one.
  assert.equal(checkValue(null, worktree), null)
  assert.equal(checkValue(undefined, worktree), null)
})

test('ignores what it does not understand rather than calling it a failure', () => {
  assert.equal(checkValue('anything', { type: 'string', format: 'uri', oneOf: [] }), null)
  assert.equal(checkValue('anything', { pattern: '(' }), null, 'including a pattern that is not one')
})

test('reads a schema and its actions off a type, and nothing off a half-written one', () => {
  assert.deepEqual(schemaOf({ schema: SCHEMA }), SCHEMA)
  assert.equal(schemaOf({ schema: 'an object, please' }), null)
  assert.equal(schemaOf(undefined), null)
  assert.deepEqual(actionsOf({ actions: ['github.mergePullRequest', 'link.open'] }), [
    'github.mergePullRequest',
    'link.open',
  ])
  // Anything that doesn't name a tool is not a button: a dead one on every
  // instance of a type is worse than one that isn't there.
  assert.deepEqual(actionsOf({ actions: ['changeset.merge', 3, '  ', null] }), ['changeset.merge'])
  // The old shape — a dictionary of name → script — says nothing now.
  assert.deepEqual(actionsOf({ actions: { Merge: 'merge()' } }), [])
  assert.deepEqual(actionsOf({}), [])
})

test('serves the type entity as events, behind anything anybody wrote', () => {
  const served = builtinEvents([TYPE_ID, 'something-else'])
  assert.equal(
    served.every((e) => e.timestamp === 0),
    true,
  )
  const rolled = rollupEntity(TYPE_ID, [
    ...served,
    { type: 'value', timestamp: 5, author: 'alex', entityId: TYPE_ID, key: 'text', value: 'Mine' },
  ])
  assert.equal(rolled.values.text, 'Mine', 'a real write wins')
  assert.deepEqual(Object.keys(schemaOf(rolled.values)?.properties as object), [
    'schema',
    'actions',
    'events',
  ])
  // Only ids asked for by name, and a dump of the store is what is written down.
  assert.deepEqual(builtinEvents(['something-else']), [])
  assert.deepEqual(builtinEvents(undefined), [])
})

test('says what an events script is handed and what it hands back', () => {
  // The one thing an agent had to read the source for: that the list it returns
  // is made of the store's own events, which three of their fields it may leave
  // out, and that its own id is in the context. This description is the only
  // account of any of it that reaches the endpoint.
  const rolled = rollupEntity(TYPE_ID, builtinEvents([TYPE_ID]))
  const events = fieldsOf(schemaOf(rolled.values)).find((f) => f.key === 'events')
  const said = events?.description ?? ''
  for (const phrase of ["type: 'value'", 'context.entityId', 'entityId', 'author', 'timestamp']) {
    assert.ok(said.includes(phrase), `the events field should mention ${phrase}`)
  }
})

test('serves the tool type, which is what an agent over MCP reads instead of the code', () => {
  const rolled = rollupEntity(TOOL_ID, builtinEvents([TOOL_ID]))
  assert.equal(rolled.values.type, TYPE_ID, 'a tool type is an ordinary type')
  const schema = schemaOf(rolled.values)
  // The one value the loader insists on. Its `type` and its text are the other two
  // things a definition needs, and neither is a field of the schema — one is what
  // says which schema to read, the other is the note's own line in the outline.
  assert.deepEqual(schema?.required, ['execute'])
  const fields = fieldsOf(schema)
  assert.deepEqual(
    fields.map((f) => f.key),
    ['execute', 'description', 'arguments', 'id', 'key', 'scope', 'reach', 'safety', 'mutates'],
  )
  // The descriptions are the documentation, so a field without one is a field an
  // agent has to guess at.
  assert.deepEqual(fields.filter((f) => !f.description).map((f) => f.key), [])
  assert.equal(checkValue('mod+shift+j', fields.find((f) => f.key === 'key')?.schema), null)
  assert.equal(
    checkValue('whenever', fields.find((f) => f.key === 'reach')?.schema),
    'must be one of "ui", "source", "external"',
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
