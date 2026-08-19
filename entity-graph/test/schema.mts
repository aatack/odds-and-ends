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

const { actionNames, checkValue, fieldsOf, isTextual, schemaOf, typeLabel } = await import(
  '../src/core/schema'
)
const { BUILTIN_TYPES, TYPE_ID, TYPES_ID, builtinEvents } = await import('../src/core/builtins')
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

test('takes a choice between shapes as any one of them', () => {
  // `arguments` on a tool: a list, a schema written the long way, or that list
  // left as text. The label says all three rather than "any", and a value that is
  // none of them is told what the field is rather than one branch's complaint.
  const shapes = { anyOf: [{ type: 'array' }, { type: 'object' }, { type: 'string' }] }
  assert.equal(typeLabel(shapes), 'array | object | string')
  assert.equal(checkValue([1], shapes), null)
  assert.equal(checkValue({ a: 1 }, shapes), null)
  assert.equal(checkValue('[1]', shapes), null)
  assert.equal(checkValue(7, shapes), 'expected array | object | string')
})

test('ignores what it does not understand rather than calling it a failure', () => {
  assert.equal(checkValue('anything', { type: 'string', format: 'uri', oneOf: [] }), null)
  assert.equal(checkValue('anything', { pattern: '(' }), null, 'including a pattern that is not one')
})

test('reads a schema and its actions off a type, and nothing off a half-written one', () => {
  assert.deepEqual(schemaOf({ schema: SCHEMA }), SCHEMA)
  assert.equal(schemaOf({ schema: 'an object, please' }), null)
  assert.equal(schemaOf(undefined), null)
  assert.deepEqual(actionNames({ actions: { Merge: 'merge()', Approve: 'approve()' } }), [
    'Merge',
    'Approve',
  ])
  // A body that isn't a script is not a button: an action that throws the moment
  // it is pressed is worse than one that isn't there.
  assert.deepEqual(actionNames({ actions: { Merge: 'merge()', Nope: 3, Blank: '  ' } }), ['Merge'])
  assert.deepEqual(actionNames({}), [])
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

test('serves a type for everything the app reads by name, hung under @types', () => {
  // The rule: a field the app gives special meaning to is described somewhere the
  // store can hand back. These four are what that comes to today.
  assert.deepEqual(
    BUILTIN_TYPES.map((t) => t.id),
    ['type', 'tool', 'code', 'file'],
  )
  for (const { id, text, schema } of BUILTIN_TYPES) {
    const rolled = rollupEntity(id, builtinEvents([id]))
    assert.equal(rolled.values.text, text, `${id} says what it is called`)
    assert.equal(rolled.values.type, TYPE_ID, `${id} is a type`)
    assert.deepEqual(rolled.values.schema, schema)
    // A description on every field, or an agent reading it is still guessing.
    for (const field of fieldsOf(schema)) {
      assert.ok(field.description, `${id}.${field.key} says what it is for`)
    }
  }

  // The heading, and the links both ways: a walk down from `@types` finds them,
  // and a type asked for on its own knows where it hangs.
  const heading = rollupEntity(TYPES_ID, builtinEvents([TYPES_ID]))
  assert.equal(heading.values.section, true)
  assert.deepEqual(heading.outboundLinks, ['type', 'tool', 'code', 'file'])
  assert.deepEqual(rollupEntity('tool', builtinEvents(['tool'])).inboundLinks, [TYPES_ID])
})

test('describes a tool the way the app and the server read one', () => {
  const tool = schemaOf(rollupEntity('tool', builtinEvents(['tool'])).values)
  const fields = Object.fromEntries(fieldsOf(tool).map((f) => [f.key, f]))
  // The app's answer to what makes a note tool-shaped: something to call it, and
  // something to run. The server wants a description and an arguments as well,
  // which the descriptions say rather than the schema refusing anything.
  assert.equal(fields.name.required, true)
  assert.equal(fields.execute.required, true)
  assert.equal(fields.description.required, false)
  assert.equal(fields.scope.label, '"frame" | "group" | "app"')
  assert.equal(checkValue('group', fields.scope.schema), null)
  assert.equal(checkValue('window', fields.scope.schema), 'must be one of "frame", "group", "app"')
  // Written as a list of arguments, and each of those is an object with a name —
  // the `tool/argument` shape, described where the list is rather than as a type
  // nothing would name.
  assert.equal(checkValue([{ name: 'who', type: 'string' }], fields.arguments.schema), null)
  assert.equal(checkValue([{ type: 'string' }], fields.arguments.schema), 'expected array | object | string')
  assert.equal(checkValue('["who"]', fields.arguments.schema), null, 'a list left as text still fits')
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
