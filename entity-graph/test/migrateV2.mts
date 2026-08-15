// The v2 migration, over rows rather than over a database.
//
// The translation is a pure function from the old store's rows to this store's
// events, which is the whole reason it can be tested like this: no file is
// opened, nothing is written, and the assertions are about what the two stores
// each say an entity is.
//
// What it checks: that a link written v2's way arrives as a link here, in the
// same order, including the operations that reorder and remove; that a picture's
// flag becomes a file's values; that the old root ends up under the index; and
// that the checker actually notices when something is lost. The last of those
// matters most — the migration's safety net is `verify`, so a `verify` that
// passes everything would be worse than no check at all.
//
//   npm test

import assert from 'node:assert/strict'
import { bucketEvents, rollupEntity } from '../src/core/entity'
import type { AppEvent } from '../src/core/events'
import { INDEX_ID, RESOURCE_MIME, translate, verify } from '../scripts/migrateV2/translate.mjs'
import type { Json, V2Row } from '../scripts/migrateV2/v2Reducer.mjs'

const AUTHOR = 'tester'

const row = (timestamp: number, uuid: string, key: string, value: Json): V2Row => ({
  timestamp,
  uuid,
  key,
  value,
})

/** A link the v2 way: both halves, one write, one timestamp. */
const link = (timestamp: number, parent: string, child: string, operation = '+'): V2Row[] => [
  row(timestamp, parent, 'outbound', `${operation}${child}`),
  row(timestamp, child, 'inbound', `${operation}${parent}`),
]

const bytes = (uuid: string, timestamp: number): { uuid: string; timestamp: number; bytes: Buffer } => ({
  uuid,
  timestamp,
  bytes: Buffer.from([0x89, 0x50, 0x4e, 0x47]),
})

const migrate = (rows: V2Row[], resources: ReturnType<typeof bytes>[] = []) =>
  translate({ rows, resources }, AUTHOR)

/** What the new store would show for an entity, given the events written. */
const entity = (events: AppEvent[], id: string) =>
  rollupEntity(id, bucketEvents([id], events).get(id) ?? [])

const kinds = (translation: { warnings: { kind: string }[] }): string[] =>
  translation.warnings.map((warning) => warning.kind)

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('a tree comes across as a tree', () => {
  const { events, warnings } = migrate([
    row(1, 'root', 'text', 'Root'),
    row(2, 'a', 'text', 'Apples'),
    ...link(2, 'root', 'a'),
    row(3, 'b', 'text', 'Bananas'),
    ...link(3, 'root', 'b'),
  ])

  assert.deepEqual(entity(events, 'root').outboundLinks, ['a', 'b'])
  assert.equal(entity(events, 'a').values.text, 'Apples')
  assert.deepEqual(entity(events, 'a').inboundLinks, ['root'])
  assert.deepEqual(warnings, [])
})

test('the old root hangs under the index, at the root\'s own age', () => {
  const { events } = migrate([row(5, 'root', 'text', 'Root'), row(6, 'a', 'text', 'Apples')])

  const index = entity(events, INDEX_ID)
  assert.deepEqual(index.outboundLinks, ['root'])
  assert.equal(index.createdAt, 5)
})

test('every event is stamped with the author it was given', () => {
  const { events } = migrate([row(1, 'root', 'text', 'Root'), ...link(2, 'root', 'a')])
  assert.deepEqual([...new Set(events.map((event) => event.author))], [AUTHOR])
})

test('rows are replayed in v2\'s order, not the order they were stored', () => {
  // The later write first, the way a table can hand them back after a delete.
  const { events } = migrate([
    ...link(3, 'root', 'b'),
    row(1, 'root', 'text', 'First'),
    row(2, 'root', 'text', 'Second'),
    ...link(2, 'root', 'a'),
  ])

  assert.equal(entity(events, 'root').values.text, 'Second')
  assert.deepEqual(entity(events, 'root').outboundLinks, ['a', 'b'])
})

test('a child moved towards the front and back lands where v2 put it', () => {
  const rows = [
    row(1, 'root', 'text', 'Root'),
    ...link(2, 'root', 'a'),
    ...link(3, 'root', 'b'),
    ...link(4, 'root', 'c'),
    row(5, 'root', 'outbound', '<c'),
    row(6, 'root', 'outbound', '>a'),
  ]
  const translation = migrate(rows)

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, ['c', 'a', 'b'])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('a move that cannot happen changes nothing, as it did there', () => {
  const rows = [
    row(1, 'root', 'text', 'Root'),
    ...link(2, 'root', 'a'),
    ...link(3, 'root', 'b'),
    row(4, 'root', 'outbound', '<a'), // already first
    row(5, 'root', 'outbound', '>b'), // already last
    row(6, 'root', 'outbound', '<zzz'), // not a child at all
  ]
  const translation = migrate(rows)

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, ['a', 'b'])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('removing a child removes it, and emptying the list empties it', () => {
  const translation = migrate([
    row(1, 'root', 'text', 'Root'),
    ...link(2, 'root', 'a'),
    ...link(3, 'root', 'b'),
    ...link(4, 'root', 'a', '-'),
    ...link(5, 'root', 'b', '-'),
  ])

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, [])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('one write holding several operations applies them left to right', () => {
  // What moving an entity looked like: off the old parent, onto the new one.
  const translation = migrate([
    row(1, 'root', 'text', 'Root'),
    ...link(2, 'root', 'a'),
    ...link(3, 'root', 'b'),
    row(4, 'root', 'outbound', ['-a', '+c', '<c']),
  ])

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, ['c', 'b'])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('adding a child twice adds it once', () => {
  const translation = migrate([row(1, 'root', 'text', 'Root'), ...link(2, 'root', 'a'), ...link(3, 'root', 'a')])

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, ['a'])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('an operation that meant nothing to v2 means nothing here, and is reported', () => {
  const translation = migrate([row(1, 'root', 'text', 'Root'), row(2, 'root', 'outbound', '?a')])

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, [])
  assert.deepEqual(kinds(translation), ['unknownOperation'])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('an image becomes a file, bytes and all', () => {
  const translation = migrate(
    [
      row(1, 'root', 'text', 'Root'),
      ...link(2, 'root', 'pic'),
      row(2, 'pic', 'image', true),
      row(3, 'pic', 'text', 'A screenshot'),
    ],
    [bytes('pic', 2)]
  )

  const pic = entity(translation.events, 'pic')
  assert.equal(pic.values.type, 'file')
  assert.equal(pic.values.mimeType, RESOURCE_MIME)
  assert.equal(pic.values.image, undefined)
  assert.equal(pic.values.text, 'A screenshot')
  assert.deepEqual(translation.resources, [
    { id: 'pic', timestamp: 2, author: AUTHOR, mimeType: RESOURCE_MIME, name: null, data: 'iVBORw==' },
  ])
  assert.deepEqual(kinds(translation), [])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('bytes with no flag are marked as a file anyway, and said so', () => {
  const translation = migrate([row(1, 'root', 'text', 'Root'), ...link(2, 'root', 'pic')], [bytes('pic', 2)])

  assert.equal(entity(translation.events, 'pic').values.type, 'file')
  assert.deepEqual(kinds(translation), ['resourceWithoutFlag'])
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('a flag with no bytes, and bytes with no entity, are both reported', () => {
  const orphan = migrate([row(1, 'root', 'text', 'Root'), row(2, 'pic', 'image', true)])
  assert.deepEqual(kinds(orphan), ['imageWithoutBytes'])

  const loose = migrate([row(1, 'root', 'text', 'Root')], [bytes('nowhere', 2)])
  assert.deepEqual(kinds(loose), ['resourceWithoutEntity', 'resourceWithoutFlag'])
  assert.equal(loose.resources.length, 1)
})

test('the other keys come across untouched', () => {
  const translation = migrate([
    row(1, 'a', 'text', 'A note'),
    row(1, 'a', 'section', true),
    row(2, 'a', 'open', false),
    row(3, 'a', 'type', 'formula'),
    row(4, 'a', 'snoozed', '2026-01-01T00:00:00.000Z'),
    row(5, 'a', 'redacted', true),
    row(6, 'a', 'ai', null),
    row(7, 'a', 'llmContext', { model: 'gemini' }),
  ])

  assert.deepEqual(entity(translation.events, 'a').values, {
    text: 'A note',
    section: true,
    open: false,
    type: 'formula',
    snoozed: '2026-01-01T00:00:00.000Z',
    redacted: true,
    ai: null,
    llmContext: { model: 'gemini' },
  })
  assert.deepEqual(verify(translation).discrepancies, [])
})

test('a link only one half of which was written is reported, not invented', () => {
  const translation = migrate([
    row(1, 'root', 'text', 'Root'),
    row(2, 'a', 'text', 'Apples'),
    row(2, 'a', 'inbound', '+root'),
  ])

  assert.deepEqual(entity(translation.events, 'root').outboundLinks, [])
  assert.deepEqual(kinds(verify(translation)), ['inboundWithoutOutbound'])
})

test('a link to an entity nothing was written to is reported', () => {
  const translation = migrate([row(1, 'root', 'text', 'Root'), row(2, 'root', 'outbound', '+ghost')])
  assert.deepEqual(kinds(translation), ['danglingLink'])
})

test('a row whose JSON would not parse is reported rather than dropped in silence', () => {
  const translation = translate(
    { rows: [row(1, 'root', 'text', 'Root')], resources: [], unreadable: [{ uuid: 'a', key: 'text' }] },
    AUTHOR
  )
  assert.deepEqual(kinds(translation), ['unreadableRow'])
})

test('the check notices when the translation loses something', () => {
  // The safety net, tested by cutting a hole in it: drop the events for one
  // entity and the rollups must stop agreeing.
  const translation = migrate([
    row(1, 'root', 'text', 'Root'),
    row(2, 'a', 'text', 'Apples'),
    ...link(2, 'root', 'a'),
  ])
  const damaged = {
    ...translation,
    events: translation.events.filter(
      (event) => !(event.type === 'value' && event.entityId === 'a')
    ),
  }

  assert.deepEqual(verify(damaged).discrepancies, [
    { id: 'a', field: 'values', expected: { text: 'Apples' }, actual: {} },
  ])
})

test('the check notices when the child order comes out wrong', () => {
  // Both links in one write, which is where order is fragile: a rollup breaks a
  // tie by the order the events arrive in, so writing them the other way round
  // is a difference nothing else would catch.
  const translation = migrate([
    row(1, 'root', 'text', 'Root'),
    ...link(2, 'root', 'a'),
    ...link(2, 'root', 'b'),
  ])
  const reversed = {
    ...translation,
    events: [...translation.events].reverse(),
  }

  assert.deepEqual(verify(reversed).discrepancies, [
    { id: 'root', field: 'children', expected: ['a', 'b'], actual: ['b', 'a'] },
  ])
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
