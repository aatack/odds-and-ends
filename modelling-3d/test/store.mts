// The sqlite store, against a real file in a temporary directory.
//
// What it checks: that a fresh store seeds itself and reads back exactly what
// was seeded, that each write operation lands, that deleting a model takes its
// rows with it, and that a node's literals survive the round trip.
//
//   npm test

import assert from 'node:assert/strict'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const { Store } = await import('../src/main/db')
const { seedModels } = await import('../src/core/seed')
const { evaluateModel } = await import('../src/core/evaluate')

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

const directory = mkdtempSync(join(tmpdir(), 'modelling-'))
let n = 0
const fresh = () => Store.open(join(directory, `store-${n++}.sqlite`))

test('a new store comes up seeded, and reads back what was seeded', async () => {
  const store = await fresh()
  const loaded = store.load()
  const seeded = seedModels()
  assert.deepEqual(Object.keys(loaded).sort(), Object.keys(seeded).sort())
  for (const [id, model] of Object.entries(seeded)) {
    assert.deepEqual(loaded[id], model)
  }
  store.close()
})

test('the seeded models still evaluate after a round trip through sqlite', async () => {
  const store = await fresh()
  const models = store.load()
  const run = evaluateModel(models['seed-colonnade'], models)
  assert.deepEqual([...run.errors], [])
  store.close()
})

test('every write operation lands', async () => {
  const store = await fresh()
  store.apply([
    { kind: 'model.create', model: { id: 'm', name: 'Test', order: 9, nodes: {}, edges: {} } },
    {
      kind: 'node.put',
      modelId: 'm',
      node: { id: 'n1', transform: 'shape2.rectangle', x: 1, y: 2, data: { width: 3 } },
    },
    {
      kind: 'node.put',
      modelId: 'm',
      node: { id: 'n2', transform: 'solid.extrude', x: 5, y: 6, data: {} },
    },
    {
      kind: 'edge.put',
      modelId: 'm',
      edge: { id: 'e1', source: 'n1', sourceOutput: 'path', target: 'n2', targetInput: 'path' },
    },
  ])
  store.apply([
    { kind: 'model.rename', id: 'm', name: 'Renamed' },
    { kind: 'node.move', id: 'n1', x: 40, y: 50 },
    { kind: 'node.data', id: 'n1', data: { width: 7, height: 2 } },
  ])

  const model = store.load()['m']
  assert.equal(model.name, 'Renamed')
  assert.equal(model.order, 9)
  assert.deepEqual({ x: model.nodes.n1.x, y: model.nodes.n1.y }, { x: 40, y: 50 })
  assert.deepEqual(model.nodes.n1.data, { width: 7, height: 2 })
  assert.equal(model.edges.e1.sourceOutput, 'path')

  store.apply([{ kind: 'edge.delete', id: 'e1' }, { kind: 'node.delete', id: 'n2' }])
  const after = store.load()['m']
  assert.deepEqual(Object.keys(after.nodes), ['n1'])
  assert.deepEqual(Object.keys(after.edges), [])
  store.close()
})

test('deleting a node takes its edges with it, in the file as well', async () => {
  const store = await fresh()
  store.apply([
    { kind: 'model.create', model: { id: 'm', name: 'Test', order: 0, nodes: {}, edges: {} } },
    { kind: 'node.put', modelId: 'm', node: { id: 'a', transform: 'number.add', x: 0, y: 0, data: {} } },
    { kind: 'node.put', modelId: 'm', node: { id: 'b', transform: 'number.add', x: 0, y: 0, data: {} } },
    {
      kind: 'edge.put',
      modelId: 'm',
      edge: { id: 'e', source: 'a', sourceOutput: 'value', target: 'b', targetInput: 'a' },
    },
  ])
  store.apply([{ kind: 'node.delete', id: 'a' }])
  assert.deepEqual(Object.keys(store.load()['m'].edges), [])
  store.close()
})

test('deleting a model empties it out of the file', async () => {
  const store = await fresh()
  store.apply([{ kind: 'model.delete', id: 'seed-column' }])
  const models = store.load()
  assert.equal(models['seed-column'], undefined)
  assert.ok(models['seed-colonnade'])
  store.close()
})

let failed = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
rmSync(directory, { recursive: true, force: true })
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
