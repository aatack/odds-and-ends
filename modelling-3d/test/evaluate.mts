// Evaluating a graph, including one model standing in for a transform inside
// another.
//
// What it checks: that unconnected inputs fall back to their defaults, that an
// error stops at the node it happened on and is reported downstream by name,
// that a model's ports become its sockets in the order they read down the
// canvas, and that a cycle is caught rather than hung on.
//
//   npm test

import assert from 'node:assert/strict'

const { evaluateModel } = await import('../src/core/evaluate')
const { modelDef, modelTransformId } = await import('../src/core/graph')

type Node = { id: string; transform: string; x: number; y: number; data?: Record<string, unknown> }

const model = (id: string, name: string, nodes: Node[], edges: [string, string, string, string][]) => ({
  id,
  name,
  order: 0,
  nodes: Object.fromEntries(nodes.map((n) => [n.id, { ...n, data: n.data ?? {} }])),
  edges: Object.fromEntries(
    edges.map(([source, sourceOutput, target, targetInput], k) => [
      `e${k}`,
      { id: `e${k}`, source, sourceOutput, target, targetInput },
    ]),
  ),
})

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('an unconnected input is worth its default, and a written one overrides it', () => {
  const m = model('m', 'Sums', [{ id: 'a', transform: 'number.add', x: 0, y: 0, data: { a: 2, b: 3 } }], [])
  const run = evaluateModel(m, { m })
  assert.equal(run.values.get('a')?.value, 5)

  const bare = model('m', 'Sums', [{ id: 'a', transform: 'number.add', x: 0, y: 0 }], [])
  assert.equal(evaluateModel(bare, { m: bare }).values.get('a')?.value, 0)
})

test('a value flows along an edge', () => {
  const m = model(
    'm',
    'Chain',
    [
      { id: 'k', transform: 'const.number', x: 0, y: 0, data: { value: 4 } },
      { id: 'a', transform: 'number.multiply', x: 1, y: 0, data: { b: 2.5 } },
    ],
    [['k', 'value', 'a', 'a']],
  )
  assert.equal(evaluateModel(m, { m }).values.get('a')?.value, 10)
})

test('a failure names the input it came in on, and stops there', () => {
  const m = model(
    'm',
    'Broken',
    [
      { id: 'd', transform: 'number.divide', x: 0, y: 0, data: { a: 1, b: 0 } },
      { id: 's', transform: 'number.add', x: 1, y: 0 },
    ],
    [['d', 'value', 's', 'a']],
  )
  const run = evaluateModel(m, { m })
  assert.equal(run.errors.get('d'), 'divide by zero')
  assert.equal(run.errors.get('s'), 'a: divide by zero')
  assert.equal(run.values.has('s'), false)
})

test('a cycle is reported rather than looped on', () => {
  const m = model(
    'm',
    'Loop',
    [
      { id: 'a', transform: 'number.add', x: 0, y: 0 },
      { id: 'b', transform: 'number.add', x: 1, y: 0 },
    ],
    [
      ['a', 'value', 'b', 'a'],
      ['b', 'value', 'a', 'a'],
    ],
  )
  const run = evaluateModel(m, { m })
  assert.ok(run.errors.get('a'))
  assert.ok(run.errors.get('b'))
})

test("a model's ports are its sockets, in the order they read down the canvas", () => {
  const inner = model(
    'inner',
    'Double',
    [
      { id: 'lower', transform: 'port.in.number', x: 0, y: 100, data: { name: 'b' } },
      { id: 'upper', transform: 'port.in.number', x: 0, y: 0, data: { name: 'a' } },
      { id: 'sum', transform: 'number.add', x: 1, y: 0 },
      { id: 'out', transform: 'port.out.number', x: 2, y: 0, data: { name: 'total' } },
    ],
    [
      ['upper', 'value', 'sum', 'a'],
      ['lower', 'value', 'sum', 'b'],
      ['sum', 'value', 'out', 'value'],
    ],
  )
  const def = modelDef(inner)
  assert.deepEqual(def.inputs.map((i) => i.name), ['a', 'b'])
  assert.deepEqual(def.outputs.map((o) => o.name), ['total'])
})

test('a model used inside another is called with what is wired to it', () => {
  const inner = model(
    'inner',
    'Double',
    [
      { id: 'in', transform: 'port.in.number', x: 0, y: 0, data: { name: 'x' } },
      { id: 'times', transform: 'number.multiply', x: 1, y: 0, data: { b: 2 } },
      { id: 'out', transform: 'port.out.number', x: 2, y: 0, data: { name: 'doubled' } },
    ],
    [
      ['in', 'value', 'times', 'a'],
      ['times', 'value', 'out', 'value'],
    ],
  )
  const outer = model(
    'outer',
    'Uses it',
    [
      { id: 'k', transform: 'const.number', x: 0, y: 0, data: { value: 7 } },
      { id: 'call', transform: modelTransformId('inner'), x: 1, y: 0 },
    ],
    [['k', 'value', 'call', 'x']],
  )
  const models = { inner, outer }
  assert.equal(evaluateModel(outer, models).values.get('call')?.doubled, 14)
})

test('a model that uses itself is caught', () => {
  const self = model(
    'self',
    'Recursive',
    [
      { id: 'in', transform: 'port.in.number', x: 0, y: 0, data: { name: 'x' } },
      { id: 'call', transform: modelTransformId('self'), x: 1, y: 0 },
      { id: 'out', transform: 'port.out.number', x: 2, y: 0, data: { name: 'y' } },
    ],
    [
      ['in', 'value', 'call', 'x'],
      ['call', 'value', 'out', 'value'],
    ],
  )
  const run = evaluateModel(self, { self })
  assert.equal(run.errors.get('call'), 'this model uses itself')
})

test('a model on its own uses its ports’ defaults, so it previews alone', () => {
  const m = model(
    'm',
    'Alone',
    [
      { id: 'in', transform: 'port.in.number', x: 0, y: 0, data: { name: 'x', value: 3 } },
      { id: 'times', transform: 'number.multiply', x: 1, y: 0, data: { b: 3 } },
    ],
    [['in', 'value', 'times', 'a']],
  )
  assert.equal(evaluateModel(m, { m }).values.get('times')?.value, 9)
})

test('a solid comes out of the graph as triangles, extruded up by default', () => {
  const m = model(
    'm',
    'A box',
    [
      { id: 'rect', transform: 'shape2.rectangle', x: 0, y: 0, data: { width: 2, height: 2 } },
      { id: 'ext', transform: 'solid.extrude', x: 1, y: 0 },
    ],
    [['rect', 'path', 'ext', 'path']],
  )
  const mesh = evaluateModel(m, { m }).values.get('ext')?.mesh as { triangles: unknown[] }
  assert.equal(mesh.triangles.length, 12)
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
