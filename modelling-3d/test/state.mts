// The editing rules, driven with no window open — which is the point of the
// state layer being plain functions over plain data.
//
// What it checks: that every edit reaches persistence as an operation, that an
// input takes only one edge and only a matching type, that a loop is refused,
// that deleting a model takes its instances with it, and that the preview falls
// back to the terminal nodes when nothing is selected.
//
//   npm test

import assert from 'node:assert/strict'

const { createStore } = await import('../src/renderer/src/state/store')
const { createActions } = await import('../src/renderer/src/state/actions')
const derive = await import('../src/renderer/src/state/derive')
const { modelTransformId } = await import('../src/core/graph')
const { seedModels } = await import('../src/core/seed')

type Op = { kind: string; [key: string]: unknown }

function app() {
  const ops: Op[] = []
  const store = createStore()
  const actions = createActions(store, { write: (batch) => ops.push(...(batch as Op[])) })
  return { store, actions, ops, state: () => store.getState() }
}

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('a new model opens, and is written once', () => {
  const { actions, ops, state } = app()
  const id = actions.createModel('Chair')
  assert.equal(state().openModelId, id)
  assert.equal(state().models[id].name, 'Chair')
  assert.deepEqual(ops.map((o) => o.kind), ['model.create'])
})

test('a node and an edge are written as they are made', () => {
  const { actions, ops, state } = app()
  actions.createModel('Bar')
  const shape = actions.addNode('shape2.rectangle', 0, 0)!
  const solid = actions.addNode('solid.extrude', 200, 0)!
  assert.equal(actions.connect(shape, 'path', solid, 'path'), null)
  const model = derive.openModel(state())!
  assert.equal(Object.keys(model.nodes).length, 2)
  assert.equal(Object.keys(model.edges).length, 1)
  assert.deepEqual(ops.map((o) => o.kind), ['model.create', 'node.put', 'node.put', 'edge.put'])
})

test('an input holds one edge; a second replaces the first', () => {
  const { actions, ops, state } = app()
  actions.createModel('Bar')
  const a = actions.addNode('shape2.rectangle', 0, 0)!
  const b = actions.addNode('shape2.circle', 0, 100)!
  const solid = actions.addNode('solid.extrude', 200, 0)!
  actions.connect(a, 'path', solid, 'path')
  actions.connect(b, 'path', solid, 'path')
  const model = derive.openModel(state())!
  assert.equal(Object.keys(model.edges).length, 1)
  assert.equal(Object.values(model.edges)[0].source, b)
  assert.ok(ops.some((o) => o.kind === 'edge.delete'))
})

test('types have to match, and a loop is refused', () => {
  const { actions, state } = app()
  actions.createModel('Bar')
  const shape = actions.addNode('shape2.rectangle', 0, 0)!
  const add = actions.addNode('number.add', 200, 0)!
  assert.equal(actions.connect(shape, 'path', add, 'a'), 'a path2 does not fit a number')

  const first = actions.addNode('number.add', 0, 200)!
  const second = actions.addNode('number.add', 200, 200)!
  actions.connect(first, 'value', second, 'a')
  assert.equal(actions.connect(second, 'value', first, 'a'), 'that would make a loop')
  assert.equal(Object.keys(derive.openModel(state())!.edges).length, 1)
})

test('a dropped input handle becomes a constant carrying what the socket was worth', () => {
  const { actions, state } = app()
  actions.createModel('Bar')
  const solid = actions.addNode('solid.extrude', 200, 0, { height: 4 })!
  const constant = actions.spawnConstant(solid, 'height', 0, 0)!
  const model = derive.openModel(state())!
  assert.equal(model.nodes[constant].transform, 'const.number')
  assert.equal(model.nodes[constant].data.value, 4)
  assert.equal(Object.values(model.edges)[0].target, solid)
})

test('deleting a node takes its edges with it', () => {
  const { actions, ops, state } = app()
  actions.createModel('Bar')
  const shape = actions.addNode('shape2.rectangle', 0, 0)!
  const solid = actions.addNode('solid.extrude', 200, 0)!
  actions.connect(shape, 'path', solid, 'path')
  actions.setSelection([shape])
  actions.deleteNodes([shape])
  const model = derive.openModel(state())!
  assert.deepEqual(Object.keys(model.nodes), [solid])
  assert.deepEqual(Object.keys(model.edges), [])
  assert.deepEqual(state().selection, [])
  assert.equal(ops.filter((o) => o.kind === 'node.delete').length, 1)
})

test('with nothing selected the preview shows every terminal node', () => {
  const { actions, state } = app()
  actions.createModel('Bar')
  const shape = actions.addNode('shape2.rectangle', 0, 0)!
  const solid = actions.addNode('solid.extrude', 200, 0)!
  actions.connect(shape, 'path', solid, 'path')
  assert.deepEqual(derive.previewedNodes(state()), [solid])
  actions.setSelection([shape])
  assert.deepEqual(derive.previewedNodes(state()), [shape])
})

test('the preview scene reduces whatever is selected to geometry', () => {
  const { actions, state } = app()
  actions.createModel('Bar')
  const shape = actions.addNode('shape2.rectangle', 0, 0)!
  const solid = actions.addNode('solid.extrude', 200, 0)!
  actions.connect(shape, 'path', solid, 'path')

  const evaluation = derive.evaluationOf(derive.openModel(state()), state().models)
  assert.equal(derive.previewScene(state(), evaluation).triangles.length, 12)

  actions.setSelection([shape])
  const outline = derive.previewScene(state(), evaluation)
  assert.equal(outline.triangles.length, 0)
  assert.equal(outline.lines.length, 1)
})

test('a model cannot be put inside itself, directly or through another', () => {
  const { actions, state } = app()
  const outer = actions.createModel('Outer')
  const inner = actions.createModel('Inner')
  assert.ok(actions.addNode(modelTransformId(outer), 0, 0))
  actions.openModel(outer)
  assert.equal(actions.addNode(modelTransformId(outer), 0, 0), null)
  // Inner already uses Outer, so Outer cannot use Inner either.
  assert.equal(actions.addNode(modelTransformId(inner), 0, 0), null)
  assert.equal(Object.keys(state().models[outer].nodes).length, 0)
})

test('deleting a model takes the nodes standing for it with it', () => {
  const { actions, ops, state } = app()
  actions.load(seedModels())
  actions.openModel('seed-colonnade')
  const before = Object.keys(state().models['seed-colonnade'].nodes).length
  actions.deleteModel('seed-column')
  const after = state().models['seed-colonnade']
  assert.equal(state().models['seed-column'], undefined)
  assert.equal(Object.keys(after.nodes).length, before - 1)
  assert.ok(ops.some((o) => o.kind === 'model.delete'))
  assert.ok(ops.some((o) => o.kind === 'node.delete'))
})

test('the seeded models evaluate to a solid', () => {
  const { actions, state } = app()
  actions.load(seedModels())
  actions.openModel('seed-colonnade')
  const model = derive.openModel(state())!
  const evaluation = derive.evaluationOf(model, state().models)
  assert.deepEqual([...evaluation.errors], [])
  assert.ok(derive.previewScene(state(), evaluation).triangles.length > 100)
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
