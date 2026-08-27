// The reading of a diagram's values: which keys are shapes, what each one says,
// and what a canvas is given to draw them in. Worth asserting rather than clicking
// because these values are written by hand and over MCP as much as they are
// dragged, so "says half of what a rectangle says" is the ordinary case and not
// the exception.
//
//   npm test

import assert from 'node:assert/strict'

const {
  DEFAULT_ASPECT_RATIO,
  DEFAULT_HEIGHT,
  DEFAULT_WIDTH,
  aspectRatioOf,
  boxesOf,
  isShapeKey,
  nextShapeKey,
  shapesOf,
} = await import('../src/core/diagram')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('a shape is a value under a `diagram/` key, and nothing else is', () => {
  assert.equal(isShapeKey('diagram/1'), true)
  assert.equal(isShapeKey('text'), false)
  assert.equal(isShapeKey('aspectRatio'), false)

  const shapes = shapesOf({
    text: 'How it fits together',
    aspectRatio: 2,
    'diagram/1': { shape: 'rectangle', x: 10, y: 20, width: 100, height: 50, text: 'Ingest' },
  })
  assert.deepEqual(shapes, [
    { key: 'diagram/1', shape: 'rectangle', x: 10, y: 20, width: 100, height: 50, text: 'Ingest' },
  ])
})

test('a shape half written is drawn with defaults for the rest', () => {
  const [box] = shapesOf({ 'diagram/1': { shape: 'rectangle' } })
  assert.deepEqual(box, {
    key: 'diagram/1',
    shape: 'rectangle',
    x: 0,
    y: 0,
    width: DEFAULT_WIDTH,
    height: DEFAULT_HEIGHT,
    text: '',
  })
})

test('a value naming no shape is not one', () => {
  assert.deepEqual(shapesOf({ 'diagram/1': { shape: 'octagon' } }), [])
  assert.deepEqual(shapesOf({ 'diagram/2': 'a rectangle, honestly' }), [])
  // How a shape comes off an entity in an append-only store.
  assert.deepEqual(shapesOf({ 'diagram/3': null }), [])
})

test('the shapes come back in key order, whatever order they were written in', () => {
  const shapes = shapesOf({
    'diagram/3': { shape: 'text' },
    'diagram/1': { shape: 'text' },
    'diagram/2': { shape: 'text' },
  })
  assert.deepEqual(
    shapes.map((s) => s.key),
    ['diagram/1', 'diagram/2', 'diagram/3'],
  )
})

test("an arrow's ends are a key or a point, and a bare id is taken as a key", () => {
  const [arrow] = shapesOf({
    'diagram/9': { shape: 'arrow', from: '1', to: { x: 30, y: 40 }, text: 'then' },
  })
  assert.deepEqual(arrow, {
    key: 'diagram/9',
    shape: 'arrow',
    from: 'diagram/1',
    to: { x: 30, y: 40 },
    text: 'then',
  })
})

test('the boxes an arrow can name are the boxes, and only those', () => {
  const boxes = boxesOf(
    shapesOf({
      'diagram/1': { shape: 'rectangle' },
      'diagram/2': { shape: 'text' },
      'diagram/3': { shape: 'arrow' },
    }),
  )
  assert.deepEqual([...boxes.keys()], ['diagram/1', 'diagram/2'])
})

test('a ratio is a number or a ratio written as one, and 16:9 otherwise', () => {
  assert.equal(aspectRatioOf({ aspectRatio: 2 }), 2)
  assert.equal(aspectRatioOf({ aspectRatio: '16:9' }), 16 / 9)
  assert.equal(aspectRatioOf({ aspectRatio: '4/3' }), 4 / 3)
  assert.equal(aspectRatioOf({ aspectRatio: '1.5' }), 1.5)
  assert.equal(aspectRatioOf({}), DEFAULT_ASPECT_RATIO)
  assert.equal(aspectRatioOf({ aspectRatio: 0 }), DEFAULT_ASPECT_RATIO)
  assert.equal(aspectRatioOf({ aspectRatio: 'wide' }), DEFAULT_ASPECT_RATIO)
  assert.equal(aspectRatioOf({ aspectRatio: '16:0' }), DEFAULT_ASPECT_RATIO)
})

test('a new key is the next number up, over whatever is already there', () => {
  assert.equal(nextShapeKey({}), 'diagram/1')
  assert.equal(nextShapeKey({ 'diagram/1': {}, 'diagram/4': {} }), 'diagram/5')
  // A key that isn't a number takes no part in the numbering, and a key that was
  // cleared still counts — writing over a null would resurrect nothing, but the
  // number is cheap and reusing it is confusing.
  assert.equal(nextShapeKey({ 'diagram/inbox': {}, 'diagram/2': null, text: 'x' }), 'diagram/3')
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
