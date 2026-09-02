// The value types themselves: colour conversions, defaults, and what counts as
// an inhabitant of a type.
//
// What it checks: that hex and HSV round-trip, that a grey has no hue to lose,
// that `isValue` turns away what storage might hand back, and that every type
// has a default that is one of its own.
//
//   npm test

import assert from 'node:assert/strict'

const v = await import('../src/core/values')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

const close = (a: number, b: number, within = 1e-9): boolean => Math.abs(a - b) < within

test('hex round-trips', () => {
  for (const hex of ['#000000', '#ffffff', '#d9d4c7', '#4c53c4']) {
    assert.equal(v.toHex(v.fromHex(hex)), hex)
  }
  assert.deepEqual(v.fromHex('#fff'), v.fromHex('#ffffff'))
})

test('HSV round-trips, and a grey keeps its lightness', () => {
  for (const hex of ['#d9d4c7', '#4c53c4', '#b0553f', '#7c9a6d', '#101010']) {
    const colour = v.fromHex(hex)
    const back = v.fromHsv(v.toHsv(colour))
    assert.ok(
      close(back.r, colour.r, 1e-6) && close(back.g, colour.g, 1e-6) && close(back.b, colour.b, 1e-6),
      `${hex} came back as ${v.toHex(back)}`,
    )
  }
  const grey = v.toHsv(v.fromHex('#808080'))
  assert.equal(grey.s, 0)
  assert.ok(close(grey.v, 128 / 255))
})

test('a hue with full saturation and value is a primary', () => {
  assert.equal(v.toHex(v.fromHsv({ h: 0, s: 1, v: 1 })), '#ff0000')
  assert.equal(v.toHex(v.fromHsv({ h: 120, s: 1, v: 1 })), '#00ff00')
  assert.equal(v.toHex(v.fromHsv({ h: 240, s: 1, v: 1 })), '#0000ff')
  assert.equal(v.toHex(v.fromHsv({ h: 360, s: 1, v: 1 })), '#ff0000')
})

test('every type has a default of its own type', () => {
  for (const type of v.VALUE_TYPES) {
    assert.ok(v.isValue(type, v.defaultValue(type)), `${type} default is not a ${type}`)
  }
})

test('isValue turns away what storage might hand back', () => {
  assert.equal(v.isValue('number', '3'), false)
  assert.equal(v.isValue('number', NaN), false)
  assert.equal(v.isValue('vec3', { x: 1, y: 2 }), false)
  assert.equal(v.isValue('vec2', { x: 1, y: 2 }), true)
  assert.equal(v.isValue('path2', { points: [{ x: 0, y: 0 }], closed: true }), true)
  assert.equal(v.isValue('path2', { points: [{ x: 0, z: 0 }], closed: true }), false)
  assert.equal(v.isValue('colour', null), false)
})

test('a value describes itself in a few words', () => {
  assert.equal(v.describe('number', 1.23456789), '1.235')
  assert.equal(v.describe('mesh', { triangles: [1, 2, 3] }), '3 triangles')
  assert.equal(v.describe('colour', v.fromHex('#4c53c4')), '#4c53c4')
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
