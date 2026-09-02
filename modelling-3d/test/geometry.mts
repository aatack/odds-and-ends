// The maths under the transforms: triangulation, and the surfaces built on it.
//
// What it checks: that a polygon is covered by the triangles it is cut into,
// that winding survives (a fill faces up, an extrusion faces outwards), and
// that a revolve closes back on itself.
//
//   npm test

import assert from 'node:assert/strict'

const g = await import('../src/core/geometry')
const { vec2, vec3, GREY } = await import('../src/core/values')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

const area2 = (a: { x: number; y: number }, b: { x: number; y: number }, c: { x: number; y: number }): number =>
  Math.abs((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)) / 2

const normalOf = (t: { a: any; b: any; c: any }) =>
  g.normalise3(g.cross3(g.sub3(t.b, t.a), g.sub3(t.c, t.a)))

test('a convex polygon is cut into n − 2 triangles covering its area', () => {
  const square = [vec2(0, 0), vec2(2, 0), vec2(2, 2), vec2(0, 2)]
  const tris = g.triangulate(square)
  assert.equal(tris.length, 2)
  const covered = tris.reduce((sum, [i, j, k]) => sum + area2(square[i], square[j], square[k]), 0)
  assert.ok(Math.abs(covered - 4) < 1e-9, `covered ${covered}`)
})

test('a reflex vertex is handled, and the winding is normalised', () => {
  // An L, given clockwise; ear clipping should still produce four triangles.
  const l = [vec2(0, 0), vec2(0, 2), vec2(1, 2), vec2(1, 1), vec2(2, 1), vec2(2, 0)]
  const tris = g.triangulate(l)
  assert.equal(tris.length, 4)
  const covered = tris.reduce((sum, [i, j, k]) => sum + area2(l[i], l[j], l[k]), 0)
  assert.ok(Math.abs(covered - 3) < 1e-9, `covered ${covered}`)
})

test('2D lifts onto the ground plane with y running to -z', () => {
  assert.deepEqual(g.lift(vec2(1, 2)), vec3(1, 0, -2))
  assert.deepEqual(g.flatten(vec3(1, 5, -2)), vec2(1, 2))
})

test('a fill faces upwards whichever way its outline was wound', () => {
  const anticlockwise = { points: [vec2(0, 0), vec2(1, 0), vec2(1, 1), vec2(0, 1)], closed: true }
  const clockwise = { points: [...anticlockwise.points].reverse(), closed: true }
  for (const path of [anticlockwise, clockwise]) {
    for (const t of g.fill(path, GREY).triangles) {
      assert.ok(normalOf(t).y > 0.99, 'a fill triangle faced down')
    }
  }
})

test('an extrusion is closed, and its walls face away from the axis', () => {
  const square = { points: [vec2(-1, -1), vec2(1, -1), vec2(1, 1), vec2(-1, 1)], closed: true }
  const mesh = g.extrude(square, 2, GREY)
  // Four walls and two caps, two triangles each.
  assert.equal(mesh.triangles.length, 12)
  // The solid is a box about (0, 1, 0), so every face points away from there.
  const middle = vec3(0, 1, 0)
  for (const t of mesh.triangles) {
    const centre = g.scale3(g.add3(g.add3(t.a, t.b), t.c), 1 / 3)
    assert.ok(g.dot3(normalOf(t), g.sub3(centre, middle)) > 0, 'a triangle faced inwards')
  }
})

test('a revolve wraps all the way round and back', () => {
  const profile = { points: [vec2(1, 0), vec2(1, 1)], closed: false }
  const mesh = g.revolve(profile, 8, GREY)
  assert.equal(mesh.triangles.length, 16)
  const bounds = g.boundsOf(mesh)!
  assert.ok(Math.abs(bounds.min.x + 1) < 1e-9)
  assert.ok(Math.abs(bounds.max.x - 1) < 1e-9)
  assert.ok(Math.abs(bounds.max.y - 1) < 1e-9)
})

test('a sphere is round and centred', () => {
  const mesh = g.sphere(2, 12, GREY)
  const bounds = g.boundsOf(mesh)!
  for (const axis of ['x', 'y', 'z'] as const) {
    assert.ok(Math.abs(bounds.max[axis] - 2) < 0.15, `${axis} reached ${bounds.max[axis]}`)
    assert.ok(Math.abs(bounds.min[axis] + 2) < 0.15, `${axis} reached ${bounds.min[axis]}`)
  }
})

test('rotating about an axis is a rotation', () => {
  const p = g.rotateAxis(vec3(1, 0, 0), vec3(0, 1, 0), 90)
  assert.ok(Math.abs(p.x) < 1e-9 && Math.abs(p.z + 1) < 1e-9, JSON.stringify(p))
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
