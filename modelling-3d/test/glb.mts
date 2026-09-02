// The glTF writer, read back the way a viewer would read it.
//
// What it checks: the container (magic, version, chunk lengths, 4-byte
// alignment), that the accessors describe as many vertices as there are
// triangle corners, that the bounds are the mesh's, and that a colour arrives
// in linear space.
//
//   npm test

import assert from 'node:assert/strict'

const { exportGlb } = await import('../src/core/glb')
const { box, extrude } = await import('../src/core/geometry')
const { colour, toLinear, vec2, vec3 } = await import('../src/core/values')

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

/** The reader half: pull the JSON and binary chunks back out of a `.glb`. */
function readGlb(bytes: Uint8Array): { json: any; bin: DataView } {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
  assert.equal(view.getUint32(0, true), 0x46546c67, 'magic')
  assert.equal(view.getUint32(4, true), 2, 'version')
  assert.equal(view.getUint32(8, true), bytes.byteLength, 'declared length')

  const jsonLength = view.getUint32(12, true)
  assert.equal(view.getUint32(16, true), 0x4e4f534a, 'json chunk type')
  assert.equal(jsonLength % 4, 0, 'json chunk padded to 4')
  const json = JSON.parse(new TextDecoder().decode(bytes.subarray(20, 20 + jsonLength)))

  const binStart = 20 + jsonLength
  const binLength = view.getUint32(binStart, true)
  assert.equal(view.getUint32(binStart + 4, true), 0x004e4942, 'bin chunk type')
  assert.equal(binStart + 8 + binLength, bytes.byteLength, 'bin chunk fills the file')
  return { json, bin: new DataView(bytes.buffer, bytes.byteOffset + binStart + 8, binLength) }
}

test('a box round-trips as a well-formed glb', () => {
  const mesh = box(vec3(2, 2, 2), colour(1, 0, 0))
  const { json, bin } = readGlb(exportGlb(mesh))

  assert.equal(json.asset.version, '2.0')
  assert.equal(json.accessors.length, 3)
  const corners = mesh.triangles.length * 3
  for (const accessor of json.accessors) assert.equal(accessor.count, corners)
  assert.deepEqual(json.accessors[0].min, [-1, -1, -1])
  assert.deepEqual(json.accessors[0].max, [1, 1, 1])
  assert.equal(json.buffers[0].byteLength, bin.byteLength)

  for (const bufferView of json.bufferViews) {
    assert.equal(bufferView.byteOffset % 4, 0, 'buffer view aligned')
    assert.ok(bufferView.byteOffset + bufferView.byteLength <= bin.byteLength)
  }
})

test('every triangle carries its own colour, converted to linear', () => {
  const red = colour(1, 0, 0)
  const mesh = box(vec3(1, 1, 1), red)
  const { json, bin } = readGlb(exportGlb(mesh))
  const view = json.bufferViews[json.accessors[2].bufferView]
  assert.equal(bin.getFloat32(view.byteOffset, true), toLinear(red.r))
  assert.equal(bin.getFloat32(view.byteOffset + 4, true), toLinear(red.g))
  assert.equal(bin.getFloat32(view.byteOffset + 12, true), 1, 'alpha')
})

test('the file declares lights, so a viewer that honours them matches the preview', () => {
  const { json } = readGlb(exportGlb(box(vec3(1, 1, 1), colour(0.5, 0.5, 0.5))))
  assert.deepEqual(json.extensionsUsed, ['KHR_lights_punctual'])
  assert.equal(json.extensions.KHR_lights_punctual.lights.length, 2)
  assert.equal(json.nodes.filter((n: any) => n.extensions).length, 2)
})

test('an empty mesh still writes a readable file', () => {
  const { json } = readGlb(exportGlb({ triangles: [] }))
  assert.equal(json.accessors[0].count, 0)
  assert.deepEqual(json.accessors[0].min, [0, 0, 0])
})

test('a positions buffer holds the vertices it was given', () => {
  const mesh = extrude({ points: [vec2(0, 0), vec2(1, 0), vec2(0, 1)], closed: true }, 1, colour(0, 0, 1))
  const { json, bin } = readGlb(exportGlb(mesh))
  const view = json.bufferViews[json.accessors[0].bufferView]
  assert.equal(view.byteLength, mesh.triangles.length * 3 * 3 * 4)
  assert.equal(bin.getFloat32(view.byteOffset, true), mesh.triangles[0].a.x)
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
