/**
 * The built-in transforms: the vocabulary a model is written in.
 *
 * A transform is data — sockets in, sockets out, and a pure function between
 * them. Nothing here knows about the graph, the store or the UI, which is what
 * makes the set extensible: a user's own model becomes a transform of the same
 * shape (see `modelDef` in `graph.ts`), and a scripted one later would only need
 * to supply the same three things.
 */

import * as g from './geometry'
import type { Colour, Mesh, Path2, Path3, Value, ValueType, Vec2, Vec3 } from './values'
import { TYPE_LABELS, VALUE_TYPES, defaultValue, path2, path3, vec2, vec3 } from './values'

export interface Socket {
  name: string
  type: ValueType
  label: string
}

/** An input socket: connectable, and worth its default until something is. */
export interface Input extends Socket {
  default: Value
}

/** A literal the node carries that cannot be connected — a constant's value. */
export interface Param extends Socket {
  default: Value
}

export interface TransformDef {
  id: string
  label: string
  category: string
  summary: string
  params: Param[]
  inputs: Input[]
  outputs: Socket[]
  /** Absent for a model: the evaluator walks its graph instead. */
  evaluate?: (inputs: Record<string, any>, params: Record<string, any>) => Record<string, unknown>
}

const inp = (name: string, type: ValueType, label: string, value?: Value): Input => ({
  name,
  type,
  label,
  default: value ?? defaultValue(type),
})

const out = (name: string, type: ValueType, label: string): Socket => ({ name, type, label })

interface DefInit {
  id: string
  label: string
  category: string
  summary: string
  params?: Param[]
  inputs?: Input[]
  outputs: Socket[]
  evaluate?: TransformDef['evaluate']
}

const def = (d: DefInit): TransformDef => ({ params: [], inputs: [], ...d })

export const CATEGORIES = [
  'Constants',
  'Ports',
  'Numbers',
  'Points',
  '2D shapes',
  '2D operations',
  '3D operations',
  'Convert',
  'Solids',
  'Mesh operations',
] as const

// ---------------------------------------------------------------------------
// Constants and ports — one per value type, so the set never drifts from
// `VALUE_TYPES`. Dragging an input handle into empty space spawns the constant
// matching that socket's type; a port is what gives a model of your own a
// socket when it is used as a transform.
// ---------------------------------------------------------------------------

export const constantId = (type: ValueType): string => `const.${type}`
export const inputPortId = (type: ValueType): string => `port.in.${type}`
export const outputPortId = (type: ValueType): string => `port.out.${type}`

const constants: TransformDef[] = VALUE_TYPES.filter((t) => t !== 'mesh').map((type) =>
  def({
    id: constantId(type),
    label: TYPE_LABELS[type],
    category: 'Constants',
    summary: `A fixed ${TYPE_LABELS[type].toLowerCase()}, edited on the node.`,
    params: [{ name: 'value', type, label: 'Value', default: defaultValue(type) }],
    outputs: [out('value', type, TYPE_LABELS[type])],
    evaluate: (_i, p) => ({ value: p.value }),
  }),
)

const ports: TransformDef[] = VALUE_TYPES.flatMap((type) => [
  def({
    id: inputPortId(type),
    label: `In · ${TYPE_LABELS[type]}`,
    category: 'Ports',
    summary: `An input of this model, carrying a ${TYPE_LABELS[type].toLowerCase()}.`,
    params: [
      { name: 'name', type: 'text', label: 'Name', default: type },
      { name: 'value', type, label: 'Default', default: defaultValue(type) },
    ],
    outputs: [out('value', type, TYPE_LABELS[type])],
    // Bound by the evaluator when the model is called; standing alone it is
    // worth its default, which is what makes a model previewable on its own.
    evaluate: (_i, p) => ({ value: p.value }),
  }),
  def({
    id: outputPortId(type),
    label: `Out · ${TYPE_LABELS[type]}`,
    category: 'Ports',
    summary: `An output of this model, carrying a ${TYPE_LABELS[type].toLowerCase()}.`,
    params: [{ name: 'name', type: 'text', label: 'Name', default: type }],
    inputs: [inp('value', type, TYPE_LABELS[type])],
    outputs: [],
    evaluate: (i) => ({ value: i.value }),
  }),
])

/** Is this transform an input/output port of the model it sits in? */
export const isInputPort = (id: string): boolean => id.startsWith('port.in.')
export const isOutputPort = (id: string): boolean => id.startsWith('port.out.')
export const portType = (id: string): ValueType => id.slice(id.lastIndexOf('.') + 1) as ValueType

// ---------------------------------------------------------------------------
// Numbers
// ---------------------------------------------------------------------------

const numbers: TransformDef[] = [
  def({
    id: 'number.add',
    label: 'Add',
    category: 'Numbers',
    summary: 'a + b',
    inputs: [inp('a', 'number', 'a'), inp('b', 'number', 'b')],
    outputs: [out('value', 'number', 'Sum')],
    evaluate: (i) => ({ value: i.a + i.b }),
  }),
  def({
    id: 'number.subtract',
    label: 'Subtract',
    category: 'Numbers',
    summary: 'a − b',
    inputs: [inp('a', 'number', 'a'), inp('b', 'number', 'b')],
    outputs: [out('value', 'number', 'Difference')],
    evaluate: (i) => ({ value: i.a - i.b }),
  }),
  def({
    id: 'number.multiply',
    label: 'Multiply',
    category: 'Numbers',
    summary: 'a × b',
    inputs: [inp('a', 'number', 'a', 1), inp('b', 'number', 'b', 1)],
    outputs: [out('value', 'number', 'Product')],
    evaluate: (i) => ({ value: i.a * i.b }),
  }),
  def({
    id: 'number.divide',
    label: 'Divide',
    category: 'Numbers',
    summary: 'a ÷ b',
    inputs: [inp('a', 'number', 'a', 1), inp('b', 'number', 'b', 1)],
    outputs: [out('value', 'number', 'Quotient')],
    evaluate: (i) => {
      if (i.b === 0) throw new Error('divide by zero')
      return { value: i.a / i.b }
    },
  }),
  def({
    id: 'number.sine',
    label: 'Sine',
    category: 'Numbers',
    summary: 'sin of an angle in degrees',
    inputs: [inp('degrees', 'number', 'Degrees')],
    outputs: [out('value', 'number', 'Sine')],
    evaluate: (i) => ({ value: Math.sin((i.degrees * Math.PI) / 180) }),
  }),
  def({
    id: 'number.cosine',
    label: 'Cosine',
    category: 'Numbers',
    summary: 'cos of an angle in degrees',
    inputs: [inp('degrees', 'number', 'Degrees')],
    outputs: [out('value', 'number', 'Cosine')],
    evaluate: (i) => ({ value: Math.cos((i.degrees * Math.PI) / 180) }),
  }),
]

// ---------------------------------------------------------------------------
// Points
// ---------------------------------------------------------------------------

const points: TransformDef[] = [
  def({
    id: 'vec2.make',
    label: 'Make 2D point',
    category: 'Points',
    summary: 'Two numbers into a 2D point.',
    inputs: [inp('x', 'number', 'x'), inp('y', 'number', 'y')],
    outputs: [out('value', 'vec2', 'Point')],
    evaluate: (i) => ({ value: vec2(i.x, i.y) }),
  }),
  def({
    id: 'vec2.split',
    label: 'Split 2D point',
    category: 'Points',
    summary: 'A 2D point into its components.',
    inputs: [inp('point', 'vec2', 'Point')],
    outputs: [out('x', 'number', 'x'), out('y', 'number', 'y')],
    evaluate: (i) => ({ x: (i.point as Vec2).x, y: (i.point as Vec2).y }),
  }),
  def({
    id: 'vec3.make',
    label: 'Make 3D point',
    category: 'Points',
    summary: 'Three numbers into a 3D point.',
    inputs: [inp('x', 'number', 'x'), inp('y', 'number', 'y'), inp('z', 'number', 'z')],
    outputs: [out('value', 'vec3', 'Point')],
    evaluate: (i) => ({ value: vec3(i.x, i.y, i.z) }),
  }),
  def({
    id: 'vec3.split',
    label: 'Split 3D point',
    category: 'Points',
    summary: 'A 3D point into its components.',
    inputs: [inp('point', 'vec3', 'Point')],
    outputs: [out('x', 'number', 'x'), out('y', 'number', 'y'), out('z', 'number', 'z')],
    evaluate: (i) => ({ x: (i.point as Vec3).x, y: (i.point as Vec3).y, z: (i.point as Vec3).z }),
  }),
  def({
    id: 'vec2.scale',
    label: 'Scale 2D point',
    category: 'Points',
    summary: 'A 2D point times a number.',
    inputs: [inp('point', 'vec2', 'Point'), inp('factor', 'number', 'Factor', 1)],
    outputs: [out('value', 'vec2', 'Point')],
    evaluate: (i) => ({ value: g.scale2(i.point, i.factor) }),
  }),
  def({
    id: 'vec3.scale',
    label: 'Scale 3D point',
    category: 'Points',
    summary: 'A 3D point times a number.',
    inputs: [inp('point', 'vec3', 'Point'), inp('factor', 'number', 'Factor', 1)],
    outputs: [out('value', 'vec3', 'Point')],
    evaluate: (i) => ({ value: g.scale3(i.point, i.factor) }),
  }),
]

// ---------------------------------------------------------------------------
// 2D shapes
// ---------------------------------------------------------------------------

const shapes2d: TransformDef[] = [
  def({
    id: 'shape2.rectangle',
    label: 'Rectangle',
    category: '2D shapes',
    summary: 'A rectangle centred on the origin.',
    inputs: [inp('width', 'number', 'Width', 1), inp('height', 'number', 'Height', 1)],
    outputs: [out('path', 'path2', 'Outline')],
    evaluate: (i) => {
      const w = i.width / 2
      const h = i.height / 2
      return { path: path2([vec2(-w, -h), vec2(w, -h), vec2(w, h), vec2(-w, h)]) }
    },
  }),
  def({
    id: 'shape2.polygon',
    label: 'Regular polygon',
    category: '2D shapes',
    summary: 'An n-sided polygon on a circle.',
    inputs: [inp('sides', 'number', 'Sides', 6), inp('radius', 'number', 'Radius', 1)],
    outputs: [out('path', 'path2', 'Outline')],
    evaluate: (i) => ({ path: g.regularPolygon(i.sides, i.radius) }),
  }),
  def({
    id: 'shape2.circle',
    label: 'Circle',
    category: '2D shapes',
    summary: 'A circle approximated by segments.',
    inputs: [inp('radius', 'number', 'Radius', 1), inp('segments', 'number', 'Segments', 32)],
    outputs: [out('path', 'path2', 'Outline')],
    evaluate: (i) => ({ path: g.regularPolygon(i.segments, i.radius) }),
  }),
  def({
    id: 'shape2.star',
    label: 'Star',
    category: '2D shapes',
    summary: 'Alternating outer and inner radii.',
    inputs: [
      inp('points', 'number', 'Points', 5),
      inp('outer', 'number', 'Outer radius', 1),
      inp('inner', 'number', 'Inner radius', 0.45),
    ],
    outputs: [out('path', 'path2', 'Outline')],
    evaluate: (i) => {
      const n = Math.max(3, Math.round(i.points))
      const pts: Vec2[] = []
      for (let k = 0; k < n * 2; k++) {
        const r = k % 2 === 0 ? i.outer : i.inner
        const a = (Math.PI * k) / n
        pts.push(vec2(Math.cos(a) * r, Math.sin(a) * r))
      }
      return { path: path2(pts) }
    },
  }),
  def({
    id: 'shape2.line',
    label: 'Line',
    category: '2D shapes',
    summary: 'An open path between two points.',
    inputs: [inp('from', 'vec2', 'From'), inp('to', 'vec2', 'To', vec2(1, 0))],
    outputs: [out('path', 'path2', 'Path')],
    evaluate: (i) => ({ path: path2([i.from, i.to], false) }),
  }),
]

// ---------------------------------------------------------------------------
// 2D operations
// ---------------------------------------------------------------------------

const ops2d: TransformDef[] = [
  def({
    id: 'path2.translate',
    label: 'Translate 2D',
    category: '2D operations',
    summary: 'Shift a 2D path.',
    inputs: [inp('path', 'path2', 'Path'), inp('offset', 'vec2', 'Offset')],
    outputs: [out('path', 'path2', 'Path')],
    evaluate: (i) => ({
      path: { ...i.path, points: (i.path as Path2).points.map((p) => g.add2(p, i.offset)) },
    }),
  }),
  def({
    id: 'path2.rotate',
    label: 'Rotate 2D',
    category: '2D operations',
    summary: 'Turn a 2D path about the origin.',
    inputs: [inp('path', 'path2', 'Path'), inp('degrees', 'number', 'Degrees')],
    outputs: [out('path', 'path2', 'Path')],
    evaluate: (i) => ({
      path: { ...i.path, points: (i.path as Path2).points.map((p) => g.rotate2(p, i.degrees)) },
    }),
  }),
  def({
    id: 'path2.scale',
    label: 'Scale 2D',
    category: '2D operations',
    summary: 'Scale a 2D path about the origin.',
    inputs: [inp('path', 'path2', 'Path'), inp('factor', 'vec2', 'Factor', vec2(1, 1))],
    outputs: [out('path', 'path2', 'Path')],
    evaluate: (i) => ({
      path: {
        ...i.path,
        points: (i.path as Path2).points.map((p) => vec2(p.x * i.factor.x, p.y * i.factor.y)),
      },
    }),
  }),
  def({
    id: 'path2.reverse',
    label: 'Reverse 2D path',
    category: '2D operations',
    summary: 'Turn a path inside out, flipping which side a fill faces.',
    inputs: [inp('path', 'path2', 'Path')],
    outputs: [out('path', 'path2', 'Path')],
    evaluate: (i) => ({ path: { ...i.path, points: [...(i.path as Path2).points].reverse() } }),
  }),
  def({
    id: 'path2.point',
    label: 'Point of 2D path',
    category: '2D operations',
    summary: 'One point out of a path, wrapping round.',
    inputs: [inp('path', 'path2', 'Path'), inp('index', 'number', 'Index')],
    outputs: [out('point', 'vec2', 'Point')],
    evaluate: (i) => {
      const pts = (i.path as Path2).points
      if (pts.length === 0) throw new Error('the path has no points')
      const k = ((Math.round(i.index) % pts.length) + pts.length) % pts.length
      return { point: pts[k] }
    },
  }),
  def({
    id: 'path2.join',
    label: 'Join 2D paths',
    category: '2D operations',
    summary: 'One path after the other.',
    inputs: [inp('a', 'path2', 'First'), inp('b', 'path2', 'Second')],
    outputs: [out('path', 'path2', 'Path')],
    evaluate: (i) => ({
      path: path2([...(i.a as Path2).points, ...(i.b as Path2).points], (i.a as Path2).closed),
    }),
  }),
]

// ---------------------------------------------------------------------------
// 3D operations
// ---------------------------------------------------------------------------

const ops3d: TransformDef[] = [
  def({
    id: 'path3.translate',
    label: 'Translate 3D path',
    category: '3D operations',
    summary: 'Shift a 3D path.',
    inputs: [inp('path', 'path3', 'Path'), inp('offset', 'vec3', 'Offset')],
    outputs: [out('path', 'path3', 'Path')],
    evaluate: (i) => ({
      path: { ...i.path, points: (i.path as Path3).points.map((p) => g.add3(p, i.offset)) },
    }),
  }),
  def({
    id: 'path3.rotate',
    label: 'Rotate 3D path',
    category: '3D operations',
    summary: 'Turn a 3D path about an axis through the origin.',
    inputs: [
      inp('path', 'path3', 'Path'),
      inp('axis', 'vec3', 'Axis', vec3(0, 1, 0)),
      inp('degrees', 'number', 'Degrees'),
    ],
    outputs: [out('path', 'path3', 'Path')],
    evaluate: (i) => ({
      path: {
        ...i.path,
        points: (i.path as Path3).points.map((p) => g.rotateAxis(p, i.axis, i.degrees)),
      },
    }),
  }),
  def({
    id: 'path3.scale',
    label: 'Scale 3D path',
    category: '3D operations',
    summary: 'Scale a 3D path about the origin.',
    inputs: [inp('path', 'path3', 'Path'), inp('factor', 'vec3', 'Factor', vec3(1, 1, 1))],
    outputs: [out('path', 'path3', 'Path')],
    evaluate: (i) => ({
      path: { ...i.path, points: (i.path as Path3).points.map((p) => g.mul3(p, i.factor)) },
    }),
  }),
]

// ---------------------------------------------------------------------------
// Convert — the 2D world onto the 3D one, and back
// ---------------------------------------------------------------------------

const converters: TransformDef[] = [
  def({
    id: 'convert.point',
    label: '2D point → 3D',
    category: 'Convert',
    summary: 'Lift a 2D point onto the ground plane at a height.',
    inputs: [inp('point', 'vec2', 'Point'), inp('height', 'number', 'Height')],
    outputs: [out('value', 'vec3', 'Point')],
    evaluate: (i) => ({ value: g.lift(i.point, i.height) }),
  }),
  def({
    id: 'convert.path',
    label: '2D path → 3D',
    category: 'Convert',
    summary: 'Lift a 2D path onto the ground plane at a height.',
    inputs: [inp('path', 'path2', 'Path'), inp('height', 'number', 'Height')],
    outputs: [out('value', 'path3', 'Path')],
    evaluate: (i) => ({
      value: path3((i.path as Path2).points.map((p) => g.lift(p, i.height)), (i.path as Path2).closed),
    }),
  }),
  def({
    id: 'convert.flattenPoint',
    label: '3D point → 2D',
    category: 'Convert',
    summary: 'Drop a 3D point onto the ground plane.',
    inputs: [inp('point', 'vec3', 'Point')],
    outputs: [out('value', 'vec2', 'Point')],
    evaluate: (i) => ({ value: g.flatten(i.point) }),
  }),
  def({
    id: 'convert.flattenPath',
    label: '3D path → 2D',
    category: 'Convert',
    summary: 'Drop a 3D path onto the ground plane.',
    inputs: [inp('path', 'path3', 'Path')],
    outputs: [out('value', 'path2', 'Path')],
    evaluate: (i) => ({
      value: path2((i.path as Path3).points.map(g.flatten), (i.path as Path3).closed),
    }),
  }),
]

// ---------------------------------------------------------------------------
// Solids
// ---------------------------------------------------------------------------

const solids: TransformDef[] = [
  def({
    id: 'solid.fill',
    label: 'Fill',
    category: 'Solids',
    summary: 'A flat polygon lying on the ground plane.',
    inputs: [
      inp('path', 'path2', 'Outline'),
      inp('height', 'number', 'Height'),
      inp('colour', 'colour', 'Colour'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.fill(i.path, i.colour, i.height) }),
  }),
  def({
    id: 'solid.extrude',
    label: 'Extrude',
    category: 'Solids',
    summary: 'A closed 2D outline swept upwards, capped at both ends.',
    inputs: [
      inp('path', 'path2', 'Outline'),
      inp('height', 'number', 'Height', 1),
      inp('colour', 'colour', 'Colour'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.extrude(i.path, i.height, i.colour) }),
  }),
  def({
    id: 'solid.revolve',
    label: 'Revolve',
    category: 'Solids',
    summary: 'A profile spun about the Y axis — x is a radius, y a height.',
    inputs: [
      inp('profile', 'path2', 'Profile'),
      inp('segments', 'number', 'Segments', 32),
      inp('degrees', 'number', 'Degrees', 360),
      inp('colour', 'colour', 'Colour'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.revolve(i.profile, i.segments, i.colour, i.degrees) }),
  }),
  def({
    id: 'solid.loft',
    label: 'Loft',
    category: 'Solids',
    summary: 'A skin between two 3D paths, point for point.',
    inputs: [
      inp('a', 'path3', 'From'),
      inp('b', 'path3', 'To'),
      inp('colour', 'colour', 'Colour'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.loft(i.a, i.b, i.colour) }),
  }),
  def({
    id: 'solid.box',
    label: 'Box',
    category: 'Solids',
    summary: 'A box centred on the origin.',
    inputs: [inp('size', 'vec3', 'Size', vec3(1, 1, 1)), inp('colour', 'colour', 'Colour')],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.box(i.size, i.colour) }),
  }),
  def({
    id: 'solid.sphere',
    label: 'Sphere',
    category: 'Solids',
    summary: 'A sphere centred on the origin.',
    inputs: [
      inp('radius', 'number', 'Radius', 0.5),
      inp('segments', 'number', 'Segments', 24),
      inp('colour', 'colour', 'Colour'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.sphere(i.radius, i.segments, i.colour) }),
  }),
  def({
    id: 'solid.cylinder',
    label: 'Cylinder',
    category: 'Solids',
    summary: 'A cylinder standing on the ground plane.',
    inputs: [
      inp('radius', 'number', 'Radius', 0.5),
      inp('height', 'number', 'Height', 1),
      inp('segments', 'number', 'Segments', 24),
      inp('colour', 'colour', 'Colour'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.cylinder(i.radius, i.height, i.segments, i.colour) }),
  }),
]

// ---------------------------------------------------------------------------
// Mesh operations
// ---------------------------------------------------------------------------

const meshOps: TransformDef[] = [
  def({
    id: 'mesh.translate',
    label: 'Translate',
    category: 'Mesh operations',
    summary: 'Shift a mesh.',
    inputs: [inp('mesh', 'mesh', 'Mesh'), inp('offset', 'vec3', 'Offset')],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.mapMesh(i.mesh, (p) => g.add3(p, i.offset)) }),
  }),
  def({
    id: 'mesh.rotate',
    label: 'Rotate',
    category: 'Mesh operations',
    summary: 'Turn a mesh about an axis through the origin.',
    inputs: [
      inp('mesh', 'mesh', 'Mesh'),
      inp('axis', 'vec3', 'Axis', vec3(0, 1, 0)),
      inp('degrees', 'number', 'Degrees'),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({ mesh: g.mapMesh(i.mesh, (p) => g.rotateAxis(p, i.axis, i.degrees)) }),
  }),
  def({
    id: 'mesh.scale',
    label: 'Scale',
    category: 'Mesh operations',
    summary: 'Scale a mesh about the origin.',
    inputs: [inp('mesh', 'mesh', 'Mesh'), inp('factor', 'vec3', 'Factor', vec3(1, 1, 1))],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => {
      const scaled = g.mapMesh(i.mesh, (p) => g.mul3(p, i.factor))
      const flipped = i.factor.x * i.factor.y * i.factor.z < 0
      return { mesh: flipped ? g.flipMesh(scaled) : scaled }
    },
  }),
  def({
    id: 'mesh.colour',
    label: 'Paint',
    category: 'Mesh operations',
    summary: 'Give every triangle the same colour.',
    inputs: [inp('mesh', 'mesh', 'Mesh'), inp('colour', 'colour', 'Colour')],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({
      mesh: { triangles: (i.mesh as Mesh).triangles.map((t) => ({ ...t, colour: i.colour as Colour })) },
    }),
  }),
  def({
    id: 'mesh.combine',
    label: 'Combine',
    category: 'Mesh operations',
    summary: 'Two meshes as one. Not a boolean union — the triangles are simply pooled.',
    inputs: [inp('a', 'mesh', 'First'), inp('b', 'mesh', 'Second')],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => ({
      mesh: { triangles: [...(i.a as Mesh).triangles, ...(i.b as Mesh).triangles] },
    }),
  }),
  def({
    id: 'mesh.mirror',
    label: 'Mirror',
    category: 'Mesh operations',
    summary: 'Reflect a mesh through the plane with this normal.',
    inputs: [inp('mesh', 'mesh', 'Mesh'), inp('normal', 'vec3', 'Normal', vec3(1, 0, 0))],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => {
      const n = g.normalise3(i.normal)
      if (g.length3(n) === 0) return { mesh: i.mesh }
      return {
        mesh: g.flipMesh(g.mapMesh(i.mesh, (p) => g.sub3(p, g.scale3(n, 2 * g.dot3(p, n))))),
      }
    },
  }),
  def({
    id: 'mesh.repeat',
    label: 'Repeat',
    category: 'Mesh operations',
    summary: 'Copies of a mesh, each one offset further along.',
    inputs: [
      inp('mesh', 'mesh', 'Mesh'),
      inp('count', 'number', 'Count', 3),
      inp('offset', 'vec3', 'Offset', vec3(1, 0, 0)),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => {
      const n = Math.max(0, Math.round(i.count))
      const triangles = []
      for (let k = 0; k < n; k++) {
        triangles.push(...g.mapMesh(i.mesh, (p) => g.add3(p, g.scale3(i.offset, k))).triangles)
      }
      return { mesh: { triangles } }
    },
  }),
  def({
    id: 'mesh.radial',
    label: 'Radial array',
    category: 'Mesh operations',
    summary: 'Copies of a mesh spun evenly about an axis.',
    inputs: [
      inp('mesh', 'mesh', 'Mesh'),
      inp('count', 'number', 'Count', 6),
      inp('axis', 'vec3', 'Axis', vec3(0, 1, 0)),
      inp('degrees', 'number', 'Sweep', 360),
    ],
    outputs: [out('mesh', 'mesh', 'Mesh')],
    evaluate: (i) => {
      const n = Math.max(1, Math.round(i.count))
      const triangles = []
      for (let k = 0; k < n; k++) {
        const angle = (i.degrees * k) / n
        triangles.push(...g.mapMesh(i.mesh, (p) => g.rotateAxis(p, i.axis, angle)).triangles)
      }
      return { mesh: { triangles } }
    },
  }),
]

// ---------------------------------------------------------------------------

export const BUILT_IN: TransformDef[] = [
  ...constants,
  ...ports,
  ...numbers,
  ...points,
  ...shapes2d,
  ...ops2d,
  ...ops3d,
  ...converters,
  ...solids,
  ...meshOps,
]

const byId = new Map(BUILT_IN.map((d) => [d.id, d]))

export const builtIn = (id: string): TransformDef | null => byId.get(id) ?? null
