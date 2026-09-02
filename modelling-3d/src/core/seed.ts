/**
 * What is in the store the first time the app opens: two models that between
 * them show the point of the thing — a column described once, parametrically,
 * and a colonnade that uses it eight times without knowing how it was made.
 */

import type { GraphEdge, GraphNode, Model, Models } from './graph'
import { modelTransformId } from './graph'
import { colour, vec3 } from './values'

const node = (
  id: string,
  transform: string,
  x: number,
  y: number,
  data: Record<string, unknown> = {},
): GraphNode => ({ id, transform, x, y, data })

const wire = (source: string, sourceOutput: string, target: string, targetInput: string): GraphEdge => ({
  id: `${source}.${sourceOutput}-${target}.${targetInput}`,
  source,
  sourceOutput,
  target,
  targetInput,
})

function model(id: string, name: string, order: number, nodes: GraphNode[], edges: GraphEdge[]): Model {
  return {
    id,
    name,
    order,
    nodes: Object.fromEntries(nodes.map((n) => [n.id, n])),
    edges: Object.fromEntries(edges.map((e) => [e.id, e])),
  }
}

const STONE = colour(0.85, 0.83, 0.78)
const PLINTH = colour(0.63, 0.62, 0.6)

const column = model(
  'seed-column',
  'Column',
  1,
  [
    node('c-sides', 'port.in.number', -560, -160, { name: 'sides', value: 8 }),
    node('c-radius', 'port.in.number', -560, -20, { name: 'radius', value: 0.28 }),
    node('c-height', 'port.in.number', -560, 140, { name: 'height', value: 2.4 }),
    node('c-shape', 'shape2.polygon', -280, -120),
    node('c-top', 'vec3.make', -280, 120),
    node('c-path', 'path3.line', -40, 140),
    node('c-extrude', 'solid.extrude', 200, -40),
    node('c-paint', 'mesh.colour', 440, -40, { colour: STONE }),
    node('c-out', 'port.out.mesh', 680, -40, { name: 'column' }),
  ],
  [
    wire('c-sides', 'value', 'c-shape', 'sides'),
    wire('c-radius', 'value', 'c-shape', 'radius'),
    wire('c-height', 'value', 'c-top', 'y'),
    wire('c-top', 'value', 'c-path', 'to'),
    wire('c-shape', 'path', 'c-extrude', 'path'),
    wire('c-path', 'path', 'c-extrude', 'along'),
    wire('c-extrude', 'mesh', 'c-paint', 'mesh'),
    wire('c-paint', 'mesh', 'c-out', 'value'),
  ],
)

const colonnade = model(
  'seed-colonnade',
  'Colonnade',
  0,
  [
    node('n-count', 'const.number', -400, -160, { value: 8 }),
    node('n-column', modelTransformId('seed-column'), -140, -160, { radius: 0.28, height: 2.4 }),
    node('n-outwards', 'mesh.translate', 140, -160, { offset: vec3(2.2, 0, 0) }),
    node('n-ring', 'mesh.radial', 400, -160, { axis: vec3(0, 1, 0), degrees: 360 }),
    node('n-base', 'shape2.circle', -400, 180, { radius: 2.9, segments: 48 }),
    node('n-plinth', 'solid.extrude', -140, 200, {
      along: { points: [vec3(0, 0, 0), vec3(0, 0.2, 0)], closed: false },
    }),
    node('n-paint', 'mesh.colour', 140, 200, { colour: PLINTH }),
    node('n-drop', 'mesh.translate', 400, 220, { offset: vec3(0, -0.2, 0) }),
    node('n-all', 'mesh.combine', 660, 20),
  ],
  [
    wire('n-count', 'value', 'n-column', 'sides'),
    wire('n-count', 'value', 'n-ring', 'count'),
    wire('n-column', 'column', 'n-outwards', 'mesh'),
    wire('n-outwards', 'mesh', 'n-ring', 'mesh'),
    wire('n-base', 'path', 'n-plinth', 'path'),
    wire('n-plinth', 'mesh', 'n-paint', 'mesh'),
    wire('n-paint', 'mesh', 'n-drop', 'mesh'),
    wire('n-ring', 'mesh', 'n-all', 'a'),
    wire('n-drop', 'mesh', 'n-all', 'b'),
  ],
)

export function seedModels(): Models {
  return { [column.id]: column, [colonnade.id]: colonnade }
}
