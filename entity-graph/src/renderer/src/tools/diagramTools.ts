import { emptyEntity } from '../../../core/entity'
import {
  DEFAULT_HEIGHT,
  DEFAULT_WIDTH,
  nextShapeKey,
  shapesOf,
  type Point,
  type Shape,
} from '../../../core/diagram'
import { readEntities, writeValue } from '../source/entity'
import { entityArg, requireId } from './entityTools'
import type { ArgSpec, ToolSpec } from './types'

// What the canvas over a `diagram` entity does, said as tools — so that dragging a
// rectangle and typing "add a rectangle" into the palette are the same write, and
// a script can draw a diagram the way a pointer does.
//
// Every one of them writes a single `diagram/…` value, which is the whole point of
// keeping the shapes as values: moving one box is one small event rather than a
// rewrite of the picture, and two of them landing at once cost each other nothing.
// `core/diagram` is what the values mean.

/**
 * Where a shape lands when the caller didn't say. The canvas always says — it
 * knows where the middle of the view is — so this is for a call from the palette
 * or a script, and it steps down the canvas rather than stacking everything on
 * one spot.
 */
const STAGGER = 28

const number = (v: unknown, fallback: number): number => {
  const n = typeof v === 'string' ? Number(v) : v
  return typeof n === 'number' && Number.isFinite(n) ? n : fallback
}

/**
 * Where a shape a tool is about to add goes: what the caller asked for, or the
 * next step down the stagger. Both arguments are filled from the call's context
 * and neither is typed, so the palette asks for neither.
 */
const placeArgs: ArgSpec[] = [
  { name: 'x', label: 'x', kind: 'number', fromContext: 'shapeX', optional: true },
  { name: 'y', label: 'y', kind: 'number', fromContext: 'shapeY', optional: true },
]

const placeAt = (shapes: readonly Shape[], x: unknown, y: unknown): Point => {
  const step = (shapes.length % 8) * STAGGER
  return { x: number(x, 40 + step), y: number(y, 40 + step) }
}

/**
 * An arrow's end as a caller gave it: the key of a shape to follow, a point, or
 * nothing at all. Both forms arrive whole from the canvas — a connection dragged
 * between two boxes names them, one dropped on the pane names one and a point —
 * so this is only the reading of them.
 */
function endpoint(value: unknown): string | Point | undefined {
  if (typeof value === 'string' && value.trim()) return value.trim()
  if (value && typeof value === 'object') {
    const point = value as { x?: unknown; y?: unknown }
    if (point.x !== undefined || point.y !== undefined) {
      return { x: number(point.x, 0), y: number(point.y, 0) }
    }
  }
  return undefined
}

/** The entity a diagram tool is about to write to, as the store currently holds it. */
async function diagramOf(entityId: unknown): Promise<{ id: string; values: Record<string, unknown> }> {
  const id = requireId(entityId, 'Entity id')
  const entity = (await readEntities([id]))[id] ?? emptyEntity(id)
  return { id, values: entity.values }
}

/**
 * Add one shape and hand back the key it was given, which is what an arrow drawn
 * afterwards names it by. Nothing is toasted: the shape appearing on the canvas is
 * the answer, and a toast for every rectangle would bury the ones that matter.
 */
async function addShape(
  entityId: unknown,
  make: (at: Point) => Record<string, unknown>,
  x: unknown,
  y: unknown,
): Promise<{ data: string }> {
  const { id, values } = await diagramOf(entityId)
  const shapes = shapesOf(values)
  const key = nextShapeKey(values)
  await writeValue(id, key, make(placeAt(shapes, x, y)))
  return { data: key }
}

export const DIAGRAM_TOOLS: ToolSpec[] = [
  {
    id: 'diagram.rectangle.add',
    label: 'Add a rectangle',
    aliases: ['diagram', 'box', 'shape', 'draw', 'sketch'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [entityArg(), ...placeArgs],
    run: ({ entityId, x, y }) =>
      addShape(
        entityId,
        (at) => ({ shape: 'rectangle', ...at, width: DEFAULT_WIDTH, height: DEFAULT_HEIGHT }),
        x,
        y,
      ),
  },
  {
    id: 'diagram.text.add',
    label: 'Add a text box',
    aliases: ['diagram', 'label', 'caption', 'draw', 'sketch'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [entityArg(), ...placeArgs],
    run: ({ entityId, x, y }) =>
      addShape(
        entityId,
        // Shorter than a rectangle: a caption is a line, and a box drawn round
        // nothing that is twice the height of its text reads as a mistake.
        (at) => ({ shape: 'text', ...at, width: DEFAULT_WIDTH, height: 28 }),
        x,
        y,
      ),
  },
  {
    id: 'diagram.arrow.add',
    label: 'Add an arrow',
    aliases: ['diagram', 'line', 'link', 'draw', 'sketch'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [
      entityArg(),
      ...placeArgs,
      // Either end is a shape's key — `"diagram/1"` — or a point, `{"x":0,"y":0}`.
      { name: 'from', label: 'From', kind: 'json', fromContext: 'shapeFrom', optional: true },
      { name: 'to', label: 'To', kind: 'json', fromContext: 'shapeTo', optional: true },
    ],
    // Tied to a shape at whichever end was named — which is what dragging between
    // two boxes does — and a bare point at the other, which is then a dot to drag.
    run: ({ entityId, x, y, from, to }) =>
      addShape(
        entityId,
        (at) => ({
          shape: 'arrow',
          from: endpoint(from) ?? at,
          to: endpoint(to) ?? { x: at.x + DEFAULT_WIDTH + 60, y: at.y },
        }),
        x,
        y,
      ),
  },
  {
    // What the canvas writes with: one whole shape, as the drag left it. Unlisted
    // for the same reason `entity.field.set` is — there is no typing a rectangle's
    // corner into a palette — and every argument carries a default so the call runs
    // straight through rather than stopping to collect one.
    id: 'diagram.shape.set',
    label: 'Set a diagram shape',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    listed: false,
    args: [
      entityArg(),
      { name: 'key', label: 'Shape key', fromContext: 'shapeKey', hasDefault: true },
      {
        name: 'shape',
        label: 'Shape (JSON)',
        kind: 'json',
        fromContext: 'shapeValue',
        hasDefault: true,
      },
    ],
    run: async ({ entityId, key, shape }) => {
      await writeValue(
        requireId(entityId, 'Entity id'),
        requireId(key, 'Shape key'),
        shape ?? null,
      )
    },
  },
  {
    id: 'diagram.shape.remove',
    label: 'Remove a diagram shape',
    aliases: ['diagram', 'delete', 'rub out'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [
      entityArg(),
      { name: 'key', label: 'Shape key', fromContext: 'shapeKey', hasDefault: true },
    ],
    // Null rather than gone: values come *off* an entity by being written null in
    // an append-only store, and an arrow still naming it simply stops being drawn.
    run: async ({ entityId, key }) => {
      await writeValue(requireId(entityId, 'Entity id'), requireId(key, 'Shape key'), null)
    },
  },
  {
    id: 'diagram.aspectRatio.set',
    label: "Set a diagram's proportions",
    aliases: ['diagram', 'height', 'size', 'canvas', 'aspect ratio'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [
      entityArg(),
      {
        name: 'ratio',
        label: 'Width over height',
        kind: 'number',
        fromContext: 'aspectRatio',
        hasDefault: true,
        placeholder: '1.78 for 16:9',
      },
    ],
    run: async ({ entityId, ratio }) => {
      const value = number(ratio, 0)
      if (value <= 0) throw new Error('A ratio is a positive number — 1.78 for 16:9')
      await writeValue(requireId(entityId, 'Entity id'), 'aspectRatio', value)
    },
  },
]
