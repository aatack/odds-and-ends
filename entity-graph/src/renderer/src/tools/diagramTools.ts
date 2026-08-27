import { emptyEntity } from '../../../core/entity'
import {
  DEFAULT_HEIGHT,
  DEFAULT_WIDTH,
  aspectRatioOf,
  nextShapeKey,
  shapesOf,
  type Point,
  type Shape,
} from '../../../core/diagram'
import { diagramAtom, editDiagramShape, type DiagramFocus } from '../state/diagram'
import { rowsOf } from '../state/query'
import { focusOf, getLayout } from '../state/store'
import { last } from '../state/types'
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
 * next step down the stagger.
 *
 * `hasDefault` rather than `optional`, and this is the difference between a
 * gesture that works and one that stops to ask: an *empty* argument opens the
 * palette however unimportant it is, and only a defaulted one lets the call run
 * straight through. So every argument on a tool the canvas invokes carries a
 * default, and the tool decides what nothing means.
 */
const placeArgs: ArgSpec[] = [
  { name: 'x', label: 'x', kind: 'number', fromContext: 'shapeX', hasDefault: true },
  { name: 'y', label: 'y', kind: 'number', fromContext: 'shapeY', hasDefault: true },
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

/**
 * What the canvas under the cursor has selected — and null unless the row the
 * frame's selection is on is that very diagram. A canvas holds its selection
 * while the cursor walks away from it, so without the second half of that
 * question Backspace on a row three below a diagram would rub out a rectangle.
 */
function selectionHere(): DiagramFocus | null {
  const focus = diagramAtom.get()
  if (!focus?.selected.length) return null
  const layout = getLayout()
  const { selectedPath } = rowsOf(focusOf(layout).frameId, layout)
  return last(selectedPath) === focus.entityId ? focus : null
}

/**
 * The two that take a key off something else, and so have to be found before it:
 * the router hands a press to the first tool in the registry that binds the key
 * and says it applies, and both of these apply only while a shape is selected.
 * See `./registry`, which is where that order is decided.
 */
export const DIAGRAM_SELECTION_TOOLS: ToolSpec[] = [
  {
    // Backspace and Delete otherwise unlink the row itself, which for a diagram
    // means throwing the whole picture away to remove one box.
    id: 'diagram.selection.remove',
    label: 'Remove selected shapes',
    aliases: ['diagram', 'delete', 'rub out'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: 'Backspace' }, { key: 'Delete' }],
    enabled: () => selectionHere() != null,
    run: async () => {
      const focus = selectionHere()
      if (!focus) return
      // One write per shape: they are separate values, and a removal that half
      // failed should leave the half that worked.
      for (const key of focus.selected) await writeValue(focus.entityId, key, null)
    },
  },
  {
    // Enter otherwise starts a new note under the row. With nothing selected on
    // the canvas that is still what it does — this only applies when there is a
    // shape for it to mean instead.
    id: 'diagram.selection.edit',
    label: 'Edit a diagram shape',
    aliases: ['diagram', 'label', 'text', 'rename'],
    hint: 'Diagram',
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Enter' }],
    enabled: () => selectionHere() != null,
    run: () => {
      const focus = selectionHere()
      if (focus) editDiagramShape(focus.entityId, focus.selected[0])
    },
  },
]

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
      { name: 'from', label: 'From', kind: 'json', fromContext: 'shapeFrom', hasDefault: true },
      { name: 'to', label: 'To', kind: 'json', fromContext: 'shapeTo', hasDefault: true },
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
    // A shape rather than nothing: this tool writes one, and `diagram.shape.remove`
    // is how a key is cleared. Letting the null through would rub a shape out on
    // its way past.
    run: async ({ entityId, key, shape }) => {
      if (shape == null) throw new Error('A shape is required')
      await writeValue(requireId(entityId, 'Entity id'), requireId(key, 'Shape key'), shape)
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
    // Read the same way the canvas reads it, so `16:9` typed into the palette and
    // `16:9` already on the entity both come back as the number they mean — and so
    // a call with nothing in it is the way back to the default.
    run: async ({ entityId, ratio }) => {
      const said = typeof ratio === 'number' ? ratio : String(ratio ?? '')
      await writeValue(
        requireId(entityId, 'Entity id'),
        'aspectRatio',
        aspectRatioOf({ aspectRatio: said }),
      )
    },
  },
]
