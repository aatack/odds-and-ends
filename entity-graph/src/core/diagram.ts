// What a `diagram` entity holds: a picture, read off its own values.
//
// A diagram is not a subtree. Every shape is one value on the entity, under a key
// beginning `diagram/`, which is what lets a diagram still have notes under it —
// the children are the notes, as they are on every other row — and what makes
// moving one rectangle a write of one key rather than a rewrite of the whole
// picture.
//
// Nothing here reads the store: these are pure readings of an entity's values, in
// the manner of `./schema`, so the app that draws a diagram, the phone and an
// agent over MCP all agree about what a `diagram/…` key says. `./builtins` is
// where that agreement is written down for a reader who only has the schema.

/** A point in the diagram's own coordinates. Positive y is down, as on screen. */
export interface Point {
  x: number
  y: number
}

/**
 * Where an arrow ends: on one of the diagram's other shapes, named by its key, or
 * at a bare point. A key is the ordinary case — an arrow tied to a box follows it
 * when the box moves — and a point is what an end that is tied to nothing has.
 */
export type Endpoint = string | Point

/** A shape with a box: a rectangle, or a line of text with nothing drawn round it. */
export interface BoxShape extends Point {
  key: string
  shape: 'rectangle' | 'text'
  width: number
  height: number
  text: string
}

/** A line with a head on it, between two {@link Endpoint}s. */
export interface ArrowShape {
  key: string
  shape: 'arrow'
  from: Endpoint
  to: Endpoint
  text: string
}

export type Shape = BoxShape | ArrowShape

export const isArrow = (shape: Shape): shape is ArrowShape => shape.shape === 'arrow'
export const isBox = (shape: Shape): shape is BoxShape => shape.shape !== 'arrow'

/** What every key holding a shape begins with. */
export const SHAPE_PREFIX = 'diagram/'

/** The shape of a canvas nobody has said otherwise about: sixteen by nine. */
export const DEFAULT_ASPECT_RATIO = 16 / 9

/** A new box, before anything has been dragged. Wide enough for a few words. */
export const DEFAULT_WIDTH = 160
export const DEFAULT_HEIGHT = 64

export const isShapeKey = (key: string): boolean => key.startsWith(SHAPE_PREFIX)

const isObject = (v: unknown): v is Record<string, unknown> =>
  !!v && typeof v === 'object' && !Array.isArray(v)

/** A number, or a fallback where the value isn't one. NaN and Infinity aren't. */
const num = (v: unknown, fallback: number): number =>
  typeof v === 'number' && Number.isFinite(v) ? v : fallback

const text = (v: unknown): string => (typeof v === 'string' ? v : '')

/**
 * An endpoint as written. A string names another shape — either the whole key or
 * the bare id after the prefix, since a hand writing one of these will drop the
 * prefix as often as not — and an object with two numbers in it is a point.
 * Anything else has no end, which the caller draws at the origin.
 */
function endpointOf(value: unknown): Endpoint {
  if (typeof value === 'string' && value.trim()) {
    const name = value.trim()
    return isShapeKey(name) ? name : SHAPE_PREFIX + name
  }
  if (isObject(value)) return { x: num(value.x, 0), y: num(value.y, 0) }
  return { x: 0, y: 0 }
}

/**
 * One shape, read loosely: a value that says which shape it is is drawn with
 * whatever else it holds and defaults for the rest, and a value that says nothing
 * recognisable is not a shape at all. Loose because these are hand-written as
 * often as they are dragged — an agent writing a diagram over MCP gets the same
 * benefit of the doubt an agent writing a schema does.
 */
export function shapeOf(key: string, value: unknown): Shape | null {
  if (!isObject(value)) return null
  const kind = value.shape
  if (kind === 'arrow') {
    return {
      key,
      shape: 'arrow',
      from: endpointOf(value.from),
      to: endpointOf(value.to),
      text: text(value.text),
    }
  }
  if (kind !== 'rectangle' && kind !== 'text') return null
  return {
    key,
    shape: kind,
    x: num(value.x, 0),
    y: num(value.y, 0),
    width: num(value.width, DEFAULT_WIDTH),
    height: num(value.height, DEFAULT_HEIGHT),
    text: text(value.text),
  }
}

/**
 * Every shape on an entity, in key order. Order is stable rather than meaningful:
 * the keys are a bag and nothing about a picture depends on which was written
 * first, but a canvas that reshuffles itself on every read is unreadable.
 */
export function shapesOf(values: Record<string, unknown> | undefined): Shape[] {
  if (!values) return []
  const out: Shape[] = []
  for (const key of Object.keys(values).sort()) {
    if (!isShapeKey(key)) continue
    const shape = shapeOf(key, values[key])
    if (shape) out.push(shape)
  }
  return out
}

/**
 * How wide the canvas is against its height. A bare number is the ratio itself;
 * `"16:9"` is the way a ratio is usually written down, and is taken too.
 */
export function aspectRatioOf(values: Record<string, unknown> | undefined): number {
  const value = values?.aspectRatio
  if (typeof value === 'number' && Number.isFinite(value) && value > 0) return value
  if (typeof value === 'string') {
    const parts = value.split(/[:/]/).map((part) => Number(part.trim()))
    const positive = parts.every((part) => Number.isFinite(part) && part > 0)
    if (positive && parts.length === 1) return parts[0]
    if (positive && parts.length === 2) return parts[0] / parts[1]
  }
  return DEFAULT_ASPECT_RATIO
}

/**
 * A key nothing on the entity is using: the next number up. Numbered rather than
 * random because these are read and written by hand as well as dragged, and
 * `diagram/3` is something a person can refer to in an arrow's `from`. Two clients
 * adding a shape at the same instant would choose the same number and the later
 * write would win — which is a lost rectangle, and cheap enough to redraw.
 */
export function nextShapeKey(values: Record<string, unknown> | undefined): string {
  let highest = 0
  for (const key of Object.keys(values ?? {})) {
    if (!isShapeKey(key)) continue
    const n = Number(key.slice(SHAPE_PREFIX.length))
    if (Number.isInteger(n) && n > highest) highest = n
  }
  return `${SHAPE_PREFIX}${highest + 1}`
}

/**
 * The boxes of a shape list, by key — what an arrow's ends are resolved against.
 * Where an end actually *is* is not answered here: an arrow tied to a box is drawn
 * against where that box has got to on screen, which during a drag is not yet what
 * the value says, so the one answer lives with the canvas that knows both.
 */
export const boxesOf = (shapes: readonly Shape[]): Map<string, BoxShape> =>
  new Map(shapes.filter(isBox).map((box) => [box.key, box]))
