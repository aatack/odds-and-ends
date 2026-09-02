/**
 * The values that flow along the edges of a model.
 *
 * Everything here is plain JSON: a value can be written to sqlite, sent over
 * IPC, or handed to a script without being reconstructed. Geometry is kept in
 * the simplest form that still says what it is — a mesh is a bag of coloured
 * triangles and nothing else, because that is exactly what the exporter and the
 * preview both want.
 */

/** The types a socket can carry. */
export const VALUE_TYPES = [
  'number',
  'text',
  'vec2',
  'vec3',
  'colour',
  'path2',
  'path3',
  'mesh',
] as const

export type ValueType = (typeof VALUE_TYPES)[number]

export interface Vec2 {
  x: number
  y: number
}

export interface Vec3 {
  x: number
  y: number
  z: number
}

/** Linear-ish sRGB components in 0..1, the way a colour picker hands them over. */
export interface Colour {
  r: number
  g: number
  b: number
}

/** A run of points; `closed` says whether the last joins back to the first. */
export interface Path2 {
  points: Vec2[]
  closed: boolean
}

export interface Path3 {
  points: Vec3[]
  closed: boolean
}

export interface Triangle {
  a: Vec3
  b: Vec3
  c: Vec3
  colour: Colour
}

export interface Mesh {
  triangles: Triangle[]
}

export interface ValueOf {
  number: number
  text: string
  vec2: Vec2
  vec3: Vec3
  colour: Colour
  path2: Path2
  path3: Path3
  mesh: Mesh
}

export type Value = ValueOf[ValueType]

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

export const vec2 = (x: number, y: number): Vec2 => ({ x, y })
export const vec3 = (x: number, y: number, z: number): Vec3 => ({ x, y, z })
export const colour = (r: number, g: number, b: number): Colour => ({ r, g, b })
export const path2 = (points: Vec2[], closed = true): Path2 => ({ points, closed })
export const path3 = (points: Vec3[], closed = false): Path3 => ({ points, closed })
export const mesh = (triangles: Triangle[]): Mesh => ({ triangles })

export const GREY: Colour = { r: 0.76, g: 0.77, b: 0.79 }

/** What an unconnected socket of this type is worth until somebody says otherwise. */
export function defaultValue(type: ValueType): Value {
  switch (type) {
    case 'number':
      return 0
    case 'text':
      return ''
    case 'vec2':
      return vec2(0, 0)
    case 'vec3':
      return vec3(0, 0, 0)
    case 'colour':
      return GREY
    case 'path2':
      return path2([vec2(-0.5, -0.5), vec2(0.5, -0.5), vec2(0.5, 0.5), vec2(-0.5, 0.5)])
    case 'path3':
      return path3([vec3(0, 0, 0), vec3(0, 1, 0)])
    case 'mesh':
      return mesh([])
  }
}

export const TYPE_LABELS: Record<ValueType, string> = {
  number: 'Number',
  text: 'Text',
  vec2: '2D point',
  vec3: '3D point',
  colour: 'Colour',
  path2: '2D path',
  path3: '3D path',
  mesh: 'Mesh',
}

/** Whether a value looks like an inhabitant of `type`, used when reading storage. */
export function isValue(type: ValueType, value: unknown): value is Value {
  const num = (v: unknown): boolean => typeof v === 'number' && Number.isFinite(v)
  const isVec2 = (v: unknown): boolean =>
    !!v && typeof v === 'object' && num((v as Vec2).x) && num((v as Vec2).y)
  const isVec3 = (v: unknown): boolean =>
    isVec2(v) && num((v as Vec3).z)
  switch (type) {
    case 'number':
      return num(value)
    case 'text':
      return typeof value === 'string'
    case 'vec2':
      return isVec2(value)
    case 'vec3':
      return isVec3(value)
    case 'colour':
      return (
        !!value &&
        typeof value === 'object' &&
        num((value as Colour).r) &&
        num((value as Colour).g) &&
        num((value as Colour).b)
      )
    case 'path2':
      return !!value && Array.isArray((value as Path2).points) && (value as Path2).points.every(isVec2)
    case 'path3':
      return !!value && Array.isArray((value as Path3).points) && (value as Path3).points.every(isVec3)
    case 'mesh':
      return !!value && Array.isArray((value as Mesh).triangles)
  }
}

/** A one-line description of a value, for a node's footer and a tooltip. */
export function describe(type: ValueType, value: unknown): string {
  const n = (v: number): string => (Math.round(v * 1000) / 1000).toString()
  switch (type) {
    case 'number':
      return n(value as number)
    case 'text':
      return JSON.stringify(value)
    case 'vec2': {
      const v = value as Vec2
      return `(${n(v.x)}, ${n(v.y)})`
    }
    case 'vec3': {
      const v = value as Vec3
      return `(${n(v.x)}, ${n(v.y)}, ${n(v.z)})`
    }
    case 'colour':
      return toHex(value as Colour)
    case 'path2':
    case 'path3': {
      const p = value as Path2
      return `${p.points.length} points${p.closed ? ', closed' : ''}`
    }
    case 'mesh':
      return `${(value as Mesh).triangles.length} triangles`
  }
}

// ---------------------------------------------------------------------------
// Colour
// ---------------------------------------------------------------------------

const clamp01 = (v: number): number => (v < 0 ? 0 : v > 1 ? 1 : v)

export function toHex(c: Colour): string {
  const part = (v: number): string =>
    Math.round(clamp01(v) * 255)
      .toString(16)
      .padStart(2, '0')
  return `#${part(c.r)}${part(c.g)}${part(c.b)}`
}

export function fromHex(hex: string): Colour {
  const h = hex.replace('#', '')
  const full = h.length === 3 ? h.split('').map((c) => c + c).join('') : h
  return {
    r: parseInt(full.slice(0, 2), 16) / 255,
    g: parseInt(full.slice(2, 4), 16) / 255,
    b: parseInt(full.slice(4, 6), 16) / 255,
  }
}

/** A colour as hue (0-360), saturation and value (0-1) — how a picker thinks. */
export interface Hsv {
  h: number
  s: number
  v: number
}

export function toHsv(c: Colour): Hsv {
  const r = clamp01(c.r)
  const g = clamp01(c.g)
  const b = clamp01(c.b)
  const max = Math.max(r, g, b)
  const min = Math.min(r, g, b)
  const span = max - min
  let h = 0
  if (span !== 0) {
    if (max === r) h = ((g - b) / span) % 6
    else if (max === g) h = (b - r) / span + 2
    else h = (r - g) / span + 4
    h *= 60
    if (h < 0) h += 360
  }
  return { h, s: max === 0 ? 0 : span / max, v: max }
}

export function fromHsv({ h, s, v }: Hsv): Colour {
  const hue = ((h % 360) + 360) % 360
  const chroma = clamp01(v) * clamp01(s)
  const second = chroma * (1 - Math.abs(((hue / 60) % 2) - 1))
  const lift = clamp01(v) - chroma
  const sector = Math.floor(hue / 60) % 6
  const [r, g, b] = [
    [chroma, second, 0],
    [second, chroma, 0],
    [0, chroma, second],
    [0, second, chroma],
    [second, 0, chroma],
    [chroma, 0, second],
  ][sector]
  return { r: r + lift, g: g + lift, b: b + lift }
}

/** sRGB → linear, which is the space glTF wants vertex colours in. */
export function toLinear(v: number): number {
  const c = clamp01(v)
  return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4)
}
