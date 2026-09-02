/**
 * The maths the transforms are made of: vectors, a polygon triangulator, and
 * the handful of surface constructions (fill, extrude, revolve, loft) that turn
 * flat input into a mesh.
 *
 * **The 2D plane is the ground.** A 2D point `(x, y)` lifts to `(x, 0, -y)`, so
 * a shape drawn with y upwards on the page lies flat with its "up" pointing away
 * from the default camera, and height runs along +Y. Every 2D → 3D conversion
 * goes through `lift`, so that choice is made in exactly one place.
 */

import type { Colour, Mesh, Path2, Path3, Triangle, Vec2, Vec3 } from './values'
import { GREY, vec2, vec3 } from './values'

// ---------------------------------------------------------------------------
// Vectors
// ---------------------------------------------------------------------------

export const add3 = (a: Vec3, b: Vec3): Vec3 => vec3(a.x + b.x, a.y + b.y, a.z + b.z)
export const sub3 = (a: Vec3, b: Vec3): Vec3 => vec3(a.x - b.x, a.y - b.y, a.z - b.z)
export const scale3 = (a: Vec3, k: number): Vec3 => vec3(a.x * k, a.y * k, a.z * k)
export const mul3 = (a: Vec3, b: Vec3): Vec3 => vec3(a.x * b.x, a.y * b.y, a.z * b.z)
export const dot3 = (a: Vec3, b: Vec3): number => a.x * b.x + a.y * b.y + a.z * b.z
export const cross3 = (a: Vec3, b: Vec3): Vec3 =>
  vec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
export const length3 = (a: Vec3): number => Math.sqrt(dot3(a, a))

export function normalise3(a: Vec3): Vec3 {
  const l = length3(a)
  return l === 0 ? vec3(0, 0, 0) : scale3(a, 1 / l)
}

export const add2 = (a: Vec2, b: Vec2): Vec2 => vec2(a.x + b.x, a.y + b.y)
export const sub2 = (a: Vec2, b: Vec2): Vec2 => vec2(a.x - b.x, a.y - b.y)
export const scale2 = (a: Vec2, k: number): Vec2 => vec2(a.x * k, a.y * k)

/** Rotate a 2D point about the origin, angle in degrees, anticlockwise. */
export function rotate2(p: Vec2, degrees: number): Vec2 {
  const r = (degrees * Math.PI) / 180
  const c = Math.cos(r)
  const s = Math.sin(r)
  return vec2(p.x * c - p.y * s, p.x * s + p.y * c)
}

/** Rotate a 3D point about an axis through the origin (Rodrigues), in degrees. */
export function rotateAxis(p: Vec3, axis: Vec3, degrees: number): Vec3 {
  const k = normalise3(axis)
  if (length3(k) === 0) return p
  const r = (degrees * Math.PI) / 180
  const c = Math.cos(r)
  const s = Math.sin(r)
  return add3(
    add3(scale3(p, c), scale3(cross3(k, p), s)),
    scale3(k, dot3(k, p) * (1 - c)),
  )
}

/** The one place the 2D plane is placed in 3D: y on the page runs to -Z. */
export const lift = (p: Vec2, height = 0): Vec3 => vec3(p.x, height, -p.y)

export const flatten = (p: Vec3): Vec2 => vec2(p.x, -p.z)

// ---------------------------------------------------------------------------
// Mesh helpers
// ---------------------------------------------------------------------------

export const triangle = (a: Vec3, b: Vec3, c: Vec3, colour: Colour = GREY): Triangle => ({
  a,
  b,
  c,
  colour,
})

/** A quad as two triangles, wound a→b→c→d. */
export function quad(a: Vec3, b: Vec3, c: Vec3, d: Vec3, colour: Colour): Triangle[] {
  return [triangle(a, b, c, colour), triangle(a, c, d, colour)]
}

export function mapMesh(m: Mesh, f: (p: Vec3) => Vec3): Mesh {
  return { triangles: m.triangles.map((t) => ({ a: f(t.a), b: f(t.b), c: f(t.c), colour: t.colour })) }
}

/** Reverse the winding of every triangle, so the surface faces the other way. */
export function flipMesh(m: Mesh): Mesh {
  return { triangles: m.triangles.map((t) => ({ a: t.a, c: t.b, b: t.c, colour: t.colour })) }
}

export function boundsOf(m: Mesh): { min: Vec3; max: Vec3 } | null {
  if (m.triangles.length === 0) return null
  const min = vec3(Infinity, Infinity, Infinity)
  const max = vec3(-Infinity, -Infinity, -Infinity)
  for (const t of m.triangles) {
    for (const p of [t.a, t.b, t.c]) {
      min.x = Math.min(min.x, p.x)
      min.y = Math.min(min.y, p.y)
      min.z = Math.min(min.z, p.z)
      max.x = Math.max(max.x, p.x)
      max.y = Math.max(max.y, p.y)
      max.z = Math.max(max.z, p.z)
    }
  }
  return { min, max }
}

// ---------------------------------------------------------------------------
// Triangulation
// ---------------------------------------------------------------------------

/** Twice the signed area; positive when the ring winds anticlockwise. */
export function signedArea(points: Vec2[]): number {
  let sum = 0
  for (let i = 0; i < points.length; i++) {
    const a = points[i]
    const b = points[(i + 1) % points.length]
    sum += a.x * b.y - b.x * a.y
  }
  return sum / 2
}

function pointInTriangle(p: Vec2, a: Vec2, b: Vec2, c: Vec2): boolean {
  const d1 = (p.x - b.x) * (a.y - b.y) - (a.x - b.x) * (p.y - b.y)
  const d2 = (p.x - c.x) * (b.y - c.y) - (b.x - c.x) * (p.y - c.y)
  const d3 = (p.x - a.x) * (c.y - a.y) - (c.x - a.x) * (p.y - a.y)
  const neg = d1 < 0 || d2 < 0 || d3 < 0
  const pos = d1 > 0 || d2 > 0 || d3 > 0
  return !(neg && pos)
}

/**
 * Ear clipping over a simple polygon, returning index triples wound
 * anticlockwise. Self-intersecting input is not detected; it just comes back
 * as a poor tessellation rather than an error.
 */
export function triangulate(points: Vec2[]): [number, number, number][] {
  const n = points.length
  if (n < 3) return []
  const anticlockwise = signedArea(points) >= 0
  const indices: number[] = []
  for (let i = 0; i < n; i++) indices.push(anticlockwise ? i : n - 1 - i)

  const out: [number, number, number][] = []
  let guard = 0
  while (indices.length > 3 && guard++ < n * n) {
    let clipped = false
    for (let i = 0; i < indices.length; i++) {
      const ai = indices[(i + indices.length - 1) % indices.length]
      const bi = indices[i]
      const ci = indices[(i + 1) % indices.length]
      const a = points[ai]
      const b = points[bi]
      const c = points[ci]
      const convex = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x) > 0
      if (!convex) continue
      const contains = indices.some(
        (j) => j !== ai && j !== bi && j !== ci && pointInTriangle(points[j], a, b, c),
      )
      if (contains) continue
      out.push([ai, bi, ci])
      indices.splice(i, 1)
      clipped = true
      break
    }
    if (!clipped) break
  }
  if (indices.length === 3) out.push([indices[0], indices[1], indices[2]])
  return out
}

// ---------------------------------------------------------------------------
// Surfaces
// ---------------------------------------------------------------------------

/** A flat polygon, lying on the ground plane at `height`, facing up. */
export function fill(path: Path2, colour: Colour, height = 0): Mesh {
  const tris = triangulate(path.points)
  return {
    triangles: tris.map(([i, j, k]) =>
      triangle(lift(path.points[i], height), lift(path.points[j], height), lift(path.points[k], height), colour),
    ),
  }
}

/**
 * A frame along a swept path: where it is, and which way the cross-section's
 * own x and y point there.
 */
interface Frame {
  at: Vec3
  u: Vec3
  v: Vec3
}

/**
 * Frames along a path, each turned as little as possible from the one before
 * it (parallel transport), so a cross-section swept through them doesn't spin
 * about the path as it goes.
 *
 * The first frame is chosen to agree with `lift`: a path running straight up
 * puts the cross-section's x on +X and its y on -Z, which is the same placement
 * a flat shape gets.
 */
export function framesAlong(path: Path3): Frame[] {
  const pts = path.points
  if (pts.length < 2) return []
  const closed = path.closed

  const tangents: Vec3[] = pts.map((p, i) => {
    const before = i === 0 ? (closed ? sub3(p, pts[pts.length - 1]) : null) : sub3(p, pts[i - 1])
    const after =
      i === pts.length - 1 ? (closed ? sub3(pts[0], p) : null) : sub3(pts[i + 1], p)
    const sum = add3(normalise3(before ?? after!), normalise3(after ?? before!))
    const t = normalise3(sum)
    return length3(t) === 0 ? normalise3(after ?? before!) : t
  })

  const first = tangents[0]
  let u = cross3(first, vec3(0, 0, 1))
  if (length3(u) < 1e-6) u = cross3(first, vec3(0, 1, 0))
  u = normalise3(u)

  const frames: Frame[] = []
  let previous = first
  for (let i = 0; i < pts.length; i++) {
    const t = tangents[i]
    // Turn the frame by whatever turned the tangent, and nothing more.
    const axis = cross3(previous, t)
    if (length3(axis) > 1e-9) {
      const dot = Math.max(-1, Math.min(1, dot3(previous, t)))
      u = normalise3(rotateAxis(u, axis, (Math.acos(dot) * 180) / Math.PI))
    }
    previous = t
    frames.push({ at: pts[i], u, v: normalise3(cross3(t, u)) })
  }
  return frames
}

/** A cross-section placed in a frame. */
const place = (frame: Frame, p: Vec2): Vec3 =>
  add3(frame.at, add3(scale3(frame.u, p.x), scale3(frame.v, p.y)))

/**
 * A 2D outline swept along a 3D path, capped at both ends when the outline is
 * closed and the path isn't. A straight path up is the plain extrusion; any
 * other path bends it.
 */
export function extrude(outline: Path2, path: Path3, colour: Colour, caps = true): Mesh {
  const pts = outline.points
  const frames = framesAlong(path)
  if (pts.length < 2 || frames.length < 2) return { triangles: [] }
  const anticlockwise = signedArea(pts) >= 0
  const ring = anticlockwise ? pts : [...pts].reverse()

  const rings = frames.map((frame) => ring.map((p) => place(frame, p)))
  const triangles: Triangle[] = []
  const last = path.closed ? rings.length : rings.length - 1
  const edges = outline.closed ? ring.length : ring.length - 1
  for (let s = 0; s < last; s++) {
    const here = rings[s]
    const next = rings[(s + 1) % rings.length]
    for (let i = 0; i < edges; i++) {
      const j = (i + 1) % ring.length
      triangles.push(...quad(here[i], here[j], next[j], next[i], colour))
    }
  }

  if (caps && outline.closed && !path.closed && ring.length >= 3) {
    const cut = triangulate(ring)
    const start = rings[0]
    const end = rings[rings.length - 1]
    for (const [i, j, k] of cut) {
      triangles.push(triangle(end[i], end[j], end[k], colour))
      triangles.push(triangle(start[i], start[k], start[j], colour))
    }
  }
  return { triangles }
}

/** A straight path, which is what an extrusion wants most of the time. */
export const line3 = (from: Vec3, to: Vec3): Path3 => ({ points: [from, to], closed: false })

/**
 * A profile spun about the Y axis. The profile's x is a radius and its y a
 * height, so a half-circle becomes a sphere and a rectangle a cylinder.
 */
export function revolve(profile: Path2, segments: number, colour: Colour, degrees = 360): Mesh {
  const pts = profile.points
  const steps = Math.max(3, Math.round(segments))
  if (pts.length < 2) return { triangles: [] }
  const full = Math.abs(degrees % 360) < 1e-9
  const rings: Vec3[][] = []
  const divisions = full ? steps : steps
  for (let s = 0; s <= divisions; s++) {
    if (full && s === divisions) break
    const angle = (degrees * s) / divisions
    const r = (angle * Math.PI) / 180
    rings.push(pts.map((p) => vec3(p.x * Math.cos(r), p.y, -p.x * Math.sin(r))))
  }
  const triangles: Triangle[] = []
  for (let s = 0; s < rings.length; s++) {
    const next = rings[(s + 1) % rings.length]
    if (!full && s === rings.length - 1) break
    const here = rings[s]
    for (let i = 0; i < pts.length - 1; i++) {
      triangles.push(...quad(here[i], next[i], next[i + 1], here[i + 1], colour))
    }
  }
  return { triangles }
}

/** A skin between two paths with the same number of points. */
export function loft(a: Path3, b: Path3, colour: Colour): Mesh {
  const n = Math.min(a.points.length, b.points.length)
  if (n < 2) return { triangles: [] }
  const closed = a.closed && b.closed
  const triangles: Triangle[] = []
  const last = closed ? n : n - 1
  for (let i = 0; i < last; i++) {
    const j = (i + 1) % n
    triangles.push(...quad(a.points[i], b.points[i], b.points[j], a.points[j], colour))
  }
  return { triangles }
}

// ---------------------------------------------------------------------------
// Primitive solids
// ---------------------------------------------------------------------------

export function box(size: Vec3, colour: Colour): Mesh {
  const h = scale3(size, 0.5)
  const p = (sx: number, sy: number, sz: number): Vec3 => vec3(h.x * sx, h.y * sy, h.z * sz)
  const triangles: Triangle[] = []
  // +Y, -Y, +X, -X, +Z, -Z
  triangles.push(...quad(p(-1, 1, 1), p(1, 1, 1), p(1, 1, -1), p(-1, 1, -1), colour))
  triangles.push(...quad(p(-1, -1, -1), p(1, -1, -1), p(1, -1, 1), p(-1, -1, 1), colour))
  triangles.push(...quad(p(1, -1, 1), p(1, -1, -1), p(1, 1, -1), p(1, 1, 1), colour))
  triangles.push(...quad(p(-1, -1, -1), p(-1, -1, 1), p(-1, 1, 1), p(-1, 1, -1), colour))
  triangles.push(...quad(p(-1, -1, 1), p(1, -1, 1), p(1, 1, 1), p(-1, 1, 1), colour))
  triangles.push(...quad(p(1, -1, -1), p(-1, -1, -1), p(-1, 1, -1), p(1, 1, -1), colour))
  return { triangles }
}

export function sphere(radius: number, segments: number, colour: Colour): Mesh {
  const s = Math.max(3, Math.round(segments))
  const rows = Math.max(2, Math.round(s / 2))
  const profile: Vec2[] = []
  for (let i = 0; i <= rows; i++) {
    const a = Math.PI * (i / rows) - Math.PI / 2
    profile.push(vec2(Math.cos(a) * radius, Math.sin(a) * radius))
  }
  return revolve({ points: profile, closed: false }, s, colour)
}

export function cylinder(radius: number, height: number, segments: number, colour: Colour): Mesh {
  const profile: Vec2[] = [
    vec2(0, 0),
    vec2(radius, 0),
    vec2(radius, height),
    vec2(0, height),
  ]
  return revolve({ points: profile, closed: false }, segments, colour)
}

/** A regular polygon, first point on +x, wound anticlockwise. */
export function regularPolygon(sides: number, radius: number): Path2 {
  const n = Math.max(3, Math.round(sides))
  const points: Vec2[] = []
  for (let i = 0; i < n; i++) {
    const a = (2 * Math.PI * i) / n
    points.push(vec2(Math.cos(a) * radius, Math.sin(a) * radius))
  }
  return { points, closed: true }
}
