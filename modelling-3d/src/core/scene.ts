/**
 * What the preview draws: a value of any type reduced to triangles, polylines
 * and marked points, all in 3D.
 *
 * Keeping this out of the viewer means the same reduction is used by the
 * exporter and can be asserted headlessly — and it is the one place that
 * decides how a 2D value is shown, which is flat on the ground plane.
 */

import { lift } from './geometry'
import type { Colour, Mesh, Path2, Path3, Triangle, ValueType, Vec2, Vec3 } from './values'
import { colour } from './values'

export interface Polyline {
  points: Vec3[]
  closed: boolean
  colour: Colour
}

/**
 * Where a marker's value is written down, when it is somewhere that can be
 * written back to — which is what makes it draggable in the viewer.
 */
export interface MarkerHandle {
  node: string
  key: string
  /** The value is a 2D point, so it goes back as one and stays on the ground. */
  flat: boolean
}

export interface Marker {
  position: Vec3
  colour: Colour
  handle?: MarkerHandle
}

export interface Scene {
  triangles: Triangle[]
  lines: Polyline[]
  markers: Marker[]
}

/** The tone anything that isn't a mesh is drawn in: the app's accent. */
export const OUTLINE = colour(0.38, 0.4, 0.85)

export const emptyScene = (): Scene => ({ triangles: [], lines: [], markers: [] })

export function addValue(
  scene: Scene,
  type: ValueType,
  value: unknown,
  handle?: MarkerHandle,
): void {
  switch (type) {
    case 'mesh':
      scene.triangles.push(...(value as Mesh).triangles)
      return
    case 'path2': {
      const p = value as Path2
      scene.lines.push({ points: p.points.map((q) => lift(q)), closed: p.closed, colour: OUTLINE })
      return
    }
    case 'path3': {
      const p = value as Path3
      scene.lines.push({ points: p.points, closed: p.closed, colour: OUTLINE })
      return
    }
    case 'vec2':
      scene.markers.push({ position: lift(value as Vec2), colour: OUTLINE, handle })
      return
    case 'vec3':
      scene.markers.push({ position: value as Vec3, colour: OUTLINE, handle })
      return
    default:
      // Numbers, text and colours have nothing to show in three dimensions.
      return
  }
}

export function sceneOf(
  values: { type: ValueType; value: unknown; handle?: MarkerHandle }[],
): Scene {
  const scene = emptyScene()
  for (const { type, value, handle } of values) addValue(scene, type, value, handle)
  return scene
}

/** The mesh part of a scene, which is all an exported file can carry. */
export const meshOf = (scene: Scene): Mesh => ({ triangles: scene.triangles })
