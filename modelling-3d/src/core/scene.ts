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

export interface Marker {
  position: Vec3
  colour: Colour
}

export interface Scene {
  triangles: Triangle[]
  lines: Polyline[]
  markers: Marker[]
}

/** The tone anything that isn't a mesh is drawn in: the app's accent. */
export const OUTLINE = colour(0.38, 0.4, 0.85)

export const emptyScene = (): Scene => ({ triangles: [], lines: [], markers: [] })

export function addValue(scene: Scene, type: ValueType, value: unknown): void {
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
      scene.markers.push({ position: lift(value as Vec2), colour: OUTLINE })
      return
    case 'vec3':
      scene.markers.push({ position: value as Vec3, colour: OUTLINE })
      return
    default:
      // Numbers, text and colours have nothing to show in three dimensions.
      return
  }
}

export function sceneOf(values: { type: ValueType; value: unknown }[]): Scene {
  const scene = emptyScene()
  for (const { type, value } of values) addValue(scene, type, value)
  return scene
}

/** The mesh part of a scene, which is all an exported file can carry. */
export const meshOf = (scene: Scene): Mesh => ({ triangles: scene.triangles })
