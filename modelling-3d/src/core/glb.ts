/**
 * Writing a mesh out as binary glTF (`.glb`).
 *
 * glTF 2.0 is the format the app settled on: a triangle soup with a colour per
 * vertex is the whole of what a model here is, and every open-source renderer
 * worth calling out to — Blender, f3d, Godot, three.js, the browser — reads it
 * without a plugin. Lighting rides along in `KHR_lights_punctual`, so a viewer
 * that honours it shows what the preview showed, and one that doesn't falls
 * back to its own environment rather than to nothing.
 *
 * The file is written by hand because that is a few dozen lines and no
 * dependency, and because the exporter can then be tested with no DOM.
 */

import { cross3, normalise3, sub3 } from './geometry'
import type { Mesh, Vec3 } from './values'
import { toLinear } from './values'

const MAGIC = 0x46546c67
const JSON_CHUNK = 0x4e4f534a
const BIN_CHUNK = 0x004e4942

/** The rotation taking -Z (a glTF light's own direction) onto `direction`. */
function aimQuaternion(direction: Vec3): [number, number, number, number] {
  const from: Vec3 = { x: 0, y: 0, z: -1 }
  const to = normalise3(direction)
  const dot = from.x * to.x + from.y * to.y + from.z * to.z
  if (dot > 0.999999) return [0, 0, 0, 1]
  if (dot < -0.999999) return [0, 1, 0, 0]
  const axis = cross3(from, to)
  const w = 1 + dot
  const length = Math.sqrt(axis.x * axis.x + axis.y * axis.y + axis.z * axis.z + w * w)
  return [axis.x / length, axis.y / length, axis.z / length, w / length]
}

const pad4 = (n: number): number => (n + 3) & ~3

export interface GlbOptions {
  /** Written into `asset.generator`. */
  generator?: string
  /** Where the key light shines, in world space. */
  keyLight?: Vec3
}

/**
 * Flat-shaded triangles, one vertex per corner (no sharing), which is what
 * gives every triangle its own colour without a material each.
 */
export function exportGlb(mesh: Mesh, options: GlbOptions = {}): Uint8Array {
  const count = mesh.triangles.length * 3
  const positions = new Float32Array(count * 3)
  const normals = new Float32Array(count * 3)
  const colours = new Float32Array(count * 4)

  const min: Vec3 = { x: Infinity, y: Infinity, z: Infinity }
  const max: Vec3 = { x: -Infinity, y: -Infinity, z: -Infinity }

  mesh.triangles.forEach((t, i) => {
    const normal = normalise3(cross3(sub3(t.b, t.a), sub3(t.c, t.a)))
    ;[t.a, t.b, t.c].forEach((p, k) => {
      const v = i * 3 + k
      positions[v * 3] = p.x
      positions[v * 3 + 1] = p.y
      positions[v * 3 + 2] = p.z
      normals[v * 3] = normal.x
      normals[v * 3 + 1] = normal.y
      normals[v * 3 + 2] = normal.z
      colours[v * 4] = toLinear(t.colour.r)
      colours[v * 4 + 1] = toLinear(t.colour.g)
      colours[v * 4 + 2] = toLinear(t.colour.b)
      colours[v * 4 + 3] = 1
      min.x = Math.min(min.x, p.x)
      min.y = Math.min(min.y, p.y)
      min.z = Math.min(min.z, p.z)
      max.x = Math.max(max.x, p.x)
      max.y = Math.max(max.y, p.y)
      max.z = Math.max(max.z, p.z)
    })
  })

  const parts = [positions, normals, colours]
  const bufferViews: unknown[] = []
  let offset = 0
  for (const part of parts) {
    bufferViews.push({ buffer: 0, byteOffset: offset, byteLength: part.byteLength, target: 34962 })
    offset += pad4(part.byteLength)
  }
  const binLength = offset

  const bounds = count > 0
    ? { min: [min.x, min.y, min.z], max: [max.x, max.y, max.z] }
    : { min: [0, 0, 0], max: [0, 0, 0] }

  const key = options.keyLight ?? { x: -0.4, y: -1, z: -0.55 }
  const fill: Vec3 = { x: -key.x, y: Math.min(-0.2, key.y * 0.35), z: -key.z }

  const json = {
    asset: { version: '2.0', generator: options.generator ?? 'modelling-3d' },
    extensionsUsed: ['KHR_lights_punctual'],
    extensions: {
      KHR_lights_punctual: {
        lights: [
          { type: 'directional', color: [1, 1, 1], intensity: 3.2, name: 'key' },
          { type: 'directional', color: [0.82, 0.85, 1], intensity: 1.1, name: 'fill' },
        ],
      },
    },
    scene: 0,
    scenes: [{ nodes: [0, 1, 2] }],
    nodes: [
      { mesh: 0, name: 'model' },
      { name: 'key light', rotation: aimQuaternion(key), extensions: { KHR_lights_punctual: { light: 0 } } },
      { name: 'fill light', rotation: aimQuaternion(fill), extensions: { KHR_lights_punctual: { light: 1 } } },
    ],
    meshes: [
      {
        name: 'model',
        primitives: [
          { attributes: { POSITION: 0, NORMAL: 1, COLOR_0: 2 }, material: 0, mode: 4 },
        ],
      },
    ],
    materials: [
      {
        name: 'vertex colours',
        doubleSided: true,
        pbrMetallicRoughness: {
          baseColorFactor: [1, 1, 1, 1],
          metallicFactor: 0,
          roughnessFactor: 0.65,
        },
      },
    ],
    buffers: [{ byteLength: binLength }],
    bufferViews,
    accessors: [
      { bufferView: 0, componentType: 5126, count, type: 'VEC3', ...bounds },
      { bufferView: 1, componentType: 5126, count, type: 'VEC3' },
      { bufferView: 2, componentType: 5126, count, type: 'VEC4' },
    ],
  }

  const jsonBytes = new TextEncoder().encode(JSON.stringify(json))
  const jsonLength = pad4(jsonBytes.byteLength)
  const total = 12 + 8 + jsonLength + 8 + binLength

  const out = new Uint8Array(total)
  const view = new DataView(out.buffer)
  view.setUint32(0, MAGIC, true)
  view.setUint32(4, 2, true)
  view.setUint32(8, total, true)

  view.setUint32(12, jsonLength, true)
  view.setUint32(16, JSON_CHUNK, true)
  out.set(jsonBytes, 20)
  out.fill(0x20, 20 + jsonBytes.byteLength, 20 + jsonLength)

  const binStart = 20 + jsonLength
  view.setUint32(binStart, binLength, true)
  view.setUint32(binStart + 4, BIN_CHUNK, true)
  let at = binStart + 8
  for (const part of parts) {
    out.set(new Uint8Array(part.buffer, part.byteOffset, part.byteLength), at)
    at += pad4(part.byteLength)
  }

  return out
}
