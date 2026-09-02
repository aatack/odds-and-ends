/**
 * The right pane: whatever is selected, in three dimensions.
 *
 * three.js draws it, and the lighting here is the same lighting the exporter
 * writes into the file, so what you see is what a viewer shows. The scene it
 * draws is derived in `core/scene.ts` rather than here, so the preview and the
 * export are looking at the same thing.
 *
 * Nothing is animated: a frame is drawn when the camera moves or the model
 * changes, and never otherwise.
 */

import { forwardRef, useEffect, useImperativeHandle, useRef } from 'react'
import * as THREE from 'three'
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js'
import type { MarkerHandle, Scene as ModelScene } from '@core/scene'
import { flatten } from '@core/geometry'
import type { Vec2, Vec3 } from '@core/values'
import { toLinear } from '@core/values'

export interface PreviewHandle {
  frame(): void
}

interface Viewer {
  renderer: THREE.WebGLRenderer
  scene: THREE.Scene
  camera: THREE.PerspectiveCamera
  controls: OrbitControls
  content: THREE.Group
  /** The draggable points, kept apart so only they are picked. */
  handles: THREE.Group
  draw: () => void
}

/** Where the key light shines, matching what the exporter writes. */
const KEY_LIGHT = new THREE.Vector3(4, 10, 5.5)

function makeViewer(host: HTMLDivElement): Viewer {
  const renderer = new THREE.WebGLRenderer({ antialias: true })
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2))
  renderer.setClearColor(0xfafafb)
  host.appendChild(renderer.domElement)
  renderer.domElement.style.display = 'block'

  const scene = new THREE.Scene()
  const camera = new THREE.PerspectiveCamera(42, 1, 0.01, 2000)
  camera.position.set(4.5, 3.2, 5.5)

  const controls = new OrbitControls(camera, renderer.domElement)
  controls.enableDamping = false
  // The vertical axis stays vertical, so you can never end up upside down.
  camera.up.set(0, 1, 0)
  controls.screenSpacePanning = false
  controls.minDistance = 0.05
  controls.maxDistance = 500

  scene.add(new THREE.HemisphereLight(0xffffff, 0x9a9da6, 1.5))
  const key = new THREE.DirectionalLight(0xffffff, 2.4)
  key.position.copy(KEY_LIGHT)
  scene.add(key)
  const fill = new THREE.DirectionalLight(0xd2d8ff, 0.8)
  fill.position.set(-4, 3, -5.5)
  scene.add(fill)

  const grid = new THREE.GridHelper(20, 20, 0xc9cad2, 0xe4e5ea)
  ;(grid.material as THREE.Material).transparent = true
  ;(grid.material as THREE.Material).opacity = 0.9
  scene.add(grid)

  const content = new THREE.Group()
  scene.add(content)
  const handles = new THREE.Group()
  scene.add(handles)

  /**
   * Handles are scaled by how far away they are, so a point stays the same
   * size on screen whether the model is a doorknob or a cathedral — and stays
   * big enough to grab at any zoom.
   */
  const draw = (): void => {
    for (const child of handles.children) {
      if (!(child as THREE.Mesh).isMesh) continue
      const distance = camera.position.distanceTo(child.position)
      child.scale.setScalar(Math.max(distance * 0.011, 1e-4))
    }
    renderer.render(scene, camera)
  }
  controls.addEventListener('change', draw)
  return { renderer, scene, camera, controls, content, handles, draw }
}

/** Throw away the geometry of the last frame before building the next. */
function clear(group: THREE.Group): void {
  for (const child of [...group.children]) {
    group.remove(child)
    const holder = child as THREE.Mesh
    holder.geometry?.dispose()
    const material = holder.material as THREE.Material | THREE.Material[] | undefined
    if (Array.isArray(material)) material.forEach((m) => m.dispose())
    else material?.dispose()
  }
}

function build(group: THREE.Group, model: ModelScene): void {
  clear(group)

  if (model.triangles.length > 0) {
    const positions = new Float32Array(model.triangles.length * 9)
    const colours = new Float32Array(model.triangles.length * 9)
    model.triangles.forEach((triangle, i) => {
      ;[triangle.a, triangle.b, triangle.c].forEach((point, k) => {
        const at = i * 9 + k * 3
        positions[at] = point.x
        positions[at + 1] = point.y
        positions[at + 2] = point.z
        colours[at] = toLinear(triangle.colour.r)
        colours[at + 1] = toLinear(triangle.colour.g)
        colours[at + 2] = toLinear(triangle.colour.b)
      })
    })
    const geometry = new THREE.BufferGeometry()
    geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3))
    geometry.setAttribute('color', new THREE.BufferAttribute(colours, 3))
    geometry.computeVertexNormals()
    group.add(
      new THREE.Mesh(
        geometry,
        new THREE.MeshStandardMaterial({
          vertexColors: true,
          flatShading: true,
          roughness: 0.65,
          metalness: 0,
          side: THREE.DoubleSide,
        }),
      ),
    )
  }

  if (model.lines.length > 0) {
    const points: number[] = []
    for (const line of model.lines) {
      const last = line.closed ? line.points.length : line.points.length - 1
      for (let i = 0; i < last; i++) {
        const a = line.points[i]
        const b = line.points[(i + 1) % line.points.length]
        points.push(a.x, a.y, a.z, b.x, b.y, b.z)
      }
    }
    const geometry = new THREE.BufferGeometry()
    geometry.setAttribute('position', new THREE.Float32BufferAttribute(points, 3))
    group.add(
      new THREE.LineSegments(geometry, new THREE.LineBasicMaterial({ color: 0x4c53c4 })),
    )
  }

}

/**
 * The points, as little spheres rather than dots, because a dot cannot be
 * picked up. One that stands for a literal on a node gets a line back to the
 * origin as well, so it reads as the vector it is.
 */
function buildHandles(group: THREE.Group, model: ModelScene): void {
  clear(group)
  // Unit spheres: `draw` scales them to a constant size on screen.
  const geometry = new THREE.SphereGeometry(1, 16, 12)
  for (const marker of model.markers) {
    const material = new THREE.MeshBasicMaterial({
      color: marker.handle ? 0x4c53c4 : 0x8b93e6,
      depthTest: false,
    })
    const ball = new THREE.Mesh(geometry.clone(), material)
    ball.position.set(marker.position.x, marker.position.y, marker.position.z)
    ball.renderOrder = 2
    ball.userData.handle = marker.handle
    group.add(ball)

    if (marker.handle) {
      const line = new THREE.BufferGeometry().setFromPoints([
        new THREE.Vector3(0, 0, 0),
        new THREE.Vector3(marker.position.x, marker.position.y, marker.position.z),
      ])
      const arm = new THREE.Line(
        line,
        new THREE.LineBasicMaterial({ color: 0x8b93e6, depthTest: false }),
      )
      arm.renderOrder = 1
      group.add(arm)
    }
  }
}

/** Put the camera where all of it is in shot, keeping the direction it faces. */
function frameContent(viewer: Viewer): void {
  const box = new THREE.Box3().setFromObject(viewer.content)
  const sphere = box.isEmpty()
    ? new THREE.Sphere(new THREE.Vector3(0, 0, 0), 2.5)
    : box.getBoundingSphere(new THREE.Sphere())
  const radius = Math.max(sphere.radius, 0.4)
  // The pane is usually taller than it is wide, and the field of view given is
  // the vertical one, so the horizontal is the tighter of the two.
  const vertical = (viewer.camera.fov * Math.PI) / 180
  const horizontal = 2 * Math.atan(Math.tan(vertical / 2) * viewer.camera.aspect)
  const fov = Math.min(vertical, horizontal)
  const distance = (radius / Math.sin(fov / 2)) * 1.15

  const direction = viewer.camera.position
    .clone()
    .sub(viewer.controls.target)
    .normalize()
  if (direction.lengthSq() === 0) direction.set(0.7, 0.5, 0.85).normalize()

  viewer.controls.target.copy(sphere.center)
  viewer.camera.position.copy(sphere.center).addScaledVector(direction, distance)
  viewer.camera.near = Math.max(distance / 1000, 0.001)
  viewer.camera.far = distance * 20
  viewer.camera.updateProjectionMatrix()
  viewer.controls.update()
  viewer.draw()
}

/** How far a drag on the ground plane is from the pointer's ray. */
function pointOnPlane(
  camera: THREE.PerspectiveCamera,
  ndc: THREE.Vector2,
  plane: THREE.Plane,
): THREE.Vector3 | null {
  const ray = new THREE.Raycaster()
  ray.setFromCamera(ndc, camera)
  const hit = new THREE.Vector3()
  return ray.ray.intersectPlane(plane, hit) ? hit : null
}

export const Preview = forwardRef<
  PreviewHandle,
  {
    scene: ModelScene
    frameOn: string | null
    onMoveHandle?: (handle: MarkerHandle, to: Vec3 | Vec2) => void
  }
>(function Preview({ scene, frameOn, onMoveHandle }, ref) {
    const host = useRef<HTMLDivElement>(null)
    const viewer = useRef<Viewer | null>(null)
    // The drag reads this rather than closing over the prop, so it is never a
    // render behind.
    const move = useRef(onMoveHandle)
    move.current = onMoveHandle

    useEffect(() => {
      const element = host.current
      if (!element) return
      const made = makeViewer(element)
      viewer.current = made

      const resize = (): void => {
        const { clientWidth, clientHeight } = element
        if (clientWidth === 0 || clientHeight === 0) return
        made.renderer.setSize(clientWidth, clientHeight, false)
        made.camera.aspect = clientWidth / clientHeight
        made.camera.updateProjectionMatrix()
        made.draw()
      }
      const observer = new ResizeObserver(resize)
      observer.observe(element)
      resize()

      /**
       * Dragging a point. The default plane is the ground, so a point slides
       * about the model rather than towards the camera; holding shift swaps to
       * the vertical plane facing you, which is the only other one worth
       * having. It runs before OrbitControls and takes the event off it.
       */
      const onPointerDown = (event: PointerEvent): void => {
        if (event.button !== 0) return
        const box = made.renderer.domElement.getBoundingClientRect()
        const ndc = new THREE.Vector2(
          ((event.clientX - box.left) / box.width) * 2 - 1,
          -((event.clientY - box.top) / box.height) * 2 + 1,
        )
        const ray = new THREE.Raycaster()
        ray.setFromCamera(ndc, made.camera)
        const hit = ray.intersectObjects(made.handles.children, false)[0]
        const handle = hit?.object.userData.handle as MarkerHandle | undefined
        if (!handle) return

        event.preventDefault()
        event.stopPropagation()
        made.controls.enabled = false
        const start = hit.object.position.clone()

        const planeFor = (vertical: boolean): THREE.Plane => {
          if (!vertical || handle.flat) return new THREE.Plane(new THREE.Vector3(0, 1, 0), -start.y)
          const facing = made.camera.getWorldDirection(new THREE.Vector3())
          facing.y = 0
          if (facing.lengthSq() === 0) facing.set(0, 0, 1)
          facing.normalize()
          return new THREE.Plane(facing, -facing.dot(start))
        }

        const drag = (moved: PointerEvent): void => {
          const at = new THREE.Vector2(
            ((moved.clientX - box.left) / box.width) * 2 - 1,
            -((moved.clientY - box.top) / box.height) * 2 + 1,
          )
          const point = pointOnPlane(made.camera, at, planeFor(moved.shiftKey))
          if (!point) return
          hit.object.position.copy(point)
          made.draw()
          const to = { x: point.x, y: point.y, z: point.z }
          move.current?.(handle, handle.flat ? flatten(to) : to)
        }
        const stop = (): void => {
          made.controls.enabled = true
          window.removeEventListener('pointermove', drag)
          window.removeEventListener('pointerup', stop)
        }
        window.addEventListener('pointermove', drag)
        window.addEventListener('pointerup', stop)
      }
      made.renderer.domElement.addEventListener('pointerdown', onPointerDown, true)

      return () => {
        made.renderer.domElement.removeEventListener('pointerdown', onPointerDown, true)
        observer.disconnect()
        made.controls.dispose()
        clear(made.content)
        clear(made.handles)
        made.renderer.dispose()
        element.removeChild(made.renderer.domElement)
        viewer.current = null
      }
    }, [])

    useEffect(() => {
      const made = viewer.current
      if (!made) return
      build(made.content, scene)
      buildHandles(made.handles, scene)
      made.draw()
    }, [scene])

    // A new model gets a fresh look at itself; edits to one do not move the camera.
    useEffect(() => {
      const made = viewer.current
      if (made) frameContent(made)
    }, [frameOn])

    useImperativeHandle(ref, () => ({
      frame: () => {
        if (viewer.current) frameContent(viewer.current)
      },
    }))

  return <div ref={host} className="h-full w-full" />
})
