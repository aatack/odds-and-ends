import React, { useEffect, useId, useMemo, useRef, useState } from 'react'
import { cn } from '../helpers/cn'
import type { EditorActions } from '../views/useEditor'
import { EntityFrame } from './EntityFrame'
import type { ViewHandle } from './useLayout'
import { useCanvas } from './useCanvas'
import { CANVAS_DEFAULT_WIDTH, entityView, type CanvasNode, type CanvasView, type Frame } from './types'

// A node's assumed height for edge geometry until it has been measured. Nodes
// always grow to fit their content — height is never fixed.
const DEFAULT_HEIGHT = 260
const MIN_WIDTH = 200
// Extra room around the edge SVG so arrowheads near the bbox aren't clipped.
const EDGE_PAD = 60

interface Rect {
  x: number
  y: number
  w: number
  h: number
}

// The point where the segment from a rect's centre toward (tx,ty) crosses the
// rect border — i.e. the side of the node facing the other node.
function borderPoint(r: Rect, tx: number, ty: number): [number, number] {
  const cx = r.x + r.w / 2
  const cy = r.y + r.h / 2
  const dx = tx - cx
  const dy = ty - cy
  if (dx === 0 && dy === 0) return [cx, cy]
  const sx = dx !== 0 ? r.w / 2 / Math.abs(dx) : Infinity
  const sy = dy !== 0 ? r.h / 2 / Math.abs(dy) : Infinity
  const s = Math.min(sx, sy)
  return [cx + dx * s, cy + dy * s]
}

interface Drag {
  id: string
  kind: 'move' | 'width'
  startX: number
  startY: number
  orig: CanvasNode
  /** Move only after passing a small distance, so a click still selects a row. */
  threshold?: boolean
}

// How far the pointer must travel before a body press becomes a node drag.
const DRAG_THRESHOLD = 4

export interface CanvasProps {
  frame: Frame
  actions: EditorActions
  onDebugEntity: (entityId: string) => void
  /** Persist a change to this frame's view (node moved / resized / added). */
  updateView: (frameId: string, view: CanvasView) => void
  /** Persist the board pan/zoom without touching nodes. */
  updateCanvasCam: (frameId: string, cam: { x: number; y: number; zoom: number }) => void
  /** Publish a handle up to the layout, proxying to the active panel. */
  onHandle?: (handle: ViewHandle | null) => void
}

/**
 * A spatial board: each entity in `view.nodes` renders as a draggable,
 * resizable {@link EntityFrame} positioned at its stored coordinates, with the
 * other board entities folded (collapsed) inside it. Directional edges connect
 * the facing sides of nodes whose subtrees reference one another. The board pans
 * (drag background / wheel) and zooms (Ctrl/⌘+wheel about the cursor).
 */
export function Canvas({
  frame,
  actions,
  onDebugEntity,
  updateView,
  updateCanvasCam,
  onHandle,
}: CanvasProps): React.JSX.Element {
  const view = frame.view as CanvasView
  const edges = useCanvas(view.nodes, actions.resolveQuery)
  const ids = Object.keys(view.nodes)
  // Unique marker id per board (multiple canvases can be mounted at once).
  const arrowId = `arrow-${useId().replace(/:/g, '')}`

  // The panel keyboard input (w/s, edit, link…) is routed to — set by clicking a
  // panel; defaults to the first node.
  const [activeNodeId, setActiveNodeId] = useState<string | null>(null)
  const activeId = activeNodeId && view.nodes[activeNodeId] ? activeNodeId : (ids[0] ?? null)
  const activeRef = useRef(activeId)
  activeRef.current = activeId

  // Each panel publishes its handle here; the board exposes one proxy handle to
  // the layout that forwards to whichever panel is active.
  const panelHandles = useRef<Map<string, ViewHandle>>(new Map())
  const panelCbs = useRef<Map<string, (h: ViewHandle | null) => void>>(new Map())
  const panelOnHandle = (id: string): ((h: ViewHandle | null) => void) => {
    let cb = panelCbs.current.get(id)
    if (!cb) {
      cb = (h) => {
        if (h) panelHandles.current.set(id, h)
        else panelHandles.current.delete(id)
      }
      panelCbs.current.set(id, cb)
    }
    return cb
  }

  const proxy = useMemo<ViewHandle>(
    () => ({
      getSelectedEntityId: () => panelHandles.current.get(activeRef.current ?? '')?.getSelectedEntityId() ?? null,
      getSelectedText: () => panelHandles.current.get(activeRef.current ?? '')?.getSelectedText?.() ?? null,
      getActiveNodeId: () => activeRef.current,
      runAction: (id) => panelHandles.current.get(activeRef.current ?? '')?.runAction?.(id),
    }),
    [],
  )
  useEffect(() => {
    if (!onHandle) return
    onHandle(proxy)
    return () => onHandle(null)
  }, [onHandle, proxy])

  // Measured panel heights (unaffected by zoom, since offsetHeight is layout
  // size) so edges can anchor to the real node rectangles.
  const [heights, setHeights] = useState<Record<string, number>>({})
  const observers = useRef<Map<string, ResizeObserver>>(new Map())
  const elCbs = useRef<Map<string, (el: HTMLDivElement | null) => void>>(new Map())
  const panelElCb = (id: string): ((el: HTMLDivElement | null) => void) => {
    let cb = elCbs.current.get(id)
    if (!cb) {
      cb = (el) => {
        observers.current.get(id)?.disconnect()
        observers.current.delete(id)
        if (el) {
          const report = (): void =>
            setHeights((h) => (h[id] === el.offsetHeight ? h : { ...h, [id]: el.offsetHeight }))
          const ro = new ResizeObserver(report)
          ro.observe(el)
          observers.current.set(id, ro)
          report()
        }
      }
      elCbs.current.set(id, cb)
    }
    return cb
  }
  useEffect(
    () => () => {
      observers.current.forEach((o) => o.disconnect())
      observers.current.clear()
    },
    [],
  )

  // Node drag/resize — mirrored locally so pointer-moves don't hit localStorage
  // on every pixel; committed on release.
  const [drag, setDrag] = useState<Drag | null>(null)
  const [draft, setDraft] = useState<{ id: string; node: CanvasNode } | null>(null)
  const draftRef = useRef<{ id: string; node: CanvasNode } | null>(null)
  // Whether the current press has moved past the threshold, and whether the
  // click that follows a real drag should be swallowed (so it doesn't select a
  // row / toggle a chevron under the pointer).
  const movedRef = useRef(false)
  const suppressClickRef = useRef(false)
  const nodeOf = (id: string): CanvasNode => (draft?.id === id ? draft.node : view.nodes[id])

  // Camera: pan + zoom in one atomic state so zoom-about-cursor stays correct
  // even across rapid wheel events (pan depends on the previous zoom). Seeded
  // from the persisted view so it survives the canvas being re-/de-rendered.
  const boardRef = useRef<HTMLDivElement>(null)
  const [cam, setCam] = useState(() => ({
    x: view.pan?.x ?? 0,
    y: view.pan?.y ?? 0,
    zoom: view.zoom ?? 1,
  }))
  const camRef = useRef(cam)
  camRef.current = cam
  const [panDrag, setPanDrag] = useState<{ startX: number; startY: number; orig: { x: number; y: number } } | null>(null)

  // Persist the camera: debounced while interacting, and once more on unmount so
  // switching away mid-gesture doesn't lose the position.
  useEffect(() => {
    const t = window.setTimeout(() => updateCanvasCam(frame.id, cam), 300)
    return () => window.clearTimeout(t)
  }, [cam, frame.id, updateCanvasCam])
  useEffect(
    () => () => updateCanvasCam(frame.id, camRef.current),
    [frame.id, updateCanvasCam],
  )

  useEffect(() => {
    if (!drag) return
    const onMove = (e: PointerEvent): void => {
      const rawX = e.clientX - drag.startX
      const rawY = e.clientY - drag.startY
      // A body press only becomes a drag once it clears the threshold — until
      // then it might still be a click.
      if (drag.threshold && !movedRef.current && Math.hypot(rawX, rawY) < DRAG_THRESHOLD) return
      if (!movedRef.current) {
        movedRef.current = true
        window.getSelection()?.removeAllRanges()
      }
      // Screen deltas → board deltas: undo the zoom scale. Coordinates may be
      // negative — nodes can live left of / above the origin.
      const z = camRef.current.zoom
      const dx = rawX / z
      const dy = rawY / z
      const node: CanvasNode = { ...drag.orig }
      if (drag.kind === 'move') {
        node.x = drag.orig.x + dx
        node.y = drag.orig.y + dy
      }
      if (drag.kind === 'width') {
        node.width = Math.max(MIN_WIDTH, (drag.orig.width ?? CANVAS_DEFAULT_WIDTH) + dx)
      }
      draftRef.current = { id: drag.id, node }
      setDraft(draftRef.current)
    }
    const onUp = (): void => {
      const d = draftRef.current
      if (d) updateView(frame.id, { ...view, nodes: { ...view.nodes, [d.id]: d.node } })
      // A real body drag should not also fire a click on the row underneath.
      if (movedRef.current && drag.threshold) suppressClickRef.current = true
      draftRef.current = null
      setDraft(null)
      setDrag(null)
    }
    window.addEventListener('pointermove', onMove)
    window.addEventListener('pointerup', onUp)
    return () => {
      window.removeEventListener('pointermove', onMove)
      window.removeEventListener('pointerup', onUp)
    }
  }, [drag, frame.id, updateView, view])

  const startDrag = (
    id: string,
    kind: Drag['kind'],
    e: React.PointerEvent,
    threshold = false,
  ): void => {
    // Resize handles react immediately (and suppress text selection); a body
    // press waits for the threshold so plain clicks still reach the rows.
    if (!threshold) {
      e.preventDefault()
      e.stopPropagation()
    }
    setActiveNodeId(id)
    movedRef.current = false
    setDrag({ id, kind, startX: e.clientX, startY: e.clientY, orig: view.nodes[id], threshold })
  }

  // Pan by dragging empty board background.
  useEffect(() => {
    if (!panDrag) return
    const onMove = (e: PointerEvent): void =>
      setCam((c) => ({ ...c, x: panDrag.orig.x + (e.clientX - panDrag.startX), y: panDrag.orig.y + (e.clientY - panDrag.startY) }))
    const onUp = (): void => setPanDrag(null)
    window.addEventListener('pointermove', onMove)
    window.addEventListener('pointerup', onUp)
    return () => {
      window.removeEventListener('pointermove', onMove)
      window.removeEventListener('pointerup', onUp)
    }
  }, [panDrag])

  const onBoardPointerDown = (e: React.PointerEvent): void => {
    if ((e.target as HTMLElement).closest('[data-panel]')) return
    setPanDrag({ startX: e.clientX, startY: e.clientY, orig: { x: camRef.current.x, y: camRef.current.y } })
  }

  // Ctrl/⌘+wheel zooms about the cursor; a plain wheel over the background pans.
  // Native + non-passive so preventDefault is allowed.
  useEffect(() => {
    const el = boardRef.current
    if (!el) return
    const onWheel = (e: WheelEvent): void => {
      if (e.ctrlKey || e.metaKey) {
        e.preventDefault()
        const rect = el.getBoundingClientRect()
        const cx = e.clientX - rect.left
        const cy = e.clientY - rect.top
        setCam((c) => {
          const nz = Math.min(3, Math.max(0.2, c.zoom * (e.deltaY < 0 ? 1.1 : 1 / 1.1)))
          // Keep the board point under the cursor fixed.
          return { zoom: nz, x: cx - (cx - c.x) * (nz / c.zoom), y: cy - (cy - c.y) * (nz / c.zoom) }
        })
      } else if (e.shiftKey) {
        // Shift+wheel pans horizontally (browsers may already map deltaY→deltaX).
        e.preventDefault()
        const d = e.deltaX || e.deltaY
        setCam((c) => ({ ...c, x: c.x - d }))
      } else {
        e.preventDefault()
        setCam((c) => ({ ...c, x: c.x - e.deltaX, y: c.y - e.deltaY }))
      }
    }
    el.addEventListener('wheel', onWheel, { passive: false })
    return () => el.removeEventListener('wheel', onWheel)
  }, [])

  const removeNode = (id: string): void => {
    const nodes = { ...view.nodes }
    delete nodes[id]
    updateView(frame.id, { ...view, nodes })
  }
  const rectOf = (id: string): Rect => {
    const n = nodeOf(id)
    return { x: n.x, y: n.y, w: n.width ?? CANVAS_DEFAULT_WIDTH, h: heights[id] ?? DEFAULT_HEIGHT }
  }

  // Bounding box of all nodes (padded), so the edge SVG can cover negative space.
  const rects = ids.map(rectOf)
  const minX = rects.length ? Math.min(...rects.map((r) => r.x)) : 0
  const minY = rects.length ? Math.min(...rects.map((r) => r.y)) : 0
  const maxX = rects.length ? Math.max(...rects.map((r) => r.x + r.w)) : 0
  const maxY = rects.length ? Math.max(...rects.map((r) => r.y + r.h)) : 0
  const ox = minX - EDGE_PAD
  const oy = minY - EDGE_PAD

  // Adaptive grid level. Both the cell size and the line thickness scale with
  // zoom so the grid reads as part of the content — but once the on-screen cell
  // would shrink below MIN_CELL (zooming out), doubling the spacing renders only
  // every 2nd line, then every 4th, … instead of collapsing into a solid tint.
  // Thickness tracks the same factor, so it never vanishes sub-pixel.
  const BASE_CELL = 24
  const MIN_CELL = 18
  let gridMult = 1
  while (BASE_CELL * cam.zoom * gridMult < MIN_CELL) gridMult *= 2
  const gridCell = BASE_CELL * cam.zoom * gridMult
  const gridLine = cam.zoom * gridMult

  return (
    <div
      ref={boardRef}
      className={cn('h-full w-full overflow-hidden bg-gray-50', panDrag ? 'cursor-grabbing' : 'cursor-grab')}
      onPointerDown={onBoardPointerDown}
      // Faint line grid that pans and zooms with the board (see gridMult above).
      style={{
        backgroundImage:
          `linear-gradient(to right, rgba(100,116,139,0.11) ${gridLine}px, transparent ${gridLine}px),` +
          `linear-gradient(to bottom, rgba(100,116,139,0.11) ${gridLine}px, transparent ${gridLine}px)`,
        backgroundSize: `${gridCell}px ${gridCell}px`,
        backgroundPosition: `${cam.x}px ${cam.y}px`,
      }}
    >
      <div
        className="relative origin-top-left"
        style={{ transform: `translate(${cam.x}px, ${cam.y}px) scale(${cam.zoom})` }}
      >
        {ids.length > 0 && (
          <svg
            className="pointer-events-none absolute"
            style={{ left: ox, top: oy }}
            width={maxX - minX + 2 * EDGE_PAD}
            height={maxY - minY + 2 * EDGE_PAD}
          >
            <defs>
              <marker
                id={arrowId}
                markerWidth="9"
                markerHeight="9"
                refX="7"
                refY="4"
                orient="auto"
                markerUnits="userSpaceOnUse"
              >
                <path d="M0,0 L8,4 L0,8 z" fill="#94a3b8" />
              </marker>
            </defs>
            {edges.map(([from, to]) => {
              if (!view.nodes[from] || !view.nodes[to]) return null
              const a = rectOf(from)
              const b = rectOf(to)
              const [x1, y1] = borderPoint(a, b.x + b.w / 2, b.y + b.h / 2)
              const [x2, y2] = borderPoint(b, a.x + a.w / 2, a.y + a.h / 2)
              return (
                <line
                  key={`${from}-${to}`}
                  x1={x1 - ox}
                  y1={y1 - oy}
                  x2={x2 - ox}
                  y2={y2 - oy}
                  stroke="#94a3b8"
                  strokeWidth={1.5}
                  markerEnd={`url(#${arrowId})`}
                />
              )
            })}
          </svg>
        )}

        {ids.map((id) => {
          const n = nodeOf(id)
          const others = ids.filter((x) => x !== id)
          return (
            <div
              key={id}
              data-panel=""
              ref={panelElCb(id)}
              // Dragging anywhere on the panel moves it (past a small threshold),
              // except when the press lands in an edit field. select-none stops
              // text highlighting; textareas keep their own selection.
              onPointerDown={(e) => {
                if ((e.target as HTMLElement).closest('textarea, input')) return
                startDrag(id, 'move', e, true)
              }}
              onClickCapture={(e) => {
                if (suppressClickRef.current) {
                  e.preventDefault()
                  e.stopPropagation()
                  suppressClickRef.current = false
                }
              }}
              className={cn(
                'absolute flex flex-col overflow-hidden rounded-lg border bg-white shadow-sm select-none [&_textarea]:select-text',
                id === activeId ? 'border-brand-300 ring-1 ring-brand-200' : 'border-gray-200',
              )}
              style={{ left: n.x, top: n.y, width: n.width ?? CANVAS_DEFAULT_WIDTH }}
            >
              {/* Panels always grow to fit their content — height is never fixed.
                  Stable key by node id — collapse updates reactively via
                  forceCollapsed, so adding/removing a node doesn't remount. */}
              <EntityFrame
                key={id}
                view={entityView(id)}
                actions={actions}
                onDebugEntity={onDebugEntity}
                collapsed={others}
                autoHeight
                onHandle={panelOnHandle(id)}
                extraMenuItems={[{ label: 'Close panel', onClick: () => removeNode(id) }]}
              />

              {/* Only width is resizable (right edge); height grows to fit. */}
              <div
                className="absolute right-0 top-0 h-full w-1.5 cursor-ew-resize"
                onPointerDown={(e) => startDrag(id, 'width', e)}
              />
            </div>
          )
        })}
      </div>
    </div>
  )
}
