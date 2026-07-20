import React, { useEffect, useMemo, useRef, useState } from 'react'
import { X } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import type { EditorActions } from '../views/useEditor'
import { EntityFrame } from './EntityFrame'
import type { ViewHandle } from './useLayout'
import { useCanvas } from './useCanvas'
import {
  CANVAS_DEFAULT_WIDTH,
  ROOT_ID,
  entityView,
  type CanvasNode,
  type CanvasView,
  type Frame,
} from './types'

// The virtualized editor needs a bounded viewport to scroll within, so a canvas
// panel gets a default height rather than growing unbounded. (True auto-grow —
// which the design notes ask for when no height is set — would need a
// non-virtualized render path; flagged in the follow-up notes.)
const DEFAULT_HEIGHT = 260
const MIN_WIDTH = 200
const MIN_HEIGHT = 120

interface Drag {
  id: string
  kind: 'move' | 'width' | 'height' | 'both'
  startX: number
  startY: number
  orig: CanvasNode
}

export interface CanvasProps {
  frame: Frame
  actions: EditorActions
  onDebugEntity: (entityId: string) => void
  /** Persist a change to this frame's view (node moved / resized / added). */
  updateView: (frameId: string, view: CanvasView) => void
  /** Publish a handle up to the layout, proxying to the active panel. */
  onHandle?: (handle: ViewHandle | null) => void
}

/**
 * A spatial board: each entity in `view.nodes` renders as a draggable,
 * resizable {@link EntityFrame} positioned at its stored coordinates, with the
 * other board entities folded (collapsed) inside it. Edges are drawn between
 * nodes whose subtrees reference one another. Double-clicking a folded
 * reference promotes it to its own node.
 */
export function Canvas({ frame, actions, onDebugEntity, updateView, onHandle }: CanvasProps): React.JSX.Element {
  const view = frame.view as CanvasView
  const edges = useCanvas(view.nodes, actions.resolveQuery)

  // The panel keyboard input (w/s, edit, link…) is routed to — set by clicking
  // a panel; defaults to the first node.
  const [activeNodeId, setActiveNodeId] = useState<string | null>(null)
  const nodeIdList = Object.keys(view.nodes)
  const activeId = activeNodeId && view.nodes[activeNodeId] ? activeNodeId : (nodeIdList[0] ?? null)
  const activeRef = useRef(activeId)
  activeRef.current = activeId

  // Each panel publishes its handle here; the board exposes one proxy handle to
  // the layout that forwards to whichever panel is active.
  const panelHandles = useRef<Map<string, ViewHandle>>(new Map())
  const registerPanel = (id: string, h: ViewHandle | null): void => {
    if (h) panelHandles.current.set(id, h)
    else panelHandles.current.delete(id)
  }
  // Stable per-id onHandle callbacks so panels don't re-register every render.
  const panelCbs = useRef<Map<string, (h: ViewHandle | null) => void>>(new Map())
  const panelOnHandle = (id: string): ((h: ViewHandle | null) => void) => {
    let cb = panelCbs.current.get(id)
    if (!cb) {
      cb = (h) => registerPanel(id, h)
      panelCbs.current.set(id, cb)
    }
    return cb
  }

  const proxy = useMemo<ViewHandle>(
    () => ({
      getSelectedEntityId: () => panelHandles.current.get(activeRef.current ?? '')?.getSelectedEntityId() ?? null,
      getSelectedText: () => panelHandles.current.get(activeRef.current ?? '')?.getSelectedText?.() ?? null,
      runAction: (id) => panelHandles.current.get(activeRef.current ?? '')?.runAction?.(id),
    }),
    [],
  )
  useEffect(() => {
    if (!onHandle) return
    onHandle(proxy)
    return () => onHandle(null)
  }, [onHandle, proxy])

  const [drag, setDrag] = useState<Drag | null>(null)
  // The node being dragged, mirrored locally so pointer-moves don't hit
  // localStorage on every pixel; committed to the atom on release.
  const [draft, setDraft] = useState<{ id: string; node: CanvasNode } | null>(null)
  const draftRef = useRef<{ id: string; node: CanvasNode } | null>(null)

  // Pan/zoom of the whole board. Ephemeral (not persisted) for now.
  const boardRef = useRef<HTMLDivElement>(null)
  const [pan, setPan] = useState({ x: 0, y: 0 })
  const [zoom, setZoom] = useState(1)
  const zoomRef = useRef(zoom)
  zoomRef.current = zoom
  const [panDrag, setPanDrag] = useState<{ startX: number; startY: number; orig: { x: number; y: number } } | null>(null)

  const nodeOf = (id: string): CanvasNode => (draft?.id === id ? draft.node : view.nodes[id])

  useEffect(() => {
    if (!drag) return
    const onMove = (e: PointerEvent): void => {
      // Screen deltas → board deltas: undo the zoom scale.
      const z = zoomRef.current
      const dx = (e.clientX - drag.startX) / z
      const dy = (e.clientY - drag.startY) / z
      const node: CanvasNode = { ...drag.orig }
      if (drag.kind === 'move') {
        node.x = Math.max(0, drag.orig.x + dx)
        node.y = Math.max(0, drag.orig.y + dy)
      }
      if (drag.kind === 'width' || drag.kind === 'both') {
        node.width = Math.max(MIN_WIDTH, (drag.orig.width ?? CANVAS_DEFAULT_WIDTH) + dx)
      }
      if (drag.kind === 'height' || drag.kind === 'both') {
        node.height = Math.max(MIN_HEIGHT, (drag.orig.height ?? DEFAULT_HEIGHT) + dy)
      }
      draftRef.current = { id: drag.id, node }
      setDraft(draftRef.current)
    }
    const onUp = (): void => {
      const d = draftRef.current
      if (d) updateView(frame.id, { ...view, nodes: { ...view.nodes, [d.id]: d.node } })
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

  const startDrag = (id: string, kind: Drag['kind'], e: React.PointerEvent): void => {
    e.preventDefault()
    e.stopPropagation()
    setDrag({ id, kind, startX: e.clientX, startY: e.clientY, orig: view.nodes[id] })
  }

  // Pan by dragging empty board background.
  useEffect(() => {
    if (!panDrag) return
    const onMove = (e: PointerEvent): void =>
      setPan({ x: panDrag.orig.x + (e.clientX - panDrag.startX), y: panDrag.orig.y + (e.clientY - panDrag.startY) })
    const onUp = (): void => setPanDrag(null)
    window.addEventListener('pointermove', onMove)
    window.addEventListener('pointerup', onUp)
    return () => {
      window.removeEventListener('pointermove', onMove)
      window.removeEventListener('pointerup', onUp)
    }
  }, [panDrag])

  const onBoardPointerDown = (e: React.PointerEvent): void => {
    // Only the background pans; pointerdowns inside a panel are its own.
    if ((e.target as HTMLElement).closest('[data-panel]')) return
    setPanDrag({ startX: e.clientX, startY: e.clientY, orig: pan })
  }

  // Ctrl/⌘+wheel zooms about the cursor; a plain wheel over the background pans.
  // Attached natively so it can be non-passive (preventDefault is allowed).
  useEffect(() => {
    const el = boardRef.current
    if (!el) return
    const onWheel = (e: WheelEvent): void => {
      if (e.ctrlKey || e.metaKey) {
        e.preventDefault()
        const rect = el.getBoundingClientRect()
        const cx = e.clientX - rect.left
        const cy = e.clientY - rect.top
        const z = zoomRef.current
        const nz = Math.min(3, Math.max(0.2, z * (e.deltaY < 0 ? 1.1 : 1 / 1.1)))
        setPan((p) => ({ x: cx - (cx - p.x) * (nz / z), y: cy - (cy - p.y) * (nz / z) }))
        setZoom(nz)
      } else {
        // Let a fixed-height panel body scroll itself; otherwise pan the board.
        if ((e.target as HTMLElement).closest('[data-panel-scroll]')) return
        e.preventDefault()
        setPan((p) => ({ x: p.x - e.deltaX, y: p.y - e.deltaY }))
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

  const addNode = (entityId: string): void => {
    if (view.nodes[entityId]) return
    const offset = Object.keys(view.nodes).length * 28
    updateView(frame.id, {
      ...view,
      nodes: { ...view.nodes, [entityId]: { x: 60 + offset, y: 60 + offset } },
    })
  }

  const patchNode = (id: string, patch: Partial<CanvasNode>): void =>
    updateView(frame.id, { ...view, nodes: { ...view.nodes, [id]: { ...view.nodes[id], ...patch } } })

  // Toggle a node between a fixed, scrolling height and growing to fit.
  const toggleHeight = (id: string): void =>
    patchNode(id, { height: view.nodes[id].height ? undefined : DEFAULT_HEIGHT })

  const ids = Object.keys(view.nodes)
  const contentW =
    Math.max(0, ...ids.map((id) => nodeOf(id).x + (nodeOf(id).width ?? CANVAS_DEFAULT_WIDTH))) + 200
  const contentH =
    Math.max(0, ...ids.map((id) => nodeOf(id).y + (nodeOf(id).height ?? DEFAULT_HEIGHT))) + 200

  // Edge anchor: the centre of a node's header strip.
  const anchor = (id: string): [number, number] => {
    const n = nodeOf(id)
    return [n.x + (n.width ?? CANVAS_DEFAULT_WIDTH) / 2, n.y + 14]
  }

  return (
    <div
      ref={boardRef}
      className={cn('h-full w-full overflow-hidden bg-gray-50', panDrag ? 'cursor-grabbing' : 'cursor-grab')}
      onPointerDown={onBoardPointerDown}
    >
      <div
        className="relative origin-top-left"
        style={{
          width: contentW,
          height: contentH,
          transform: `translate(${pan.x}px, ${pan.y}px) scale(${zoom})`,
        }}
      >
        <svg className="pointer-events-none absolute inset-0" width={contentW} height={contentH}>
          {edges.map(([from, to]) => {
            if (!view.nodes[from] || !view.nodes[to]) return null
            const [x1, y1] = anchor(from)
            const [x2, y2] = anchor(to)
            return (
              <line
                key={`${from}-${to}`}
                x1={x1}
                y1={y1}
                x2={x2}
                y2={y2}
                stroke="#cbd5e1"
                strokeWidth={1.5}
              />
            )
          })}
        </svg>

        {ids.map((id) => {
          const n = nodeOf(id)
          const others = ids.filter((x) => x !== id)
          return (
            <div
              key={id}
              data-panel=""
              onPointerDownCapture={() => setActiveNodeId(id)}
              className={cn(
                'absolute flex flex-col rounded-lg border bg-white shadow-sm',
                id === activeId ? 'border-brand-300 ring-1 ring-brand-200' : 'border-gray-200',
              )}
              style={{ left: n.x, top: n.y, width: n.width ?? CANVAS_DEFAULT_WIDTH }}
            >
              <div
                className="flex cursor-move select-none items-center gap-1 rounded-t-lg border-b border-gray-100 bg-gray-50 px-2 py-1"
                onPointerDown={(e) => startDrag(id, 'move', e)}
              >
                <span className="truncate text-[11px] font-medium text-gray-500">
                  {id === ROOT_ID ? 'Index' : id}
                </span>
                <div className="flex-1" />
                <button
                  className="rounded px-1 text-[10px] text-gray-400 hover:bg-gray-200 hover:text-gray-700 focus:outline-none"
                  onClick={() => toggleHeight(id)}
                  title={n.height ? 'Grow to fit content' : 'Use a fixed, scrolling height'}
                >
                  {n.height ? 'auto' : 'fix'}
                </button>
                <button
                  className="text-gray-300 hover:text-gray-600 focus:outline-none"
                  onClick={() => removeNode(id)}
                  title="Remove from canvas"
                >
                  <X size={12} />
                </button>
              </div>

              {/* A node with no set height grows to fit; one with a height scrolls. */}
              <div
                className={n.height ? 'overflow-auto' : ''}
                style={n.height ? { height: n.height } : undefined}
                data-panel-scroll={n.height ? '' : undefined}
              >
                {/* Re-key on the folded set so collapse re-seeds when nodes change. */}
                <EntityFrame
                  key={others.join(',')}
                  view={entityView(id)}
                  actions={actions}
                  onDebugEntity={onDebugEntity}
                  collapsed={others}
                  onActivateEntity={addNode}
                  autoHeight={!n.height}
                  onHandle={panelOnHandle(id)}
                />
              </div>

              {/* Resize handles: right edge = width, bottom edge = height, corner = both. */}
              <div
                className="absolute right-0 top-0 h-full w-1.5 cursor-ew-resize"
                onPointerDown={(e) => startDrag(id, 'width', e)}
              />
              <div
                className="absolute bottom-0 left-0 h-1.5 w-full cursor-ns-resize"
                onPointerDown={(e) => startDrag(id, 'height', e)}
              />
              <div
                className="absolute bottom-0 right-0 h-3 w-3 cursor-nwse-resize"
                onPointerDown={(e) => startDrag(id, 'both', e)}
                onDoubleClick={() => patchNode(id, { height: undefined })}
                title="Drag to resize; double-click for auto height"
              />
            </div>
          )
        })}
      </div>
    </div>
  )
}
