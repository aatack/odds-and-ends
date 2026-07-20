import React, { useEffect, useRef, useState } from 'react'
import { X } from '@untitledui/icons'
import type { EditorActions } from '../views/useEditor'
import { EntityFrame } from './EntityFrame'
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
  kind: 'move' | 'width' | 'height'
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
}

/**
 * A spatial board: each entity in `view.nodes` renders as a draggable,
 * resizable {@link EntityFrame} positioned at its stored coordinates, with the
 * other board entities folded (collapsed) inside it. Edges are drawn between
 * nodes whose subtrees reference one another. Double-clicking a folded
 * reference promotes it to its own node.
 */
export function Canvas({ frame, actions, onDebugEntity, updateView }: CanvasProps): React.JSX.Element {
  const view = frame.view as CanvasView
  const edges = useCanvas(view.nodes, actions.resolveQuery)

  const [drag, setDrag] = useState<Drag | null>(null)
  // The node being dragged, mirrored locally so pointer-moves don't hit
  // localStorage on every pixel; committed to the atom on release.
  const [draft, setDraft] = useState<{ id: string; node: CanvasNode } | null>(null)
  const draftRef = useRef<{ id: string; node: CanvasNode } | null>(null)

  const nodeOf = (id: string): CanvasNode => (draft?.id === id ? draft.node : view.nodes[id])

  useEffect(() => {
    if (!drag) return
    const onMove = (e: PointerEvent): void => {
      const dx = e.clientX - drag.startX
      const dy = e.clientY - drag.startY
      const node: CanvasNode = { ...drag.orig }
      if (drag.kind === 'move') {
        node.x = Math.max(0, drag.orig.x + dx)
        node.y = Math.max(0, drag.orig.y + dy)
      } else if (drag.kind === 'width') {
        node.width = Math.max(MIN_WIDTH, (drag.orig.width ?? CANVAS_DEFAULT_WIDTH) + dx)
      } else {
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
    <div className="h-full w-full overflow-auto bg-gray-50">
      <div className="relative" style={{ width: contentW, height: contentH }}>
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
              className="absolute flex flex-col rounded-lg border border-gray-200 bg-white shadow-sm"
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
                  className="text-gray-300 hover:text-gray-600 focus:outline-none"
                  onClick={() => removeNode(id)}
                  title="Remove from canvas"
                >
                  <X size={12} />
                </button>
              </div>

              <div className="overflow-auto" style={{ height: n.height ?? DEFAULT_HEIGHT }}>
                {/* Re-key on the folded set so collapse re-seeds when nodes change. */}
                <EntityFrame
                  key={others.join(',')}
                  view={entityView(id)}
                  actions={actions}
                  onDebugEntity={onDebugEntity}
                  collapsed={others}
                  onActivateEntity={addNode}
                />
              </div>

              {/* Resize handles: right edge = width, bottom edge = height. */}
              <div
                className="absolute right-0 top-0 h-full w-1.5 cursor-ew-resize"
                onPointerDown={(e) => startDrag(id, 'width', e)}
              />
              <div
                className="absolute bottom-0 left-0 h-1.5 w-full cursor-ns-resize"
                onPointerDown={(e) => startDrag(id, 'height', e)}
              />
            </div>
          )
        })}
      </div>
    </div>
  )
}
