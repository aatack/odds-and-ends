import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react'
import {
  Background,
  BackgroundVariant,
  BaseEdge,
  ConnectionMode,
  Controls,
  EdgeLabelRenderer,
  Handle,
  MarkerType,
  NodeResizer,
  Panel,
  Position,
  ReactFlow,
  ReactFlowProvider,
  useEdgesState,
  useInternalNode,
  useNodesInitialized,
  useNodesState,
  useReactFlow,
  useStore,
  useStoreApi,
  type Connection,
  type Edge,
  type EdgeProps,
  type FinalConnectionState,
  type InternalNode,
  type Node,
  type NodeProps,
  type OnNodeDrag,
  type ResizeParams,
} from '@xyflow/react'
import { ArrowRight, Square, Trash01, Type01 } from '@untitledui/icons'
import { EntityMarkdown } from './EntityMarkdown'
import { TextEditor } from './ui/TextEditor'
import { cn } from '../helpers/cn'
import {
  aspectRatioOf,
  boxesOf,
  isArrow,
  isBox,
  isShapeKey,
  shapesOf,
  type ArrowShape,
  type BoxShape,
  type Point,
  type Shape,
} from '../../../core/diagram'
import {
  diagramAtom,
  editDiagramShape,
  focusOn,
  releaseDiagram,
  reportDiagramSelection,
} from '../state/diagram'
import { useAtomValue, useGetEntities } from '../state/hooks'
import { themeAtom } from '../state/ui'
import { runTool } from '../tools/call'

// The canvas over a `diagram` entity: the shapes it holds, drawn where they say,
// dragged where you put them, and written back one key at a time. `core/diagram`
// is what a `diagram/…` value means; this is what it looks like and how a pointer
// changes it.
//
// React Flow does the parts of a canvas nobody should write twice — the viewport,
// the dragging, the connection gesture, the arrow heads — and none of the parts
// that are this app's. The shapes are the entity's values rather than the library's
// state, so a drag ends in a write and the picture is redrawn from what came back;
// every change goes out through a tool, so the palette can do what the pointer
// does; and **no key is bound here.** React Flow's own key handling is switched off
// wholesale, because there is one key listener in this app and it is at the top.
//
// What it draws is the app's own language rather than the library's default look:
// the nodes are our components in our type, the canvas is a dotted field of tone,
// and the few surfaces the library owns are re-pointed at our tokens in `index.css`.

/** The smallest a canvas can be dragged. Below this there is nothing to draw in. */
const MIN_HEIGHT = 120

/** The two ends of an arrow, in the order they read. */
const ENDS = ['from', 'to'] as const
type End = (typeof ENDS)[number]

/** The sides of a box a connection can be started from. */
const SIDES = [
  { id: 'top', position: Position.Top },
  { id: 'right', position: Position.Right },
  { id: 'bottom', position: Position.Bottom },
  { id: 'left', position: Position.Left },
] as const

/** The id of the invisible dot standing at a loose end of an arrow. */
const anchorId = (key: string, end: End): string => `${key}::${end}`

// --- What a node and an edge carry -----------------------------------------

interface ShapeData extends Record<string, unknown> {
  /** The shape this node draws, or the arrow one of whose ends it is. */
  shape: Shape
  /** Which end, for an anchor. Absent on a box. */
  end?: End
}

interface ArrowData extends Record<string, unknown> {
  arrow: ArrowShape
}

type DiagramNode = Node<ShapeData>
type DiagramEdge = Edge<ArrowData, 'arrow'>

// --- What the shapes need from the row they are drawn in --------------------

/**
 * Passed through a context rather than through each node's `data`: a callback in
 * `data` would make every node object new whenever anything on the row changed,
 * and React Flow compares those objects to decide what has moved.
 */
interface DiagramEditor {
  entityId: string
  /** The row's path — what a write is aimed along, as an inline field's calls are. */
  path: string[]
  /** The frame's find text, marked in a shape's text as it is in a row's. */
  highlight?: string
  /** The shape being typed into, if any. */
  editing: string | null
  edit: (key: string | null) => void
  /** Write one shape back as it now stands. */
  write: (shape: Shape) => void
}

const EditorContext = createContext<DiagramEditor | null>(null)

function useEditor(): DiagramEditor {
  const editor = useContext(EditorContext)
  if (!editor) throw new Error('A diagram shape was drawn outside a diagram')
  return editor
}

/** A shape as the entity stores it: everything but the key, which *is* the key. */
const valueOf = ({ key: _key, ...rest }: Shape): Record<string, unknown> => rest

// --- Reading the shapes as a graph -----------------------------------------

/**
 * A node per box, and a node per loose end of an arrow. The loose ends are nodes
 * because that is what makes them draggable, and because an arrow needs something
 * at each end to be drawn between; an end tied to a box needs none, the box being
 * already there.
 */
function nodesOf(shapes: readonly Shape[]): DiagramNode[] {
  const out: DiagramNode[] = []
  for (const shape of shapes) {
    if (isBox(shape)) {
      out.push({
        id: shape.key,
        type: shape.shape,
        position: { x: shape.x, y: shape.y },
        // The size is a style rather than a measurement: it is the shape's own,
        // and the resize handles write it back.
        style: { width: shape.width, height: shape.height },
        data: { shape },
      })
      continue
    }
    for (const end of ENDS) {
      const at = shape[end]
      if (typeof at === 'string') continue
      out.push({
        id: anchorId(shape.key, end),
        type: 'anchor',
        position: at,
        // The dot stands *on* the point rather than hanging below and right of it.
        origin: [0.5, 0.5],
        data: { shape, end },
        // Not a thing to connect to or to remove: it is one end of the arrow it
        // belongs to, and the arrow is the shape.
        connectable: false,
        selectable: false,
      })
    }
  }
  return out
}

/**
 * An edge per arrow, between whatever its ends resolve to. An arrow naming a shape
 * the diagram no longer has is left undrawn rather than drawn to the origin: a
 * value comes off an entity by being written null, so a dangling end is the
 * ordinary aftermath of a removal and not a thing to complain about.
 */
function edgesOf(shapes: readonly Shape[], boxes: ReadonlyMap<string, BoxShape>): DiagramEdge[] {
  const out: DiagramEdge[] = []
  for (const shape of shapes) {
    if (!isArrow(shape)) continue
    const [source, target] = ENDS.map((end) => {
      const at = shape[end]
      if (typeof at !== 'string') return anchorId(shape.key, end)
      return boxes.has(at) ? at : null
    })
    if (!source || !target) continue
    out.push({
      id: shape.key,
      type: 'arrow',
      source,
      target,
      data: { arrow: shape },
      markerEnd: {
        type: MarkerType.ArrowClosed,
        width: 18,
        height: 18,
        // A token rather than a hex: the head is drawn into the flow's own <defs>,
        // in the same document as the canvas, so it follows the theme with
        // everything else.
        color: 'var(--color-gray-500)',
      },
    })
  }
  return out
}

const NODE_TYPES = { rectangle: BoxNode, text: BoxNode, anchor: AnchorNode }
const EDGE_TYPES = { arrow: ArrowEdge }

// --- The row's canvas -------------------------------------------------------

/**
 * A diagram entity's picture, above whatever the note itself says. As wide as the
 * note, and as tall as its `aspectRatio` makes it — dragging the strip along the
 * bottom is what writes that.
 */
export function DiagramView({
  entityId,
  path,
  highlight,
}: {
  entityId: string
  path: string[]
  highlight?: string
}): React.JSX.Element {
  const get = useGetEntities()
  const values = get([entityId])[entityId].values

  // A string that changes exactly when a shape does. The cache hands back a new
  // values object whenever anything anywhere lands in it, so without this the
  // nodes would be rebuilt — and a drag in progress thrown away — on every
  // unrelated read.
  const signature = useMemo(() => {
    const parts: string[] = []
    for (const key of Object.keys(values).sort()) {
      if (isShapeKey(key)) parts.push(key, JSON.stringify(values[key]) ?? 'null')
    }
    return parts.join(' ')
  }, [values])
  const shapes = useMemo(() => shapesOf(values), [signature]) // eslint-disable-line react-hooks/exhaustive-deps
  const aspectRatio = aspectRatioOf(values)

  // The path is a new array on every render and its serialisation is not, so the
  // callbacks below are stable for as long as the row is where it was.
  const at = path.join('\0')
  // Which shape is being typed into is not the canvas's own state: Enter starts an
  // edit and Enter is a tool, so it is held where a tool can reach it.
  const editing = focusOn(useAtomValue(diagramAtom), entityId)?.editing ?? null
  const edit = useCallback(
    (key: string | null): void => editDiagramShape(entityId, key),
    [entityId],
  )

  const write = useCallback(
    (shape: Shape): void => {
      runTool('diagram.shape.set', {
        within: at.split('\0'),
        extra: { shapeKey: shape.key, shapeValue: valueOf(shape) },
      })
    },
    [at],
  )

  const editor = useMemo<DiagramEditor>(
    () => ({ entityId, path: at.split('\0'), highlight, editing, edit, write }),
    [entityId, at, highlight, editing, edit, write],
  )

  return (
    <EditorContext.Provider value={editor}>
      <ReactFlowProvider>
        <Canvas shapes={shapes} aspectRatio={aspectRatio} />
      </ReactFlowProvider>
    </EditorContext.Provider>
  )
}

// --- The canvas itself ------------------------------------------------------

/**
 * The surface: as wide as it is given, as tall as the ratio says, and dragged
 * taller by the strip along its bottom.
 *
 * The wheel is deliberately *not* the zoom. This is a canvas inside an outline,
 * and a diagram that swallowed the scroll would be a hole in the page; the wheel
 * scrolls the tree, and zooming is on the controls in the corner and on a pinch.
 */
function Canvas({
  shapes,
  aspectRatio,
}: {
  shapes: Shape[]
  aspectRatio: number
}): React.JSX.Element {
  const editor = useEditor()
  const theme = useAtomValue(themeAtom)
  const ref = useRef<HTMLDivElement>(null)
  const { height, grip } = useCanvasHeight(ref, editor.path, aspectRatio)
  const { screenToFlowPosition } = useReactFlow()
  const store = useStoreApi()

  const [nodes, setNodes, onNodesChange] = useNodesState<DiagramNode>([])
  const [edges, setEdges, onEdgesChange] = useEdgesState<DiagramEdge>([])

  const shapesByKey = useMemo(() => new Map(shapes.map((s) => [s.key, s])), [shapes])
  const boxes = useMemo(() => boxesOf(shapes), [shapes])

  // The values are the picture and the flow's nodes are a copy of them, so that a
  // drag can move a box before anything has been written. Selection is the one
  // thing the copy owns, so it survives the sync.
  useEffect(() => {
    setNodes((prev) => {
      const was = new Map(prev.map((n) => [n.id, n.selected]))
      return nodesOf(shapes).map((n) => ({ ...n, selected: was.get(n.id) ?? false }))
    })
    setEdges((prev) => {
      const was = new Map(prev.map((e) => [e.id, e.selected]))
      return edgesOf(shapes, boxes).map((e) => ({ ...e, selected: was.get(e.id) ?? false }))
    })
  }, [shapes, boxes, setNodes, setEdges])

  /**
   * Where a drag left things. One write per *shape* rather than per node, since
   * dragging both ends of an arrow at once is two nodes and one value, and each
   * node holds the arrow as it was before the drag — applied separately, the
   * second would undo the first.
   */
  const onNodeDragStop = useCallback<OnNodeDrag<DiagramNode>>(
    (_event, _node, dragged) => {
      const moved = new Map<string, Shape>()
      for (const node of dragged) {
        const key = node.data.shape.key
        const shape = moved.get(key) ?? shapesByKey.get(key)
        if (!shape) continue
        if (isBox(shape)) {
          moved.set(key, { ...shape, x: node.position.x, y: node.position.y })
        } else if (node.data.end) {
          // Dropped on a box, the end takes hold of it and follows it from then
          // on; dropped anywhere else it is the point it was let go at.
          const onto = boxUnder(boxes, node.position)
          moved.set(key, { ...shape, [node.data.end]: onto ? onto.key : node.position })
        }
      }
      for (const shape of moved.values()) editor.write(shape)
    },
    [boxes, shapesByKey, editor],
  )

  /** A connection dragged from one box to another: an arrow tied to both. */
  const onConnect = useCallback(
    ({ source, target }: Connection): void => {
      if (!source || !target || source === target) return
      runTool('diagram.arrow.add', {
        within: editor.path,
        extra: { shapeFrom: source, shapeTo: target },
      })
    },
    [editor.path],
  )

  /**
   * A connection let go of over nothing: an arrow from the box it started at to
   * the point it was dropped at, whose far end is then a dot to drag. `isValid`
   * says it landed on a handle, which `onConnect` has already dealt with.
   */
  const onConnectEnd = useCallback(
    (event: MouseEvent | TouchEvent, state: FinalConnectionState): void => {
      const from = state.fromNode?.id
      if (state.isValid || !from) return
      const pointer = 'changedTouches' in event ? event.changedTouches[0] : event
      const to = screenToFlowPosition({ x: pointer.clientX, y: pointer.clientY })
      runTool('diagram.arrow.add', {
        within: editor.path,
        extra: { shapeFrom: from, shapeTo: to },
      })
    },
    [editor.path, screenToFlowPosition],
  )

  /**
   * The middle of what is on screen, so a shape added from the controls lands in
   * front of whoever pressed the button rather than at the diagram's origin, which
   * may be a long way off. Read at the press rather than subscribed to, so panning
   * doesn't redraw the controls.
   */
  const add = useCallback(
    (toolId: string): void => {
      const { width, height: h, transform } = store.getState()
      const [tx, ty, zoom] = transform
      runTool(toolId, {
        within: editor.path,
        extra: { shapeX: (width / 2 - tx) / zoom, shapeY: (h / 2 - ty) / zoom },
      })
    },
    [editor.path, store],
  )

  const selected = useMemo(
    () =>
      [
        ...nodes.filter((n) => n.selected).map((n) => n.data.shape.key),
        ...edges.filter((e) => e.selected).map((e) => e.id),
      ].filter((key, i, all) => all.indexOf(key) === i),
    [nodes, edges],
  )

  // Said out loud, because the keys that act on a selection are tools and a tool
  // has to be able to ask. Keyed on the serialisation rather than the array, which
  // is new whenever anything about a node is.
  const entityId = editor.entityId
  const selection = selected.join('\0')
  useEffect(() => {
    reportDiagramSelection(entityId, selection ? selection.split('\0') : [])
  }, [entityId, selection])

  // Rows are only mounted near the viewport, so a diagram scrolled off the screen
  // must not still be holding the Backspace key.
  useEffect(() => () => releaseDiagram(entityId), [entityId])

  return (
    <div className="my-1 w-full">
      <div
        ref={ref}
        className="w-full overflow-hidden rounded-md bg-gray-50"
        style={height == null ? { aspectRatio: String(aspectRatio) } : { height }}
      >
        <ReactFlow
          nodes={nodes}
          edges={edges}
          nodeTypes={NODE_TYPES}
          edgeTypes={EDGE_TYPES}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onNodeDragStop={onNodeDragStop}
          onConnect={onConnect}
          onConnectEnd={onConnectEnd}
          onNodeDoubleClick={(_event, node) => editor.edit(node.data.shape.key)}
          onEdgeDoubleClick={(_event, edge) => editor.edit(edge.id)}
          onPaneClick={() => editor.edit(null)}
          colorMode={theme === 'dark' ? 'dark' : 'light'}
          // Either end of a connection may be a side dot, so a drag between two
          // boxes works whichever way round it is made.
          connectionMode={ConnectionMode.Loose}
          minZoom={0.2}
          maxZoom={2}
          fitView
          fitViewOptions={{ padding: 0.25, maxZoom: 1 }}
          // The tree keeps the wheel; a double click is how a shape is typed into.
          zoomOnScroll={false}
          preventScrolling={false}
          zoomOnDoubleClick={false}
          // Every key React Flow would take for itself, given back. There is one
          // key listener in this app and it is at the top of it.
          deleteKeyCode={null}
          selectionKeyCode={null}
          multiSelectionKeyCode={null}
          zoomActivationKeyCode={null}
          panActivationKeyCode={null}
          nodesFocusable={false}
          edgesFocusable={false}
          proOptions={{ hideAttribution: true }}
        >
          <Background variant={BackgroundVariant.Dots} gap={16} size={1} />
          <Controls showInteractive={false} position="bottom-right" />
          <FitWhenDrawn />
          <Panel position="top-right" className="flex items-center gap-1">
            <CanvasButton
              icon={Square}
              label="Add a rectangle"
              onClick={() => add('diagram.rectangle.add')}
            />
            <CanvasButton
              icon={Type01}
              label="Add a text box"
              onClick={() => add('diagram.text.add')}
            />
            <CanvasButton
              icon={ArrowRight}
              label="Add an arrow"
              onClick={() => add('diagram.arrow.add')}
            />
            {selected.length > 0 && (
              <CanvasButton
                icon={Trash01}
                label={selected.length === 1 ? 'Remove this shape' : 'Remove these shapes'}
                onClick={() =>
                  selected.forEach((key) =>
                    runTool('diagram.shape.remove', {
                      within: editor.path,
                      extra: { shapeKey: key },
                    }),
                  )
                }
              />
            )}
          </Panel>
        </ReactFlow>
      </div>
      {/* The bottom edge, as a strip to pull on. Toneless until it is under the
          pointer: it is a handle on the canvas's height and has no business
          drawing a line across the page. */}
      <div
        {...grip}
        className="h-1.5 w-full cursor-ns-resize rounded-b-md hover:bg-gray-200"
        title="Drag to change the height"
      />
    </div>
  )
}

/** The box a point is inside, if any. */
function boxUnder(boxes: ReadonlyMap<string, BoxShape>, at: Point): BoxShape | null {
  for (const box of boxes.values()) {
    const inside =
      at.x >= box.x && at.x <= box.x + box.width && at.y >= box.y && at.y <= box.y + box.height
    if (inside) return box
  }
  return null
}

/**
 * A diagram is drawn from values that arrive after the canvas does, so React
 * Flow's own fit at mount happens over an empty canvas. This does it again, once,
 * when there is something to fit — the count as well as the measuring, since an
 * empty canvas counts as measured and fitting one would spend the one chance on
 * nothing.
 */
function FitWhenDrawn(): null {
  const initialised = useNodesInitialized()
  const drawn = useStore((s) => s.nodes.length > 0)
  const { fitView } = useReactFlow()
  const fitted = useRef(false)
  useEffect(() => {
    if (!initialised || !drawn || fitted.current) return
    fitted.current = true
    void fitView({ padding: 0.25, maxZoom: 1 })
  }, [initialised, drawn, fitView])
  return null
}

/** A control on the canvas: quiet, and in the app's own tone rather than the library's. */
function CanvasButton({
  icon: Icon,
  label,
  onClick,
}: {
  icon: typeof Square
  label: string
  onClick: () => void
}): React.JSX.Element {
  return (
    <button
      title={label}
      aria-label={label}
      onClick={onClick}
      className="flex size-6 items-center justify-center rounded-md bg-white text-gray-400 shadow-xs hover:text-gray-700 focus:outline-none focus-visible:ring-1 focus-visible:ring-brand-300"
    >
      <Icon size={13} />
    </button>
  )
}

/**
 * How tall the canvas is while it is being dragged. Otherwise it is the ratio's to
 * say, and the width's — which is what makes a diagram as wide as the note it is in
 * without anything having to measure anything.
 */
function useCanvasHeight(
  ref: React.RefObject<HTMLDivElement>,
  path: string[],
  aspectRatio: number,
): { height: number | null; grip: React.HTMLAttributes<HTMLDivElement> } {
  const [height, setHeight] = useState<number | null>(null)
  const from = useRef<{ y: number; height: number } | null>(null)

  // Let go of the dragged height once the ratio written from it comes back, so the
  // canvas returns to following the width it is given.
  useEffect(() => setHeight(null), [aspectRatio])

  const onPointerDown = (e: React.PointerEvent<HTMLDivElement>): void => {
    const box = ref.current?.getBoundingClientRect()
    if (e.button !== 0 || !box) return
    e.preventDefault()
    from.current = { y: e.clientY, height: box.height }
    e.currentTarget.setPointerCapture(e.pointerId)
  }

  const onPointerMove = (e: React.PointerEvent<HTMLDivElement>): void => {
    const start = from.current
    if (!start) return
    setHeight(Math.max(MIN_HEIGHT, start.height + (e.clientY - start.y)))
  }

  const onPointerUp = (e: React.PointerEvent<HTMLDivElement>): void => {
    if (!from.current) return
    from.current = null
    if (e.currentTarget.hasPointerCapture(e.pointerId)) {
      e.currentTarget.releasePointerCapture(e.pointerId)
    }
    const box = ref.current?.getBoundingClientRect()
    if (!box?.height) return setHeight(null)
    const ratio = round(box.width / box.height)
    // A drag that came back to where it started has nothing to write — and writing
    // it anyway would leave the height pinned with no event to release it.
    if (ratio === round(aspectRatio)) return setHeight(null)
    runTool('diagram.aspectRatio.set', { within: path, extra: { aspectRatio: ratio } })
  }

  return {
    height,
    grip: { onPointerDown, onPointerMove, onPointerUp, onPointerCancel: onPointerUp },
  }
}

const round = (n: number): number => Math.round(n * 1000) / 1000

// --- Shapes -----------------------------------------------------------------

/**
 * A rectangle, or a line of text with nothing drawn round it. One component,
 * because the difference between them is a surface: the text, the dragging, the
 * resizing and the four dots a connection starts from are the same either way.
 */
function BoxNode({ type, data, selected }: NodeProps<DiagramNode>): React.JSX.Element {
  const editor = useEditor()
  const shape = data.shape
  const framed = type === 'rectangle'

  return (
    <>
      <NodeResizer
        isVisible={selected === true}
        minWidth={40}
        minHeight={24}
        onResizeEnd={(_event, { x, y, width, height }: ResizeParams) => {
          if (isBox(shape)) editor.write({ ...shape, x, y, width, height })
        }}
      />
      {SIDES.map(({ id, position }) => (
        <Handle key={id} id={id} type="source" position={position} />
      ))}
      <div
        className={cn(
          'flex size-full items-center justify-center overflow-hidden rounded-md px-2 py-1',
          framed && 'bg-white shadow-xs ring-1',
          framed && (selected ? 'ring-blue-200' : 'ring-gray-200'),
          !framed && selected && 'bg-blue-100/60',
        )}
      >
        <ShapeText shape={shape} />
      </div>
    </>
  )
}

/**
 * A loose end of an arrow: a dot to take hold of. It is not itself a shape — the
 * arrow is — so it cannot be selected or connected to, and all it says is where
 * that end of the line has got to.
 */
function AnchorNode(): React.JSX.Element {
  return (
    <>
      {/* Kept out of sight by a style rather than a class: the rule that reveals a
          box's dots on hover would otherwise reveal this one too, on top of the
          dot it already is. */}
      <Handle id="a" type="source" position={Position.Top} style={{ opacity: 0 }} />
      <div className="size-2.5 rounded-full bg-gray-300 ring-2 ring-white" />
    </>
  )
}

/**
 * A shape's text, in the app's own rendering of entity text, so a word in a box
 * reads exactly as the same word in a row does — and typed into the app's one text
 * control. A double click opens it; `nodrag` and `nopan` are what stop the canvas
 * treating a caret as a gesture.
 */
function ShapeText({ shape }: { shape: Shape }): React.JSX.Element {
  const editor = useEditor()

  if (editor.editing === shape.key) {
    return (
      <TextEditor
        autoFocus
        value={shape.text}
        setValue={(text) => editor.write({ ...shape, text })}
        onBlur={() => editor.edit(null)}
        placeholder="Label…"
        className="nodrag nopan nowheel block text-center font-serif text-[13px] leading-4 text-gray-900"
      />
    )
  }

  if (!shape.text) {
    return (
      <span className="font-serif text-[13px] italic leading-4 text-gray-400 select-none">
        Label
      </span>
    )
  }

  return (
    <EntityMarkdown
      entityId={editor.entityId}
      path={editor.path}
      text={shape.text}
      highlight={editor.highlight}
      className="block text-center font-serif text-[13px] leading-4 text-gray-900"
    />
  )
}

// --- Arrows -----------------------------------------------------------------

/**
 * An arrow between two things, wherever they have got to. The line is computed
 * from where the two nodes actually are rather than from a handle's position, so an
 * arrow leaves the side of the box it points at and follows that box as it is
 * dragged — and an end tied to nothing is a dot the line stops at.
 */
function ArrowEdge({
  id,
  source,
  target,
  data,
  selected,
  markerEnd,
}: EdgeProps<DiagramEdge>): React.JSX.Element | null {
  const editor = useEditor()
  const from = useInternalNode(source)
  const to = useInternalNode(target)
  if (!from || !to) return null

  const a = borderPoint(from, to)
  const b = borderPoint(to, from)
  const arrow = data?.arrow
  const editing = arrow != null && editor.editing === arrow.key

  return (
    <>
      <BaseEdge
        id={id}
        path={`M ${a.x},${a.y} L ${b.x},${b.y}`}
        markerEnd={markerEnd}
        style={{ strokeWidth: selected ? 2 : 1.5 }}
      />
      {arrow && (arrow.text || editing) && (
        <EdgeLabelRenderer>
          <div
            className="nodrag nopan pointer-events-auto absolute max-w-40 rounded-md bg-white px-1 shadow-xs"
            style={{
              transform: `translate(-50%, -50%) translate(${(a.x + b.x) / 2}px, ${(a.y + b.y) / 2}px)`,
            }}
          >
            <ShapeText shape={arrow} />
          </div>
        </EdgeLabelRenderer>
      )}
    </>
  )
}

/** A node as a centre and a half-size, which is all the geometry below needs. */
function boundsOf(node: InternalNode): { cx: number; cy: number; hw: number; hh: number } {
  const { x, y } = node.internals.positionAbsolute
  const w = node.measured.width ?? 0
  const h = node.measured.height ?? 0
  return { cx: x + w / 2, cy: y + h / 2, hw: w / 2, hh: h / 2 }
}

/**
 * Where the line between two nodes' centres crosses the first one's border — or the
 * centre itself, for a node with no size, which is what an arrow's loose end is.
 * Never past the other centre, so two boxes on top of one another draw a stub
 * rather than an arrow pointing backwards.
 */
function borderPoint(node: InternalNode, towards: InternalNode): Point {
  const a = boundsOf(node)
  const b = boundsOf(towards)
  const dx = b.cx - a.cx
  const dy = b.cy - a.cy
  if (!dx && !dy) return { x: a.cx, y: a.cy }
  const t = Math.min(dx ? a.hw / Math.abs(dx) : Infinity, dy ? a.hh / Math.abs(dy) : Infinity, 1)
  return { x: a.cx + dx * t, y: a.cy + dy * t }
}
