/**
 * The middle pane: the open model as nodes and edges.
 *
 * React Flow does the viewport, the dragging and the wires; it holds none of
 * the state. What is on screen is derived from the store on every render, and
 * every gesture comes straight back out as an action.
 */

import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import {
  Background,
  BackgroundVariant,
  ReactFlow,
  ReactFlowProvider,
  useNodesInitialized,
  useReactFlow,
  type Connection,
  type Edge,
  type EdgeChange,
  type FinalConnectionState,
  type IsValidConnection,
  type Node,
  type NodeChange,
} from '@xyflow/react'
import { isOutputPort, isInputPort } from '@core/transforms'
import type { ValueType } from '@core/values'
import { TYPE_LABELS, describe } from '@core/values'
import type { Evaluation } from '@core/evaluate'
import { useActions, useAppState } from '../hooks'
import {
  connectionProblem,
  defOf,
  literalOf,
  openModel,
  outputsOf,
  searchTransforms,
} from '../state/derive'
import type { AppState } from '../state/store'
import { NodeSearch } from './NodeSearch'
import { NodeView, TransformNode } from './TransformNode'
import { Empty } from './ui'

const nodeTypes = { transform: TransformNode }

/** The word above a node's title: what sort of thing it is. */
function kindOf(transform: string, category: string): string {
  if (isInputPort(transform)) return 'input'
  if (isOutputPort(transform)) return 'output'
  if (transform.startsWith('const.')) return 'constant'
  return category.toLowerCase()
}

function buildNodes(
  state: AppState,
  evaluation: Evaluation,
  onValue: (id: string, key: string, value: unknown) => void,
  onOpenModel: (id: string) => void,
): Node[] {
  const model = openModel(state)
  if (!model) return []
  const selected = new Set(state.selection)

  return Object.values(model.nodes).map((node) => {
    const def = defOf(node, state.models)
    const connected = new Set(
      Object.values(model.edges)
        .filter((edge) => edge.target === node.id)
        .map((edge) => edge.targetInput),
    )
    const outputs = outputsOf(node, state.models, evaluation)
    const modelId = node.transform.startsWith('model:') ? node.transform.slice(6) : null

    const data: NodeView = {
      label: def?.label ?? 'Unknown',
      kind: kindOf(node.transform, def?.category ?? ''),
      accent: modelId !== null,
      error: evaluation.errors.get(node.id),
      params: (def?.params ?? []).map((param) => ({
        name: param.name,
        label: param.label,
        type: param.type,
        literal: literalOf(node, param),
      })),
      inputs: (def?.inputs ?? []).map((socket) => ({
        name: socket.name,
        label: socket.label,
        type: socket.type,
        connected: connected.has(socket.name),
        literal: literalOf(node, socket),
      })),
      outputs: (def?.outputs ?? []).map((socket) => {
        const value = outputs.find((out) => out.name === socket.name)
        return {
          name: socket.name,
          label: socket.label,
          type: socket.type,
          summary: value ? describe(socket.type, value.value) : undefined,
        }
      }),
      onValue: (key, value) => onValue(node.id, key, value),
      onOpen: modelId ? () => onOpenModel(modelId) : undefined,
    }

    return {
      id: node.id,
      type: 'transform',
      position: { x: node.x, y: node.y },
      selected: selected.has(node.id),
      data: data as unknown as Record<string, unknown>,
    }
  })
}

/** An open search box: where it sits, what has been typed, what it will join. */
interface Search {
  at: { x: number; y: number }
  where: { x: number; y: number }
  query: string
  from?: { node: string; output: string; type: ValueType }
}

function Canvas({ evaluation }: { evaluation: Evaluation }) {
  const state = useAppState()
  const actions = useActions()
  const flow = useReactFlow()
  const model = openModel(state)
  const surface = useRef<HTMLDivElement>(null)
  const [search, setSearch] = useState<Search | null>(null)

  const openSearch = useCallback(
    (screen: { x: number; y: number }, where: { x: number; y: number }, from?: Search['from']) => {
      const box = surface.current?.getBoundingClientRect()
      setSearch({
        at: { x: screen.x - (box?.left ?? 0), y: screen.y - (box?.top ?? 0) },
        where,
        query: '',
        from,
      })
    },
    [],
  )

  const matches = useMemo(
    () => (search ? searchTransforms(state.models, search.query, search.from?.type) : []),
    [search, state.models],
  )

  /** Put the chosen transform down, and join it up if something was dragged. */
  const pick = useCallback(
    (item: { transform: string; input?: string }) => {
      if (!search) return
      const id = actions.addNode(item.transform, search.where.x - 60, search.where.y - 20)
      if (!id) actions.notify({ text: 'A model cannot be used inside itself.' })
      else {
        if (search.from && item.input) {
          const problem = actions.connect(search.from.node, search.from.output, id, item.input)
          if (problem) actions.notify({ text: problem })
        }
        actions.setSelection([id])
      }
      setSearch(null)
    },
    [actions, search],
  )

  const nodes = useMemo(
    () =>
      buildNodes(
        state,
        evaluation,
        (id, key, value) => actions.setNodeValue(id, key, value),
        (id) => actions.openModel(id),
      ),
    [state, evaluation, actions],
  )

  const edges = useMemo<Edge[]>(
    () =>
      model
        ? Object.values(model.edges).map((edge) => ({
            id: edge.id,
            source: edge.source,
            sourceHandle: edge.sourceOutput,
            target: edge.target,
            targetHandle: edge.targetInput,
          }))
        : [],
    [model],
  )

  // `fitView` on mount fits whatever has been measured, and a custom node has
  // no size until it has been laid out — so fit again the moment they all do.
  const measured = useNodesInitialized()
  const fitted = useRef(false)
  useEffect(() => {
    if (!measured || fitted.current) return
    fitted.current = true
    flow.fitView({ padding: 0.3, maxZoom: 1 })
  }, [measured, flow])

  /**
   * Selection is the store's, not React Flow's. Letting both hold it races:
   * the controlled `selected` prop lands back on the canvas before the store
   * has been told, and the two take turns undoing each other. So the select
   * changes are applied here and nothing listens to `onSelectionChange`.
   */
  const onNodesChange = useCallback(
    (changes: NodeChange[]) => {
      let selection: string[] | null = null
      for (const change of changes) {
        if (change.type === 'select') {
          const base: string[] = selection ?? state.selection
          selection = change.selected
            ? base.includes(change.id)
              ? base
              : [...base, change.id]
            : base.filter((id) => id !== change.id)
        }
        if (change.type === 'position' && change.position) {
          actions.moveNode(change.id, change.position.x, change.position.y)
          if (change.dragging === false) actions.commitNode(change.id)
        }
        if (change.type === 'remove') actions.deleteNodes([change.id])
      }
      if (selection) actions.setSelection(selection)
    },
    [actions, state.selection],
  )

  const onEdgesChange = useCallback(
    (changes: EdgeChange[]) => {
      for (const change of changes) {
        if (change.type === 'remove') actions.deleteEdge(change.id)
      }
    },
    [actions],
  )

  const onConnect = useCallback(
    (connection: Connection) => {
      if (!connection.sourceHandle || !connection.targetHandle) return
      const problem = actions.connect(
        connection.source,
        connection.sourceHandle,
        connection.target,
        connection.targetHandle,
      )
      if (problem) actions.notify({ text: problem })
    },
    [actions],
  )

  const isValidConnection = useCallback<IsValidConnection>(
    (connection) => {
      if (!model || !connection.sourceHandle || !connection.targetHandle) return false
      return (
        connectionProblem(
          model,
          state.models,
          connection.source,
          connection.sourceHandle,
          connection.target,
          connection.targetHandle,
        ) === null
      )
    },
    [model, state.models],
  )

  /**
   * A handle let go over nothing. An *input* wanted a value, so it gets a
   * constant already holding what that socket was worth. An *output* has a
   * value looking for somewhere to go, so it asks which transform should take
   * it — and only transforms that can are offered.
   */
  const onConnectEnd = useCallback(
    (event: MouseEvent | TouchEvent, connection: FinalConnectionState) => {
      if (connection.isValid) return
      const from = connection.fromHandle
      const node = connection.fromNode
      if (!from || !node || !from.id) return
      const point = 'changedTouches' in event ? event.changedTouches[0] : event
      const screen = { x: point.clientX, y: point.clientY }
      const position = flow.screenToFlowPosition(screen)

      if (from.type === 'target') {
        actions.spawnConstant(node.id, from.id, position.x - 150, position.y - 20)
        return
      }

      const source = model?.nodes[node.id]
      const socket = source && defOf(source, state.models)?.outputs.find((o) => o.name === from.id)
      if (!socket) return
      openSearch(screen, position, { node: node.id, output: from.id, type: socket.type })
    },
    [actions, flow, model, state.models, openSearch],
  )

  const onDrop = useCallback(
    (event: React.DragEvent) => {
      event.preventDefault()
      const transform = event.dataTransfer.getData('application/transform')
      if (!transform) return
      const position = flow.screenToFlowPosition({ x: event.clientX, y: event.clientY })
      const id = actions.addNode(transform, position.x - 80, position.y - 20)
      if (id) actions.setSelection([id])
      else actions.notify({ text: 'A model cannot be used inside itself.' })
    },
    [actions, flow],
  )

  return (
    <div ref={surface} className="relative h-full w-full">
      <ReactFlow
        nodes={nodes}
        edges={edges}
        nodeTypes={nodeTypes}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onConnect={onConnect}
        onConnectEnd={onConnectEnd}
        onPaneContextMenu={(event) => {
          event.preventDefault()
          const screen = { x: event.clientX, y: event.clientY }
          openSearch(screen, flow.screenToFlowPosition(screen))
        }}
        isValidConnection={isValidConnection}
        onDrop={onDrop}
        onDragOver={(event) => {
          event.preventDefault()
          event.dataTransfer.dropEffect = 'copy'
        }}
        // Keys are the command registry's business, not React Flow's.
        deleteKeyCode={null}
        selectionKeyCode={null}
        multiSelectionKeyCode={['Meta', 'Control']}
        // A trackpad scrolls the canvas about; zooming is ctrl/⌘ with it, or a
        // pinch. Double-click belongs to whatever was double-clicked.
        panOnScroll
        zoomOnScroll={false}
        zoomOnPinch
        zoomOnDoubleClick={false}
        minZoom={0.2}
        maxZoom={2}
        fitView
        fitViewOptions={{ padding: 0.3, maxZoom: 1 }}
        proOptions={{ hideAttribution: true }}
      >
        <Background variant={BackgroundVariant.Dots} gap={18} size={1} color="#dedee4" />
      </ReactFlow>

      {search && (
        <NodeSearch
          at={search.at}
          items={matches}
          query={search.query}
          onQuery={(query) => setSearch((was) => (was ? { ...was, query } : was))}
          onPick={pick}
          onClose={() => setSearch(null)}
          note={
            search.from &&
            `Something to take a ${TYPE_LABELS[search.from.type].toLowerCase()}`
          }
        />
      )}
    </div>
  )
}

export function Builder({ evaluation }: { evaluation: Evaluation }) {
  const state = useAppState()
  const model = openModel(state)
  if (!model) {
    return (
      <div className="flex h-full items-center justify-center bg-sunken">
        <Empty>No model open. Make one in the navigator.</Empty>
      </div>
    )
  }
  return (
    // Remounting per model gives each its own viewport rather than one shared.
    <ReactFlowProvider key={model.id}>
      <Canvas evaluation={evaluation} />
    </ReactFlowProvider>
  )
}
