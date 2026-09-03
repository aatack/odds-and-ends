import React, { useCallback, useEffect, useMemo, useState } from 'react'
import {
  Background,
  BackgroundVariant,
  Controls,
  MarkerType,
  Panel,
  ReactFlow,
  ReactFlowProvider,
  useReactFlow,
  type Connection,
  type Edge,
  type Node,
  type OnNodeDrag,
} from '@xyflow/react'
import { Plus, Wifi } from '@untitledui/icons'
import type { SourceNode } from '../../../core/client'
import { NODE_KINDS, nodeKind, publishes } from '../../../core/client'
import { PhoneAccessPanel } from '../components/PhoneAccess'
import {
  NodeContextProvider,
  PensiveNode,
  type PensiveFlowNode,
} from '../components/sources/PensiveNode'
import { AccessModal } from '../components/sources/AccessModal'
import { Button } from '../components/ui/Button'
import { Modal } from '../components/ui/Modal'
import { useAtomValue } from '../state/hooks'
import { clearSourceSelection, reportSourceSelection } from '../state/sources'
import { themeAtom, updateUi } from '../state/ui'
import { positionPatch, useSourceGraph } from './useSourceGraph'
import { useTailscale } from './useTailscale'

// The sources page: every pensive this app knows about, drawn as a graph.
//
// A node is a store, a way of joining stores, or a way of publishing one; an
// edge is "read that one". The node at the origin is this window, so what the
// outliner shows is whatever has been dragged into it — which is why there is
// one button in the corner and no source picker anywhere.
//
// React Flow does the viewport, the dragging and the connection gesture, and
// none of the state: the graph lives in the main process, every gesture is a
// round trip, and what is drawn is what is running. **No key is bound here** —
// there is one key listener in this app and it is at the top, so Backspace is a
// tool (`sources.delete`) reading the selection this page reports.

const NODE_TYPES = { pensive: PensiveNode }

export function Sources(): React.JSX.Element {
  return (
    <ReactFlowProvider>
      <Canvas />
    </ReactFlowProvider>
  )
}

function Canvas(): React.JSX.Element {
  const { graph, error, actions } = useSourceGraph()
  // One instance for the page: there is a single serve config for the machine.
  const tailscale = useTailscale()
  const theme = useAtomValue(themeAtom)
  const flow = useReactFlow()
  const [adding, setAdding] = useState(false)
  const [phone, setPhone] = useState(false)
  const [accessId, setAccessId] = useState<string | null>(null)

  useEffect(() => clearSourceSelection, [])

  /** Which nodes feed each node, in the order they were connected. */
  const inputs = useMemo(() => {
    const map: Record<string, string[]> = {}
    for (const edge of graph?.edges ?? []) (map[edge.to] ??= []).push(edge.from)
    return map
  }, [graph])

  const nodes = useMemo<PensiveFlowNode[]>(
    () =>
      (graph?.nodes ?? []).map((node) => ({
        id: node.id,
        type: 'pensive' as const,
        position: { x: node.x, y: node.y },
        data: { node },
        // The window's node is where the graph is anchored, so it stays put.
        draggable: nodeKind(node.config.kind).addable,
      })),
    [graph],
  )

  const edges = useMemo<Edge[]>(
    () =>
      (graph?.edges ?? []).map((edge) => ({
        id: edge.id,
        source: edge.from,
        target: edge.to,
        markerEnd: { type: MarkerType.ArrowClosed, width: 14, height: 14 },
      })),
    [graph],
  )

  const onConnect = useCallback(
    (connection: Connection) => {
      if (connection.source && connection.target) {
        void actions.connect(connection.source, connection.target)
      }
    },
    [actions],
  )

  const onNodeDragStop = useCallback<OnNodeDrag<PensiveFlowNode>>(
    (_event, node) => void actions.updateNode(node.id, positionPatch(node.position.x, node.position.y)),
    [actions],
  )

  /** Somewhere sensible for a new node: the middle of what is on screen. */
  const addAt = useCallback(
    (kind: (typeof NODE_KINDS)[number]['kind']) => {
      const { x, y, zoom } = flow.getViewport()
      const centre = {
        x: (window.innerWidth / 2 - x) / zoom - 128,
        y: (window.innerHeight / 2 - y) / zoom - 40,
      }
      setAdding(false)
      void actions.addNode(kind, centre.x, centre.y)
    },
    [actions, flow],
  )

  const access = graph?.nodes.find((n) => n.id === accessId) ?? null

  return (
    <NodeContextProvider
      value={{
        actions,
        nodes: graph?.nodes ?? [],
        inputs,
        status: graph?.status ?? {},
        openAccess: setAccessId,
      }}
    >
      <div className="relative h-full">
        <ReactFlow
          nodes={nodes}
          edges={edges}
          nodeTypes={NODE_TYPES}
          onConnect={onConnect}
          onNodeDragStop={onNodeDragStop}
          onSelectionChange={({ nodes: selectedNodes, edges: selectedEdges }) =>
            reportSourceSelection({
              nodes: selectedNodes.map((n: Node) => n.id),
              edges: selectedEdges.map((e: Edge) => e.id),
            })
          }
          colorMode={theme === 'dark' ? 'dark' : 'light'}
          minZoom={0.3}
          maxZoom={1.5}
          fitView
          fitViewOptions={{ padding: 0.3, maxZoom: 1 }}
          // Every key React Flow would take for itself, given back.
          deleteKeyCode={null}
          selectionKeyCode={null}
          multiSelectionKeyCode={null}
          zoomActivationKeyCode={null}
          panActivationKeyCode={null}
          proOptions={{ hideAttribution: true }}
        >
          <Background variant={BackgroundVariant.Dots} gap={16} size={1} />
          <Controls showInteractive={false} position="bottom-right" />

          <Panel position="top-left" className="flex items-center gap-2">
            <Button variant="primary" size="sm" onClick={() => setAdding(true)}>
              <Plus size={16} /> Add
            </Button>
            <Button variant="secondary" size="sm" onClick={() => setPhone(true)}>
              <Wifi size={16} /> Phone
            </Button>
          </Panel>

          <Panel position="top-right">
            <Button variant="secondary" size="sm" onClick={() => updateUi({ page: 'editor' })}>
              Open the outliner
            </Button>
          </Panel>

          {error && (
            <Panel position="bottom-left">
              <p className="max-w-md rounded-md bg-white px-3 py-2 text-[13px] text-error-600 shadow-xs">
                {error}
              </p>
            </Panel>
          )}
        </ReactFlow>

        {adding && <AddNode onPick={addAt} onClose={() => setAdding(false)} />}
        {phone && (
          <Modal title="Phone access" onClose={() => setPhone(false)} size="wide">
            <PhoneAccessPanel model={tailscale} />
          </Modal>
        )}
        {access && publishes(access.config.kind) && (
          <AccessModal
            node={access}
            localUrl={graph?.status[access.id]?.localUrl ?? null}
            actions={actions}
            tailscale={tailscale}
            onClose={() => setAccessId(null)}
          />
        )}
      </div>
    </NodeContextProvider>
  )
}

/** The list of things a node can be, with a line each on what it is for. */
function AddNode({
  onPick,
  onClose,
}: {
  onPick: (kind: SourceNode['config']['kind']) => void
  onClose: () => void
}): React.JSX.Element {
  return (
    <Modal title="Add a node" onClose={onClose}>
      <div className="space-y-1">
        {NODE_KINDS.filter((k) => k.addable).map((kind) => (
          <button
            key={kind.kind}
            className="block w-full rounded-md px-3 py-2 text-left hover:bg-gray-50 focus:outline-none focus-visible:bg-gray-50"
            onClick={() => onPick(kind.kind)}
          >
            <span className="block text-[13px] text-gray-900">{kind.label}</span>
            <span className="block text-xs text-gray-400">{kind.blurb}</span>
          </button>
        ))}
      </div>
    </Modal>
  )
}
