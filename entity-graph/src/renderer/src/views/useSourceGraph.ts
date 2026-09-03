import { useCallback, useEffect, useMemo, useState } from 'react'
import type { NodeKind, NodePatch, SourceGraph, SourceToken } from '../../../core/client'

const api = window.entityGraph

export interface SourceGraphActions {
  refresh: () => Promise<void>
  addNode: (kind: NodeKind, x: number, y: number) => Promise<void>
  updateNode: (id: string, patch: NodePatch) => Promise<void>
  removeNode: (id: string) => Promise<void>
  /** Plug one node's output into another's input. A loop is refused. */
  connect: (from: string, to: string) => Promise<void>
  disconnect: (edgeId: string) => Promise<void>
  tokens: (nodeId: string) => Promise<SourceToken[]>
  issueToken: (nodeId: string, name: string) => Promise<SourceToken>
  pauseToken: (token: string, paused: boolean) => Promise<void>
  revokeToken: (token: string) => Promise<void>
}

export interface SourceGraphModel {
  /** Null until the first read comes back. */
  graph: SourceGraph | null
  /** The last refused gesture, cleared when the next one is made. */
  error: string | null
  actions: SourceGraphActions
}

/**
 * All the logic behind the sources page: the graph as the main process holds it,
 * and the gestures that change it.
 *
 * Nothing is kept optimistically. Every change is a round trip that rebuilds the
 * pensives and may start or stop a server, and the answer to "what is wrong with
 * this node" only exists on the other side — so the drawing shown is always the
 * one the app is actually running.
 */
export function useSourceGraph(): SourceGraphModel {
  const [graph, setGraph] = useState<SourceGraph | null>(null)
  const [error, setError] = useState<string | null>(null)

  const refresh = useCallback(async () => {
    try {
      setGraph(await api.readGraph())
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [])

  useEffect(() => {
    void refresh()
  }, [refresh])

  // Something else may have changed it — a token revoked from a node's panel,
  // or the user renaming themselves, both of which rebuild every pensive.
  useEffect(() => api.onPensiveChanged(() => void refresh()), [refresh])

  /** Run a change, keep what it refused, and re-read either way. */
  const change = useCallback(
    async (fn: () => Promise<unknown>) => {
      setError(null)
      try {
        await fn()
      } catch (e) {
        setError(e instanceof Error ? e.message : String(e))
      }
      await refresh()
    },
    [refresh],
  )

  const actions = useMemo<SourceGraphActions>(
    () => ({
      refresh,
      addNode: (kind, x, y) => change(() => api.addNode(kind, x, y)),
      updateNode: (id, patch) => change(() => api.updateNode(id, patch)),
      removeNode: (id) => change(() => api.removeNode(id)),
      connect: (from, to) => change(() => api.connectNodes(from, to)),
      disconnect: (edgeId) => change(() => api.disconnectNodes(edgeId)),
      tokens: (nodeId) => api.listTokens(nodeId),
      issueToken: (nodeId, name) => api.issueToken(nodeId, name),
      pauseToken: async (token, paused) => {
        await api.pauseToken(token, paused)
      },
      revokeToken: async (token) => {
        await api.revokeToken(token)
      },
    }),
    [refresh, change],
  )

  return { graph, error, actions }
}

/**
 * Where a node's position is written back. A drag fires on every frame, so the
 * write is left until the drag stops — {@link SourceGraphActions.updateNode}
 * with only `x` and `y` is the one change that rebuilds nothing.
 */
export const positionPatch = (x: number, y: number): NodePatch => ({
  x: Math.round(x),
  y: Math.round(y),
})
