/**
 * Everything the user can do, as pure edits to the store plus the writes those
 * edits imply.
 *
 * Persistence is injected, so the same actions run against sqlite in the app
 * and against an array in a test. A write is never waited on: the store is the
 * truth on screen and the file catches up.
 */

import type { Persistence, WriteOp } from '@core/api'
import type { GraphEdge, GraphNode, Model } from '@core/graph'
import { incomingEdge, modelIdOf } from '@core/graph'
import { constantId } from '@core/transforms'
import type { ValueType } from '@core/values'
import { defaultValue } from '@core/values'
import { connectionProblem, defOf, literalOf, openModel } from './derive'
import type { AppState, Notice, Store } from './store'

export function newId(): string {
  return globalThis.crypto?.randomUUID
    ? globalThis.crypto.randomUUID()
    : `id-${Math.random().toString(36).slice(2)}-${Date.now().toString(36)}`
}

const replaceModel = (state: AppState, model: Model): AppState => ({
  ...state,
  models: { ...state.models, [model.id]: model },
})

const withNodes = (model: Model, nodes: Record<string, GraphNode>): Model => ({ ...model, nodes })
const withEdges = (model: Model, edges: Record<string, GraphEdge>): Model => ({ ...model, edges })

export interface Actions {
  load(models: Record<string, Model>): void
  createModel(name?: string): string
  renameModel(id: string, name: string): void
  deleteModel(id: string): void
  openModel(id: string | null): void

  setSelection(ids: string[]): void
  toggleSelection(id: string): void

  addNode(transform: string, x: number, y: number, data?: Record<string, unknown>): string | null
  /** Where a node is being dragged to. Not written: a drag is not an edit yet. */
  moveNode(id: string, x: number, y: number): void
  /** The drag ended here — now it is worth writing down. */
  commitNode(id: string): void
  setNodeValue(id: string, key: string, value: unknown): void
  deleteNodes(ids: string[]): void

  connect(source: string, sourceOutput: string, target: string, targetInput: string): string | null
  deleteEdge(id: string): void
  /**
   * A dropped input handle: a constant of the socket's type, wired in and
   * carrying whatever that socket was already worth.
   */
  spawnConstant(target: string, targetInput: string, x: number, y: number): string | null

  notify(notice: Notice | null): void
}

export function createActions(store: Store, db: Persistence): Actions {
  const write = (...ops: WriteOp[]): void => db.write(ops)

  /** Edit the open model, or do nothing if there isn't one. */
  function edit(change: (model: Model, state: AppState) => { model: Model; ops: WriteOp[] } | null): void {
    store.update((state) => {
      const model = openModel(state)
      if (!model) return state
      const result = change(model, state)
      if (!result) return state
      write(...result.ops)
      return replaceModel(state, result.model)
    })
  }

  return {
    load(models) {
      store.update((state) => ({
        ...state,
        models,
        loaded: true,
        openModelId:
          state.openModelId && models[state.openModelId]
            ? state.openModelId
            : (Object.values(models).sort((a, b) => a.order - b.order)[0]?.id ?? null),
      }))
    },

    createModel(name) {
      const id = newId()
      store.update((state) => {
        const order = Math.max(-1, ...Object.values(state.models).map((m) => m.order)) + 1
        const model: Model = {
          id,
          name: name ?? 'New model',
          order,
          nodes: {},
          edges: {},
        }
        write({ kind: 'model.create', model })
        return { ...replaceModel(state, model), openModelId: id, selection: [] }
      })
      return id
    },

    renameModel(id, name) {
      store.update((state) => {
        const model = state.models[id]
        if (!model || model.name === name) return state
        write({ kind: 'model.rename', id, name })
        return replaceModel(state, { ...model, name })
      })
    },

    deleteModel(id) {
      store.update((state) => {
        if (!state.models[id]) return state
        const ops: WriteOp[] = [{ kind: 'model.delete', id }]
        const models = { ...state.models }
        delete models[id]

        // Nodes standing for the deleted model go with it, rather than being
        // left in other graphs as holes.
        for (const other of Object.values(models)) {
          const orphans = Object.values(other.nodes).filter((n) => modelIdOf(n.transform) === id)
          if (orphans.length === 0) continue
          const nodes = { ...other.nodes }
          const edges = { ...other.edges }
          for (const node of orphans) {
            delete nodes[node.id]
            ops.push({ kind: 'node.delete', id: node.id })
            for (const edge of Object.values(edges)) {
              if (edge.source === node.id || edge.target === node.id) delete edges[edge.id]
            }
          }
          models[other.id] = { ...other, nodes, edges }
        }

        write(...ops)
        const openModelId =
          state.openModelId === id
            ? (Object.values(models).sort((a, b) => a.order - b.order)[0]?.id ?? null)
            : state.openModelId
        return { ...state, models, openModelId, selection: [] }
      })
    },

    openModel(id) {
      store.update((state) =>
        state.openModelId === id ? state : { ...state, openModelId: id, selection: [] },
      )
    },

    setSelection(ids) {
      store.update((state) => ({ ...state, selection: ids }))
    },

    toggleSelection(id) {
      store.update((state) => ({
        ...state,
        selection: state.selection.includes(id)
          ? state.selection.filter((other) => other !== id)
          : [...state.selection, id],
      }))
    },

    addNode(transform, x, y, data = {}) {
      const id = newId()
      let made = false
      edit((model, state) => {
        // A model cannot contain itself, at any depth.
        const nested = modelIdOf(transform)
        if (nested && (nested === model.id || usesModel(state.models, nested, model.id))) return null
        const node: GraphNode = { id, transform, x, y, data }
        made = true
        return {
          model: withNodes(model, { ...model.nodes, [id]: node }),
          ops: [{ kind: 'node.put', modelId: model.id, node }],
        }
      })
      return made ? id : null
    },

    moveNode(id, x, y) {
      edit((model) => {
        const node = model.nodes[id]
        if (!node || (node.x === x && node.y === y)) return null
        return { model: withNodes(model, { ...model.nodes, [id]: { ...node, x, y } }), ops: [] }
      })
    },

    commitNode(id) {
      const model = openModel(store.getState())
      const node = model?.nodes[id]
      if (node) write({ kind: 'node.move', id, x: node.x, y: node.y })
    },

    setNodeValue(id, key, value) {
      edit((model) => {
        const node = model.nodes[id]
        if (!node) return null
        const data = { ...node.data, [key]: value }
        return {
          model: withNodes(model, { ...model.nodes, [id]: { ...node, data } }),
          ops: [{ kind: 'node.data', id, data }],
        }
      })
    },

    deleteNodes(ids) {
      const doomed = new Set(ids)
      store.update((state) => {
        const model = openModel(state)
        if (!model) return state
        const present = [...doomed].filter((id) => id in model.nodes)
        if (present.length === 0) return state
        const nodes = { ...model.nodes }
        const edges = { ...model.edges }
        const ops: WriteOp[] = []
        for (const id of present) {
          delete nodes[id]
          ops.push({ kind: 'node.delete', id })
        }
        for (const edge of Object.values(edges)) {
          if (doomed.has(edge.source) || doomed.has(edge.target)) delete edges[edge.id]
        }
        write(...ops)
        return {
          ...replaceModel(state, withEdges(withNodes(model, nodes), edges)),
          selection: state.selection.filter((id) => !doomed.has(id)),
        }
      })
    },

    connect(source, sourceOutput, target, targetInput) {
      let problem: string | null = null
      edit((model, state) => {
        problem = connectionProblem(model, state.models, source, sourceOutput, target, targetInput)
        if (problem) return null

        // An input takes one edge; a new one replaces what was there.
        const ops: WriteOp[] = []
        const edges = { ...model.edges }
        const existing = incomingEdge(model, target, targetInput)
        if (existing) {
          delete edges[existing.id]
          ops.push({ kind: 'edge.delete', id: existing.id })
        }
        const edge: GraphEdge = { id: newId(), source, sourceOutput, target, targetInput }
        edges[edge.id] = edge
        ops.push({ kind: 'edge.put', modelId: model.id, edge })
        return { model: withEdges(model, edges), ops }
      })
      return problem
    },

    deleteEdge(id) {
      edit((model) => {
        if (!model.edges[id]) return null
        const edges = { ...model.edges }
        delete edges[id]
        return { model: withEdges(model, edges), ops: [{ kind: 'edge.delete', id }] }
      })
    },

    spawnConstant(target, targetInput, x, y) {
      const id = newId()
      let made = false
      edit((model, state) => {
        const node = model.nodes[target]
        if (!node) return null
        const def = defOf(node, state.models)
        const socket = def?.inputs.find((input) => input.name === targetInput)
        if (!socket) return null
        const type: ValueType = socket.type
        const constant: GraphNode = {
          id,
          transform: constantId(type),
          x,
          y,
          data: { value: literalOf(node, socket) ?? defaultValue(type) },
        }
        const ops: WriteOp[] = [{ kind: 'node.put', modelId: model.id, node: constant }]
        const edges = { ...model.edges }
        const existing = incomingEdge(model, target, targetInput)
        if (existing) {
          delete edges[existing.id]
          ops.push({ kind: 'edge.delete', id: existing.id })
        }
        const edge: GraphEdge = {
          id: newId(),
          source: id,
          sourceOutput: 'value',
          target,
          targetInput,
        }
        edges[edge.id] = edge
        ops.push({ kind: 'edge.put', modelId: model.id, edge })
        made = true
        return {
          model: withEdges(withNodes(model, { ...model.nodes, [id]: constant }), edges),
          ops,
        }
      })
      return made ? id : null
    },

    notify(notice) {
      store.update((state) => ({ ...state, notice }))
    },
  }
}

/** Whether `outer` uses `inner` anywhere below it — the containment check. */
function usesModel(
  models: Record<string, Model>,
  outer: string,
  inner: string,
  seen = new Set<string>(),
): boolean {
  if (outer === inner) return true
  if (seen.has(outer)) return false
  seen.add(outer)
  const model = models[outer]
  if (!model) return false
  return Object.values(model.nodes).some((node) => {
    const id = modelIdOf(node.transform)
    return id !== null && usesModel(models, id, inner, seen)
  })
}
