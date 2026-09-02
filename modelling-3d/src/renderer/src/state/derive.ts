/**
 * Everything the views need that isn't stored: the open model, its evaluation,
 * what the preview is looking at, what the navigator lists, and whether a
 * connection is allowed.
 *
 * Pure functions of state. Nothing here is ever written back into the store.
 */

import type { Evaluation } from '@core/evaluate'
import { evaluateModel } from '@core/evaluate'
import type { GraphNode, Model, Models } from '@core/graph'
import { lookupTransform, modelDef, modelIdOf, terminalNodes } from '@core/graph'
import type { Scene } from '@core/scene'
import { sceneOf } from '@core/scene'
import type { Socket, TransformDef } from '@core/transforms'
import { BUILT_IN, CATEGORIES, isOutputPort, portType } from '@core/transforms'
import type { ValueType } from '@core/values'
import { isValue } from '@core/values'
import type { AppState } from './store'

export function openModel(state: AppState): Model | null {
  return state.openModelId ? (state.models[state.openModelId] ?? null) : null
}

/** Models in the order the navigator shows them. */
export function modelList(state: AppState): Model[] {
  return Object.values(state.models).sort((a, b) => a.order - b.order || a.name.localeCompare(b.name))
}

export function evaluationOf(model: Model | null, models: Models): Evaluation {
  if (!model) return { values: new Map(), errors: new Map() }
  return evaluateModel(model, models)
}

export const defOf = (node: GraphNode, models: Models): TransformDef | null =>
  lookupTransform(node.transform, models)

/** What a socket is worth right now: the literal written on the node, or its default. */
export function literalOf(node: GraphNode, socket: Socket & { default: unknown }): unknown {
  const written = node.data[socket.name]
  return isValue(socket.type, written) ? written : socket.default
}

export interface OutputValue {
  name: string
  type: ValueType
  value: unknown
}

/**
 * A node's outputs with their types. An output port has no socket of its own —
 * it *is* its input — so its value is reported under the port's type.
 */
export function outputsOf(
  node: GraphNode,
  models: Models,
  evaluation: Evaluation,
): OutputValue[] {
  const computed = evaluation.values.get(node.id)
  if (!computed) return []
  if (isOutputPort(node.transform)) {
    return 'value' in computed
      ? [{ name: 'value', type: portType(node.transform), value: computed.value }]
      : []
  }
  const def = defOf(node, models)
  if (!def) return []
  return def.outputs
    .filter((socket) => socket.name in computed)
    .map((socket) => ({ name: socket.name, type: socket.type, value: computed[socket.name] }))
}

/**
 * The nodes the preview is showing: what is selected, or — with nothing
 * selected — everything nothing downstream reads.
 */
export function previewedNodes(state: AppState): string[] {
  const model = openModel(state)
  if (!model) return []
  const selected = state.selection.filter((id) => id in model.nodes)
  return selected.length > 0 ? selected : terminalNodes(model)
}

export function previewScene(state: AppState, evaluation: Evaluation): Scene {
  const model = openModel(state)
  if (!model) return sceneOf([])
  const values = previewedNodes(state).flatMap((id) =>
    outputsOf(model.nodes[id], state.models, evaluation),
  )
  return sceneOf(values)
}

// ---------------------------------------------------------------------------
// The navigator
// ---------------------------------------------------------------------------

export interface NavigatorItem {
  transform: string
  label: string
  summary: string
  /** Set when the item is one of the user's own models. */
  modelId?: string
}

export interface NavigatorGroup {
  category: string
  items: NavigatorItem[]
}

/**
 * The palette: the user's own models first, since those are the reason for the
 * app, then the built-ins by category. Constants are left out — one is made by
 * dragging an input handle into empty space, which is where you want it.
 */
export function navigatorGroups(state: AppState, openId: string | null): NavigatorGroup[] {
  const models: NavigatorItem[] = modelList(state)
    .filter((model) => model.id !== openId)
    .map((model) => {
      const def = modelDef(model)
      return {
        transform: def.id,
        label: model.name,
        summary: def.summary,
        modelId: model.id,
      }
    })

  const groups: NavigatorGroup[] = models.length > 0 ? [{ category: 'Models', items: models }] : []
  for (const category of CATEGORIES) {
    if (category === 'Constants') continue
    const items = BUILT_IN.filter((def) => def.category === category).map((def) => ({
      transform: def.id,
      label: def.label,
      summary: def.summary,
    }))
    if (items.length > 0) groups.push({ category, items })
  }
  return groups
}

export function searchGroups(groups: NavigatorGroup[], query: string): NavigatorGroup[] {
  const needle = query.trim().toLowerCase()
  if (needle === '') return groups
  return groups
    .map((group) => ({
      category: group.category,
      items: group.items.filter(
        (item) =>
          item.label.toLowerCase().includes(needle) ||
          item.summary.toLowerCase().includes(needle) ||
          group.category.toLowerCase().includes(needle),
      ),
    }))
    .filter((group) => group.items.length > 0)
}

// ---------------------------------------------------------------------------
// Connections
// ---------------------------------------------------------------------------

/** Whether `to` already feeds `from`, which is what would make a loop. */
export function reaches(model: Model, from: string, to: string, seen = new Set<string>()): boolean {
  if (from === to) return true
  if (seen.has(from)) return false
  seen.add(from)
  for (const edge of Object.values(model.edges)) {
    if (edge.target === from && reaches(model, edge.source, to, seen)) return true
  }
  return false
}

/** Why a connection can't be made, or null if it can. */
export function connectionProblem(
  model: Model,
  models: Models,
  source: string,
  sourceOutput: string,
  target: string,
  targetInput: string,
): string | null {
  if (source === target) return 'a node cannot feed itself'
  const from = model.nodes[source]
  const to = model.nodes[target]
  if (!from || !to) return 'that node is gone'
  const fromDef = defOf(from, models)
  const toDef = defOf(to, models)
  if (!fromDef || !toDef) return 'that transform is unknown'

  const out = isOutputPort(from.transform)
    ? null
    : fromDef.outputs.find((socket) => socket.name === sourceOutput)
  const into = toDef.inputs.find((socket) => socket.name === targetInput)
  if (!out || !into) return 'that socket is gone'
  if (out.type !== into.type) return `a ${out.type} does not fit a ${into.type}`
  if (reaches(model, source, target)) return 'that would make a loop'
  return null
}

/** The models this one uses, so deleting one can say what it will take with it. */
export function instancesOfModel(models: Models, modelId: string): { model: Model; nodes: GraphNode[] }[] {
  return Object.values(models)
    .map((model) => ({
      model,
      nodes: Object.values(model.nodes).filter((node) => modelIdOf(node.transform) === modelId),
    }))
    .filter((entry) => entry.nodes.length > 0)
}
