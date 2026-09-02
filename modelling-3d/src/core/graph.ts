/**
 * A model: a graph of transform instances, and the transform it becomes when
 * another model uses it.
 *
 * The only thing that distinguishes a model from a built-in is where its
 * behaviour comes from — ports on the canvas rather than a function — so both
 * are looked up through `lookupTransform` and the rest of the app never has to
 * ask which it is holding.
 */

import type { TransformDef, Socket, Input } from './transforms'
import { builtIn, isInputPort, isOutputPort, portType } from './transforms'
import type { ValueType } from './values'
import { defaultValue } from './values'

export interface GraphNode {
  id: string
  /** A built-in's id, or `model:<id>` for one of the user's own. */
  transform: string
  x: number
  y: number
  /** Literal inputs and params, by socket name. Missing means "use the default". */
  data: Record<string, unknown>
}

export interface GraphEdge {
  id: string
  source: string
  sourceOutput: string
  target: string
  targetInput: string
}

export interface Model {
  id: string
  name: string
  /** Position in the navigator. */
  order: number
  nodes: Record<string, GraphNode>
  edges: Record<string, GraphEdge>
}

export type Models = Record<string, Model>

export const MODEL_PREFIX = 'model:'

export const modelTransformId = (modelId: string): string => `${MODEL_PREFIX}${modelId}`

export const modelIdOf = (transform: string): string | null =>
  transform.startsWith(MODEL_PREFIX) ? transform.slice(MODEL_PREFIX.length) : null

/** A node's ports, in the order they read down the canvas. */
function sortedNodes(model: Model, matches: (t: string) => boolean): GraphNode[] {
  return Object.values(model.nodes)
    .filter((n) => matches(n.transform))
    .sort((a, b) => a.y - b.y || a.x - b.x || a.id.localeCompare(b.id))
}

export const inputPorts = (model: Model): GraphNode[] => sortedNodes(model, isInputPort)
export const outputPorts = (model: Model): GraphNode[] => sortedNodes(model, isOutputPort)

/** The name a port node carries, falling back to its type. */
export function portName(node: GraphNode): string {
  const written = node.data.name
  const type = portType(node.transform)
  return typeof written === 'string' && written.trim() !== '' ? written.trim() : type
}

/** Names made unique, so two ports called the same thing still both work. */
function uniqueNames(nodes: GraphNode[]): string[] {
  const seen = new Map<string, number>()
  return nodes.map((n) => {
    const base = portName(n)
    const count = seen.get(base) ?? 0
    seen.set(base, count + 1)
    return count === 0 ? base : `${base} ${count + 1}`
  })
}

/** The transform a model presents to the models that use it. */
export function modelDef(model: Model): TransformDef {
  const ins = inputPorts(model)
  const outs = outputPorts(model)
  const inNames = uniqueNames(ins)
  const outNames = uniqueNames(outs)
  const inputs: Input[] = ins.map((n, k) => {
    const type = portType(n.transform) as ValueType
    return {
      name: inNames[k],
      type,
      label: inNames[k],
      default: (n.data.value as never) ?? defaultValue(type),
    }
  })
  const outputs: Socket[] = outs.map((n, k) => ({
    name: outNames[k],
    type: portType(n.transform) as ValueType,
    label: outNames[k],
  }))
  return {
    id: modelTransformId(model.id),
    label: model.name,
    category: 'Models',
    summary: `${inputs.length} in, ${outputs.length} out`,
    params: [],
    inputs,
    outputs,
  }
}

export function lookupTransform(transform: string, models: Models): TransformDef | null {
  const modelId = modelIdOf(transform)
  if (modelId === null) return builtIn(transform)
  const model = models[modelId]
  return model ? modelDef(model) : null
}

/** The edge feeding a node's input, if there is one. */
export function incomingEdge(model: Model, nodeId: string, input: string): GraphEdge | null {
  for (const e of Object.values(model.edges)) {
    if (e.target === nodeId && e.targetInput === input) return e
  }
  return null
}

/**
 * The nodes nothing downstream reads — what the preview shows when the
 * selection is empty.
 */
export function terminalNodes(model: Model): string[] {
  const feeding = new Set(Object.values(model.edges).map((e) => e.source))
  return Object.keys(model.nodes).filter((id) => !feeding.has(id))
}

/** Whether `inner` is used by `outer`, directly or at any depth. */
export function modelReaches(outer: string, inner: string, models: Models, seen = new Set<string>()): boolean {
  if (outer === inner) return true
  if (seen.has(outer)) return false
  seen.add(outer)
  const model = models[outer]
  if (!model) return false
  for (const node of Object.values(model.nodes)) {
    const id = modelIdOf(node.transform)
    if (id && modelReaches(id, inner, models, seen)) return true
  }
  return false
}
