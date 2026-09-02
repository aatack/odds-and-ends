/**
 * Evaluating a model: every node's outputs, or the reason it has none.
 *
 * Pure and synchronous. Nothing is cached between calls — a model is small
 * enough that recomputing the lot on every edit is what keeps the preview
 * honest, and it means there is no invalidation to get wrong.
 */

import type { GraphNode, Model, Models } from './graph'
import { inputPorts, lookupTransform, modelIdOf, outputPorts, portName } from './graph'
import { isInputPort, isOutputPort, portType } from './transforms'
import { isValue } from './values'

export interface Evaluation {
  /** Node id to its outputs, for the nodes that computed. */
  values: Map<string, Record<string, unknown>>
  /** Node id to why it did not. */
  errors: Map<string, string>
}

/** Thrown inside the walk; never escapes `evaluateModel`. */
class Failure extends Error {}

const MAX_DEPTH = 16

export function evaluateModel(
  model: Model,
  models: Models,
  bindings: Record<string, unknown> = {},
  stack: string[] = [],
): Evaluation {
  const values = new Map<string, Record<string, unknown>>()
  const errors = new Map<string, string>()
  const visiting = new Set<string>()
  // The models being evaluated, outermost first, so a model that reaches
  // itself is caught on the first call rather than one level down.
  const chain = [...stack, model.id]

  const edgeInto = new Map<string, { source: string; sourceOutput: string }>()
  for (const e of Object.values(model.edges)) {
    edgeInto.set(`${e.target} ${e.targetInput}`, { source: e.source, sourceOutput: e.sourceOutput })
  }

  function fail(nodeId: string, message: string): never {
    errors.set(nodeId, message)
    throw new Failure(message)
  }

  function compute(nodeId: string): Record<string, unknown> {
    const done = values.get(nodeId)
    if (done) return done
    const failed = errors.get(nodeId)
    if (failed !== undefined) throw new Failure(failed)
    if (visiting.has(nodeId)) fail(nodeId, 'this node feeds itself')
    visiting.add(nodeId)
    try {
      const outputs = run(model.nodes[nodeId])
      values.set(nodeId, outputs)
      return outputs
    } finally {
      visiting.delete(nodeId)
    }
  }

  function run(node: GraphNode): Record<string, unknown> {
    const def = lookupTransform(node.transform, models)
    if (!def) fail(node.id, `unknown transform "${node.transform}"`)

    // An input port takes what the caller bound to it, or its own default when
    // the model is being looked at on its own.
    if (isInputPort(node.transform)) {
      const name = portName(node)
      const type = portType(node.transform)
      if (name in bindings) return { value: bindings[name] }
      const written = node.data.value
      return { value: isValue(type, written) ? written : def.params[1].default }
    }

    const inputs: Record<string, unknown> = {}
    for (const socket of def.inputs) {
      const edge = edgeInto.get(`${node.id} ${socket.name}`)
      if (edge) {
        let upstream: Record<string, unknown>
        try {
          upstream = compute(edge.source)
        } catch (error) {
          // An output port *is* its input, so it reports the reason unadorned.
          const why = (error as Error).message
          fail(node.id, isOutputPort(node.transform) ? why : `${socket.label}: ${why}`)
        }
        if (!(edge.sourceOutput in upstream)) {
          fail(node.id, `${socket.label}: nothing came out of the node feeding it`)
        }
        inputs[socket.name] = upstream[edge.sourceOutput]
      } else {
        const written = node.data[socket.name]
        inputs[socket.name] = isValue(socket.type, written) ? written : socket.default
      }
    }

    const params: Record<string, unknown> = {}
    for (const param of def.params) {
      const written = node.data[param.name]
      params[param.name] = isValue(param.type, written) ? written : param.default
    }

    const nested = modelIdOf(node.transform)
    if (nested !== null) return callModel(node, nested, inputs)

    try {
      return def.evaluate ? def.evaluate(inputs, params) : {}
    } catch (error) {
      fail(node.id, (error as Error).message)
    }
  }

  function callModel(
    node: GraphNode,
    modelId: string,
    inputs: Record<string, unknown>,
  ): Record<string, unknown> {
    if (chain.includes(modelId)) fail(node.id, 'this model uses itself')
    if (chain.length > MAX_DEPTH) fail(node.id, 'models nested too deeply')
    const inner = models[modelId]
    if (!inner) fail(node.id, 'the model this stands for is gone')

    const innerBindings: Record<string, unknown> = {}
    const ins = inputPorts(inner)
    const def = lookupTransform(node.transform, models)!
    def.inputs.forEach((socket, k) => {
      const port = ins[k]
      if (port) innerBindings[portName(port)] = inputs[socket.name]
    })

    const result = evaluateModel(inner, models, innerBindings, chain)
    const outputs: Record<string, unknown> = {}
    outputPorts(inner).forEach((port, k) => {
      const name = def.outputs[k]?.name ?? portName(port)
      const computed = result.values.get(port.id)
      if (computed && 'value' in computed) outputs[name] = computed.value
      else {
        const why = result.errors.get(port.id) ?? 'nothing reached this output'
        fail(node.id, `${name}: ${why}`)
      }
    })
    return outputs
  }

  for (const id of Object.keys(model.nodes)) {
    try {
      compute(id)
    } catch (error) {
      if (!(error instanceof Failure)) throw error
      if (!errors.has(id)) errors.set(id, (error as Error).message)
    }
  }

  return { values, errors }
}

/** An output port's own value is not a socket, so ask for outputs this way. */
export function nodeOutputs(evaluation: Evaluation, nodeId: string): Record<string, unknown> {
  return evaluation.values.get(nodeId) ?? {}
}

export const isPortNode = (transform: string): boolean =>
  isInputPort(transform) || isOutputPort(transform)
