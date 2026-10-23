// JSON Schema from the zod shapes: for the forms, the docs, and the agents'
// structured output.

import { z } from 'zod'
import { zodToJsonSchema } from 'zod-to-json-schema'
import type { ObjectSchema } from '../shared/types'
import type { HandoffType } from './integrations'

/** Drops what structured output and the forms don't want. */
function clean(node: unknown): unknown {
  if (Array.isArray(node)) return node.map(clean)
  if (!node || typeof node !== 'object') return node
  return Object.fromEntries(
    Object.entries(node)
      .filter(([k]) => k !== '$schema' && k !== 'default')
      .map(([k, v]) => [k, clean(v)]),
  )
}

export const objectSchema = (shape: z.ZodRawShape): ObjectSchema =>
  clean(zodToJsonSchema(z.object(shape), { $refStrategy: 'none' })) as ObjectSchema

/** One handoff of any type, as the agents return them. */
export function handoffSchema(types: HandoffType[]): Record<string, unknown> {
  return {
    anyOf: types.map((t) => ({
      title: t.type,
      type: 'object',
      description: t.description,
      properties: { type: { type: 'string', enum: [t.type] }, params: objectSchema(t.params) },
      required: ['type', 'params'],
      additionalProperties: false,
    })),
  }
}

/** What an agent's run must return. */
export function agentOutputSchema(types: HandoffType[]): Record<string, unknown> {
  return {
    type: 'object',
    properties: {
      state: {
        type: 'string',
        description: 'Where the item is now, in a few lines: what is done and what is left. Replaces the last state.',
      },
      next: {
        type: 'array',
        description:
          'What happens next, in order: the first is what the item waits for now, and each resolves into the one after. ' +
          'Empty only with close.',
        items: handoffSchema(types),
      },
      close: { type: 'boolean', description: 'The item is done. next must then be empty.' },
    },
    required: ['state', 'next'],
    additionalProperties: false,
  }
}
