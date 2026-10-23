// JSON Schema from the zod shapes: for the forms, the docs, and the bots'
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

/** One handoff of any type, as the bots return them. */
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

/** What a bot's run must return. */
export function botOutputSchema(types: HandoffType[]): Record<string, unknown> {
  return {
    type: 'object',
    properties: {
      next: {
        type: 'array',
        description:
          'What happens next, in order: the first is what the item waits for now, and each resolves into the one after. ' +
          'Empty only with close.',
        items: handoffSchema(types),
      },
      close: { type: 'boolean', description: 'The item is done. next must then be empty.' },
    },
    required: ['next'],
    additionalProperties: false,
  }
}
