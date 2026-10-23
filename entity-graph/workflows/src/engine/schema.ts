// JSON Schema from the zod shapes: for the forms, the docs, and the bots'
// structured output.

import { z } from 'zod'
import { zodToJsonSchema } from 'zod-to-json-schema'
import type { ObjectSchema } from '../shared/types'
import type { EventType } from './integrations'

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

/** One event of any type a bot may add, as the bots return them. */
export function eventSchema(types: EventType[]): Record<string, unknown> {
  return {
    anyOf: types
      .filter((t) => t.bots)
      .map((t) => ({
        title: t.type,
        type: 'object',
        description: t.description,
        properties: {
          type: { type: 'string', enum: [t.type] },
          params: objectSchema(t.params),
          ref: { type: 'string', description: 'A name for this event, so an after in the same list can wait for it.' },
        },
        required: ['type', 'params'],
        additionalProperties: false,
      })),
  }
}

/** What a bot's run must return. */
export function botOutputSchema(types: EventType[]): Record<string, unknown> {
  return {
    type: 'object',
    properties: {
      message: {
        type: 'string',
        description:
          'To the human: what you did, in a line or two, or one direct question. No preamble and no waffle. Leave it out if there is nothing to say.',
      },
      events: {
        type: 'array',
        description:
          'Blockers to add to the item: waits, approvals, more bots, afters. With none, and the item not closed, it is the human\'s turn.',
        items: eventSchema(types),
      },
      close: { type: 'boolean', description: 'The item is done.' },
    },
    additionalProperties: false,
  }
}
