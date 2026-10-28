// JSON Schema from the zod shapes: for the forms, the docs, and the bots'
// structured output.

import { z } from 'zod'
import { zodToJsonSchema } from 'zod-to-json-schema'
import type { ObjectSchema } from '../shared/types'
import type { ItemType } from './integrations'

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

/** One item of any type a bot may add, as the bots return them. */
export function itemSchema(types: ItemType[]): Record<string, unknown> {
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
          ref: { type: 'string', description: 'A name for this item, so another in the same list can refer to it.' },
        },
        required: ['type', 'params'],
        additionalProperties: false,
      })),
  }
}

/** What a bot's run must return: an update for each item it did something in. */
export function botOutputSchema(types: ItemType[]): Record<string, unknown> {
  return {
    type: 'object',
    properties: {
      updates: {
        type: 'array',
        description:
          'One for each item you have, and one for any other you want to say something in (its message is a note there; who has it is left alone unless you say). An item you have and say nothing of goes back ' +
          'to the person who last had it, unless you added a wait to it.',
        items: {
          type: 'object',
          properties: {
            item: { type: 'string', description: "The item's id." },
            message: {
              type: 'string',
              description:
                'To the people in the item: what you did, in a line or two, or one direct question. No preamble and no waffle. ' +
                'Leave it out if there is nothing to say.',
            },
            items: {
              type: 'array',
              description: 'Items to add in it: waits, approvals, afters, child items.',
              items: itemSchema(types),
            },
            assign: {
              type: 'array',
              items: { type: 'string' },
              description:
                'Who has the item from now on: people by username, bots by name; yourself to keep it. Left out, the item goes back ' +
                'to the person who last had it, unless you added a wait to it: then you keep it.',
            },
            close: { type: 'boolean', description: 'Close the task. Only when someone has explicitly asked you to close it.' },
          },
          required: ['item'],
          additionalProperties: false,
        },
      },
      handoff: {
        type: 'string',
        description:
          "Optional. Only if you're done for now and nothing in this session needs carrying over: a few lines for your next run, which then " +
          'starts a fresh session with them (and every task in full as it next moves). Leave it out to carry on in this session.',
      },
    },
    additionalProperties: false,
  }
}
