// The actions on items themselves. With the integrations' actions, these are
// the one list both MCP scopes serve and scripts are handed.

import { z } from 'zod'
import type { EventSpec } from '../shared/types'
import type { Action, ActionContext } from './integrations'
import { objectSchema } from './schema'

const itemId = z.string().describe('The id of the item.')
const spec = z
  .object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })
  .describe('An event: see event_types. ref names it, so an after in the same list can wait for it.')

/** Adding events directly, cancelling and closing are the human's: a bot returns its events as output, a script returns them. */
function assertHuman(ctx: ActionContext): void {
  if (ctx.author !== 'human')
    throw new Error('Only a human adds events directly, cancels or closes. A bot returns its events as its output; a script returns them.')
}

export const itemActions: Action[] = [
  {
    name: 'event_types',
    unsafe: false,
    description:
      'Every kind of event: what it is for, whether it blocks, the params it takes (as JSON Schema), and what it resolves with.',
    input: {},
    run: async (_a, { engine }) =>
      engine.eventTypes.map((t) => ({
        type: t.type,
        description: t.description,
        blocking: t.blocking,
        botsMayAdd: Boolean(t.bots),
        params: objectSchema(t.params),
        resolvesWith: t.data,
      })),
  },
  {
    name: 'unsafe_actions',
    unsafe: false,
    description:
      'The unsafe actions: those that change things outside this app, which only a human may run. ' +
      'A bot can not call them; it asks for one by adding an approval event that names it and fills in its inputs.',
    input: {},
    run: async (_a, { engine }) =>
      engine.actions.filter((a) => a.unsafe).map((a) => ({ name: a.name, description: a.description, inputs: objectSchema(a.input) })),
  },
  {
    name: 'list_items',
    unsafe: false,
    description:
      "Every open item, at any depth, and those closed in the last day: whether it is the human's turn, and what blocks it.",
    input: {},
    run: async (_a, { engine }) =>
      engine.overview().items.map((i) => ({
        id: i.id,
        title: i.title,
        parentId: i.parentId,
        closed: i.closed,
        withHuman: i.withHuman,
        blockers: i.blockers.map((b) => ({ id: b.id, type: b.type, summary: b.summary })),
      })),
  },
  {
    name: 'get_item',
    unsafe: false,
    description:
      'An item in full: its children (every event in it, with its data and timeline, child items included), its bot runs, and its ' +
      'trace: every thing that happened in it, when, and by whom.',
    input: { item_id: itemId },
    run: async (a, { engine }) => {
      const d = engine.detail(String(a.item_id))
      if (!d) throw new Error(`No item ${a.item_id}.`)
      return d
    },
  },
  {
    name: 'get_children',
    unsafe: false,
    description: "The events in an item, oldest first. With open, only those still open: the item's blockers.",
    input: { item_id: itemId, open: z.boolean().optional() },
    run: async (a, { engine }) => engine.children(String(a.item_id), Boolean(a.open)),
  },
  {
    name: 'recent_items',
    unsafe: false,
    description:
      'Items touched in the last `days`, for looking back at how the work went: how long each kind of blocker held them, bot runs and cost, ' +
      'human messages, approvals the human changed, cancels, timeouts, bot failures, views and skips, and what was handled outside the tool.',
    input: { days: z.number().default(7) },
    run: async (a, { engine }) => engine.recent(Number(a.days)),
  },
  {
    name: 'run_transcript',
    unsafe: false,
    description: "One bot run's prompt and whole transcript.",
    input: { run_id: z.string() },
    run: async (a, { engine }) => {
      const t = engine.transcript(String(a.run_id))
      if (!t) throw new Error(`No run ${a.run_id}.`)
      return t.length > 200_000 ? `${t.slice(0, 200_000)}\n… (cut)` : t
    },
  },
  {
    name: 'add_events',
    unsafe: false,
    description:
      'Add events in an item: messages, blockers, approvals, afters, child items. With no item_id, add items that sit in none. Human only.',
    input: { item_id: itemId.optional(), events: z.array(spec) },
    run: async (a, ctx) => {
      assertHuman(ctx)
      return { ids: await ctx.engine.addEvents(a.item_id ? String(a.item_id) : null, a.events as EventSpec[], ctx.actor) }
    },
  },
  {
    name: 'cancel_event',
    unsafe: false,
    description: 'End a blocker without it happening (a child item is closed as cancelled). Human only.',
    input: { event_id: z.string() },
    run: async (a, ctx) => {
      assertHuman(ctx)
      await ctx.engine.cancel(String(a.event_id), ctx.actor)
      return 'Cancelled.'
    },
  },
  {
    name: 'close_item',
    unsafe: false,
    description: 'Close an item: its open children are cancelled, child items closed, all the way down. Human only.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      assertHuman(ctx)
      await ctx.engine.close(String(a.item_id), { actor: ctx.actor })
      return 'Closed.'
    },
  },
]
