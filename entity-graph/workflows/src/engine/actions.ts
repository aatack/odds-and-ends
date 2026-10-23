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
    description: 'The open items, and those closed in the last day, with what blocks each and whether it is the human\'s turn.',
    input: {},
    run: async (_a, { engine }) =>
      engine.overview().items.map((i) => ({
        id: i.id,
        title: i.title,
        closed: i.closed,
        withHuman: i.withHuman,
        blockers: i.blockers.map((b) => ({ id: b.id, type: b.type, params: b.params })),
        url: i.url,
      })),
  },
  {
    name: 'get_item',
    unsafe: false,
    description: 'An item in full: every event (resolved or not) with its data and timeline, its bot runs, and its trace: every thing that happened to it, when, and by whom.',
    input: { item_id: itemId },
    run: async (a, { engine }) => {
      const d = engine.detail(String(a.item_id))
      if (!d) throw new Error(`No item ${a.item_id}.`)
      return d
    },
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
    name: 'create_item',
    unsafe: false,
    description: 'Create an item, with events to start it off. Answers its id. With no blockers, it is the human\'s turn.',
    input: { title: z.string(), events: z.array(spec).optional() },
    run: async (a, ctx) => {
      const id = await ctx.engine.create(String(a.title), {
        events: a.events as EventSpec[] | undefined,
        actor: ctx.actor,
        author: ctx.author,
        source: ctx.itemId ? `item:${ctx.itemId}` : ctx.actor,
      })
      if (ctx.itemId) ctx.engine.mark(ctx.itemId, 'item_spawned', { actor: ctx.actor, runId: ctx.runId, data: { id, title: a.title } })
      return { id }
    },
  },
  {
    name: 'add_events',
    unsafe: false,
    description: 'Add events to an item: messages, blockers, approvals, afters. Human only.',
    input: { item_id: itemId, events: z.array(spec) },
    run: async (a, ctx) => {
      assertHuman(ctx)
      return { ids: await ctx.engine.addEvents(String(a.item_id), a.events as EventSpec[], ctx.actor) }
    },
  },
  {
    name: 'cancel_event',
    unsafe: false,
    description: 'End a blocker without it happening. Human only.',
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
    description: 'Close an item, cancelling its blockers. Human only.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      assertHuman(ctx)
      await ctx.engine.close(String(a.item_id), { actor: ctx.actor })
      return 'Closed.'
    },
  },
]
