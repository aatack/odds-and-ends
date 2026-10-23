// The actions on items themselves. With the integrations' actions, these are
// the one list both MCP scopes serve and scripts are handed.

import { z } from 'zod'
import type { HandoffSpec } from '../shared/types'
import type { Action, ActionContext } from './integrations'
import { objectSchema } from './schema'

const itemId = z.string().describe('The id of the item.')
const spec = z.object({ type: z.string(), params: z.record(z.unknown()) }).describe('A handoff: see handoff_types.')

/** An agent, or a script an agent wrote, may change only the item it works on. */
export function assertMayChange(ctx: ActionContext, id: string): void {
  if (ctx.author === 'agent' && ctx.itemId !== id) throw new Error('An agent may only change the item it is working on.')
}

/** Handing on and closing are the human's: an agent or a script returns its handoffs instead. */
function assertHuman(ctx: ActionContext): void {
  if (ctx.author !== 'human')
    throw new Error('Only a human hands an item on or closes it directly. An agent returns its next handoffs as its output; a script returns them.')
}

export const itemActions: Action[] = [
  {
    name: 'handoff_types',
    description: 'Every kind of handoff: what it is for, the params it takes (as JSON Schema), and what it resolves with.',
    input: {},
    run: async (_a, { engine }) =>
      engine.handoffTypes.map((t) => ({ type: t.type, description: t.description, params: objectSchema(t.params), resolution: t.resolution })),
  },
  {
    name: 'list_items',
    description: 'The open items, and those closed in the last day, with what each waits for.',
    input: { type: z.string().optional().describe('Only items whose current handoff is of this type.') },
    run: async (a, { engine }) =>
      engine
        .overview()
        .items.filter((i) => !a.type || i.current?.type === a.type)
        .map((i) => ({ id: i.id, title: i.title, state: i.state, closed: i.closed, current: i.current && { type: i.current.type, params: i.current.params }, url: i.url })),
  },
  {
    name: 'get_item',
    description: 'An item in full: its state, every handoff with its resolution, the events between them, and its agent runs.',
    input: { item_id: itemId },
    run: async (a, { engine }) => {
      const d = engine.detail(String(a.item_id))
      if (!d) throw new Error(`No item ${a.item_id}.`)
      return d
    },
  },
  {
    name: 'recent_items',
    description:
      'Items touched in the last `days`, for looking back at how the work went: time spent on each kind of handoff, agent runs and cost, ' +
      'human decisions, approvals the human changed, waits that timed out, and what was handled outside the tool.',
    input: { days: z.number().default(7) },
    run: async (a, { engine }) => engine.recent(Number(a.days)),
  },
  {
    name: 'run_transcript',
    description: "One agent run's prompt and whole transcript.",
    input: { run_id: z.string() },
    run: async (a, { engine }) => {
      const t = engine.transcript(String(a.run_id))
      if (!t) throw new Error(`No run ${a.run_id}.`)
      return t.length > 200_000 ? `${t.slice(0, 200_000)}\n… (cut)` : t
    },
  },
  {
    name: 'create_item',
    description: "Create an item. Answers its id. Without handoffs it goes to the human to decide what happens.",
    input: {
      title: z.string(),
      state: z.string().optional(),
      next: z.array(spec).optional().describe('Its handoffs, in order.'),
    },
    run: async (a, ctx) => {
      const id = await ctx.engine.create(String(a.title), {
        state: a.state as string | undefined,
        next: a.next as HandoffSpec[] | undefined,
        actor: ctx.actor,
        author: ctx.author,
        source: ctx.itemId ? `item:${ctx.itemId}` : ctx.actor,
      })
      if (ctx.itemId) ctx.engine.note(ctx.itemId, `Created item ${id}: ${a.title}`, ctx.actor)
      return { id }
    },
  },
  {
    name: 'set_state',
    description: "Replace an item's state: where it is now, in a few lines.",
    input: { item_id: itemId, state: z.string() },
    run: async (a, ctx) => {
      assertMayChange(ctx, String(a.item_id))
      ctx.engine.setState(String(a.item_id), String(a.state), ctx.actor)
      return 'Recorded.'
    },
  },
  {
    name: 'note',
    description: "Add a line to an item's history: something done or found worth keeping.",
    input: { item_id: itemId, text: z.string() },
    run: async (a, ctx) => {
      assertMayChange(ctx, String(a.item_id))
      ctx.engine.note(String(a.item_id), String(a.text), ctx.actor)
      return 'Recorded.'
    },
  },
  {
    name: 'hand_off',
    description:
      "Resolve an item's current handoff and put `next` at the front of its queue (or in place of it, with replace_queue). Human only.",
    input: {
      item_id: itemId,
      next: z.array(spec).describe('The handoffs, in order.'),
      state: z.string().optional(),
      note: z.string().optional(),
      replace_queue: z.boolean().optional().describe('Drop the handoffs already queued.'),
    },
    run: async (a, ctx) => {
      assertHuman(ctx)
      await ctx.engine.handOff(String(a.item_id), {
        next: a.next as HandoffSpec[],
        state: a.state as string | undefined,
        note: a.note as string | undefined,
        replaceQueue: Boolean(a.replace_queue),
        actor: ctx.actor,
      })
      return 'Handed off.'
    },
  },
  {
    name: 'close_item',
    description: 'Close an item: resolve its current handoff and drop the rest. Human only.',
    input: { item_id: itemId, state: z.string().optional(), note: z.string().optional() },
    run: async (a, ctx) => {
      assertHuman(ctx)
      await ctx.engine.close(String(a.item_id), { state: a.state as string | undefined, note: a.note as string | undefined, actor: ctx.actor })
      return 'Closed.'
    },
  },
]
