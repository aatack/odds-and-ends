// The actions on items themselves. With the integrations' actions, these are
// the one list both MCP scopes serve and scripts are handed.

import { z } from 'zod'
import type { EventSpec } from '../shared/types'
import type { Action } from './integrations'
import { PARADIGM } from './paradigm'
import { objectSchema } from './schema'

const itemId = z.string().describe('The id of the item.')
const spec = z
  .object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })
  .describe('An event: see event_types. ref names it, so an after in the same list can wait for it.')


export const itemActions: Action[] = [
  {
    name: 'paradigm',
    unsafe: false,
    description: 'How items, events and blockers work, in a few lines: read it first.',
    input: {},
    run: async () => PARADIGM,
  },
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
    name: 'items_waiting_on',
    unsafe: false,
    description:
      "The open items waiting on one thing: `human` (the human's turn: nothing blocks them but approvals), `bot` (a bot is " +
      'working on them or will be), or any blocking event type (`github`, `slack`, `script`, `after`, `approval`, `item` for a ' +
      "child item, …: see event_types). Each with the blockers that match, longest-waiting first.",
    input: { on: z.string().describe('human, bot, or a blocking event type.') },
    run: async (a, { engine }) => {
      const on = String(a.on)
      const types: string[] = engine.eventTypes.filter((t) => t.blocking).map((t) => t.type)
      if (on !== 'human' && !types.includes(on)) throw new Error(`Waiting on what? One of: human, ${types.join(', ')}.`)
      return engine
        .overview()
        .items.filter((i) => !i.closed && (on === 'human' ? i.withHuman : i.blockers.some((b) => b.type === on)))
        .map((i) => ({
          id: i.id,
          title: i.title,
          parentId: i.parentId,
          since: new Date(i.since).toISOString(),
          blockers: i.blockers.filter((b) => on === 'human' || b.type === on).map((b) => ({ id: b.id, type: b.type, summary: b.summary })),
        }))
    },
  },
  {
    name: 'next_for_human',
    unsafe: false,
    description:
      "The item the human's Next button would show them now: the one that has been their turn longest. Null when nothing needs them.",
    input: {},
    run: async (_a, { engine }) => {
      const id = engine.next()
      const item = id ? engine.overview().items.find((i) => i.id === id) : null
      return item ? { id: item.id, title: item.title, since: new Date(item.since).toISOString() } : null
    },
  },
  {
    name: 'custom_actions',
    unsafe: false,
    description:
      "The human's own actions, defined per event type on the Types page: each a script over the safe actions. With type, only " +
      'those for that event type. Run one with run_custom_action.',
    input: { type: z.string().optional().describe('An event type, e.g. item.') },
    run: async (a, { engine }) =>
      engine.customActions
        .list()
        .filter((c) => !a.type || c.type === a.type)
        .map(({ id, type, name, description, important }) => ({ id, type, name, description, important })),
  },
  {
    name: 'run_custom_action',
    unsafe: false,
    description: 'Run one of the custom actions on an event of its type: what it says is added in the item, as an action event.',
    input: { action_id: z.string().describe('From custom_actions.'), event_id: z.string().describe('The event (an item, or one in it) to run it on.') },
    run: async (a, ctx) => ctx.engine.customActions.run(String(a.action_id), String(a.event_id), ctx.actor, ctx.runId),
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
      'Add events in an item (your own or any other): messages, blockers, approvals, afters, child items. With no item_id, add items that sit in none.',
    input: { item_id: itemId.optional(), events: z.array(spec) },
    run: async (a, ctx) => {
      return { ids: await ctx.engine.addEvents(a.item_id ? String(a.item_id) : null, a.events as EventSpec[], ctx.actor, ctx.author, ctx.runId) }
    },
  },
  {
    name: 'attach_event',
    unsafe: false,
    description:
      'Attach any event (an item, a message, anything) to an item, to gather it there: an attachment event in the item points at it. ' +
      'The event itself stays where it was.',
    input: { event_id: z.string().describe('The event to attach.'), item_id: itemId.describe('The item to attach it to.') },
    run: async (a, ctx) => ({ id: await ctx.engine.attach(String(a.event_id), String(a.item_id), ctx.actor, ctx.author, ctx.runId) }),
  },
  {
    name: 'cancel_event',
    unsafe: false,
    description: 'End a blocker without it happening (a child item is closed as cancelled).',
    input: { event_id: z.string() },
    run: async (a, ctx) => {
      await ctx.engine.cancel(String(a.event_id), ctx.actor)
      return 'Cancelled.'
    },
  },
  {
    name: 'close_item',
    unsafe: false,
    description: 'Close an item: its open children are cancelled, child items closed, all the way down.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      await ctx.engine.close(String(a.item_id), { actor: ctx.actor })
      return 'Closed.'
    },
  },
]
