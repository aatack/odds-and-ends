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
      "Every open item, at any depth, and those closed in the last day: whether it is the human's turn, and what blocks it. " +
      'With type, the open events of that type instead, the newest first, with their params and age: e.g. directive.',
    input: { type: z.string().optional().describe('An event type (see event_types); item by default.') },
    run: async (a, { engine }) => {
      if (a.type && a.type !== 'item') {
        engine.type(String(a.type))
        return engine.openOfType(String(a.type)).map((e) => ({
          id: e.id,
          type: e.type,
          parentId: e.parentId,
          summary: e.summary,
          params: e.params,
          createdBy: e.createdBy,
          createdAt: new Date(e.createdAt).toISOString(),
          ageHours: Math.round(((engine.now() - e.createdAt) / 3_600_000) * 10) / 10,
        }))
      }
      return engine.overview().items.map((i) => ({
        id: i.id,
        title: i.title,
        parentId: i.parentId,
        closed: i.closed,
        assignees: i.assignees,
        withYou: i.withYou,
        blockers: i.blockers.map((b) => ({ id: b.id, type: b.type, summary: b.summary })),
      }))
    },
  },
  {
    name: 'items_waiting_on',
    unsafe: false,
    description:
      "The open items waiting on one thing: a person (by username: their turn, so assigned to them and blocked by nothing but " +
      'approvals), `bot` (assigned to any bot), a bot by its name, or any blocking event type (`github`, `slack`, `script`, ' +
      "`after`, `approval`, `item` for a child item, …: see event_types). Each with the blockers that match, longest-waiting first.",
    input: { on: z.string().describe("A person's username, bot, a bot's name, or a blocking event type.") },
    run: async (a, { engine }) => {
      const on = String(a.on)
      const types: string[] = engine.eventTypes.filter((t) => t.blocking).map((t) => t.type)
      const person = engine.user(on.toLowerCase())
      const agent = on === 'bot' || person ? null : engine.findAgent(on)
      if (!person && on !== 'bot' && !agent && !types.includes(on))
        throw new Error(`Waiting on what? One of: a person (${engine.users().map((u) => u.id).join(', ')}), bot, a bot's name, ${types.join(', ')}.`)
      const items = engine.overview(person?.id).items.filter((i) => !i.closed)
      const waits = (i: (typeof items)[number]) =>
        person ? i.withYou
        : on === 'bot' ? i.assignees.some((x) => !engine.isUser(x))
        : agent ? i.assignees.includes(agent.id)
        : i.blockers.some((b) => b.type === on)
      return items.filter(waits).map((i) => ({
        id: i.id,
        title: i.title,
        parentId: i.parentId,
        since: new Date(i.since).toISOString(),
        assignees: i.assignees,
        blockers: i.blockers.filter((b) => !types.includes(on) || b.type === on).map((b) => ({ id: b.id, type: b.type, summary: b.summary })),
      }))
    },
  },
  {
    name: 'next_for_human',
    unsafe: false,
    description:
      "The item a person's Next would show them now (the default person's, without `username`): their priority items first, then " +
      'the one least recently touched. Null when nothing needs them.',
    input: { username: z.string().optional() },
    run: async (a, { engine }) => {
      const who = a.username ? engine.signIn(String(a.username)).id : engine.me()
      const id = engine.next([], who)
      const item = id ? engine.get(id, who) : null
      return item ? { id: item.id, title: item.title, since: new Date(item.since).toISOString() } : null
    },
  },
  {
    name: 'assign',
    unsafe: false,
    description:
      'Assign an item to a person (by username) or a bot (by name or id), or take it off them (on: false): an `assigned_to` link from ' +
      'the item. Assigning a bot wakes it (its debounce after), even one that has the item already. Your updates can also say who ' +
      'has an item, with `assign`.',
    input: { item_id: itemId, to: z.string().describe("A person's username, or a bot's name or id."), on: z.boolean().optional() },
    run: async (a, ctx) => {
      await ctx.engine.assign(String(a.item_id), String(a.to), a.on !== false, ctx.actor, ctx.runId)
      return { assignees: ctx.engine.assignees(String(a.item_id)) }
    },
  },
  {
    name: 'list_people',
    unsafe: false,
    description: 'The people who use the app: each with their username (assign or @ them by that) and name, and the open items they have.',
    input: {},
    run: async (_a, { engine }) => engine.users().map((u) => ({ ...u, assigned: engine.assignedTo(u.id) })),
  },
  {
    name: 'list_bots',
    unsafe: false,
    description:
      'The bots: each with its name (assign or @ it by that), what it is for, whether it is running, and the open items assigned to it.',
    input: {},
    run: async (_a, { engine }) =>
      engine.agents().map((b) => ({
        id: b.id,
        name: b.name,
        instructions: b.instructions,
        provider: b.provider,
        model: b.model || 'default',
        debounceMinutes: b.debounceMinutes,
        running: b.runId !== null,
        assigned: b.assigned,
      })),
  },
  {
    name: 'recently_assigned',
    unsafe: false,
    description: "Assignments and their ends, the newest first, over the last `days`: to one assignee (a username, or a bot's name), or to anyone.",
    input: { to: z.string().optional().describe("A username or a bot's name; anyone if left out."), days: z.number().default(7) },
    run: async (a, { engine }) =>
      engine.assignments(a.to ? engine.resolveAssignee(String(a.to)) : null, Number(a.days)).map((x) => ({ ...x, at: new Date(x.at).toISOString() })),
  },
  {
    name: 'stale_items',
    unsafe: false,
    description:
      'The open items a person (the default person, without `username`) made or was assigned at some time, in which nothing has ' +
      'happened for `hours`: those that may have been lost track of, the longest untouched first.',
    input: { hours: z.number().default(24), username: z.string().optional() },
    run: async (a, { engine }) =>
      engine
        .stale(Number(a.hours), a.username ? engine.signIn(String(a.username)).id : engine.me())
        .map((i) => ({ id: i.id, title: i.title, assignees: i.assignees, since: new Date(i.since).toISOString() })),
  },
  {
    name: 'directives',
    unsafe: false,
    description:
      "The operator's directives: short markdown notes on what they are working on at the moment, the newest first, with each one's " +
      'age. They are not in your prompt: read the latest for the broad picture when an item needs more context. ' +
      'The same as list_items with type directive. Add one as a directive event, with add_events.',
    input: {},
    run: (_a, ctx) => itemActions.find((x) => x.name === 'list_items')!.run({ type: 'directive' }, ctx),
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
      const t = await engine.transcript(String(a.run_id))
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
      await ctx.engine.cancel(String(a.event_id), ctx.actor, ctx.runId)
      return 'Cancelled.'
    },
  },
  {
    name: 'close_item',
    unsafe: false,
    description: 'Close an item: its open children are cancelled, child items closed, all the way down.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      await ctx.engine.close(String(a.item_id), { actor: ctx.actor, runId: ctx.runId ?? undefined })
      return 'Closed.'
    },
  },
]
