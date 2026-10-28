// The actions on items themselves. With the integrations' actions, these are
// the one list both MCP scopes serve and scripts are handed.

import { z } from 'zod'
import { runScript } from './script'
import { SEARCH_DOC } from './itemTypes'
import type { SearchSection } from '../shared/search'
import type { ItemSpec } from '../shared/types'
import type { Action } from './integrations'
import { PARADIGM } from './paradigm'
import { objectSchema } from './schema'

const itemId = z.string().describe('The id of the item.')
const spec = z
  .object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })
  .describe('An item: see item_types. ref names it, so an after in the same list can wait for it.')


/**
 * The tools' names from when items were called items: still answered, as the new ones, so bots in sessions that
 * began before know them, but not listed (mcp.ts). Their old parameters are taken too (LEGACY_PARAMS).
 */
export const LEGACY_NAMES: Record<string, string> = {
  event_types: 'item_types',
  add_events: 'add_items',
  link_events: 'link_items',
  cancel_event: 'cancel_item',
}

/** Each tool's parameters by their old names (from when items were called items), and what they are called now. */
export const LEGACY_PARAMS: Record<string, Record<string, string>> = {
  add_items: { events: 'items' },
  link_items: { event_ids: 'item_ids' },
  cancel_item: { event_id: 'item_id' },
  run_custom_action: { event_id: 'item_id' },
}

/** A call by an old name, or with old parameters, as it is called now. */
export function current(name: string, args: Record<string, unknown>): { name: string; args: Record<string, unknown> } {
  const now = LEGACY_NAMES[name] ?? name
  const out = { ...args }
  for (const [old, to] of Object.entries(LEGACY_PARAMS[now] ?? {}))
    if (old in out) {
      if (!(to in out)) out[to] = out[old]
      delete out[old]
    }
  return { name: now, args: out }
}

export const itemActions: Action[] = [
  {
    name: 'paradigm',
    unsafe: false,
    description: 'How tasks, items and blockers work, in a few lines: read it first.',
    input: {},
    run: async () => PARADIGM,
  },
  {
    name: 'item_types',
    unsafe: false,
    description:
      'Every kind of item: what it is for, whether it blocks, the params it takes (as JSON Schema), and what it resolves with.',
    input: {},
    run: async (_a, { engine }) =>
      engine.itemTypes.map((t) => ({
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
      'A bot can not call them; it asks for one by adding an approval item that names it and fills in its inputs.',
    input: {},
    run: async (_a, { engine }) =>
      engine.actions.filter((a) => a.unsafe).map((a) => ({ name: a.name, description: a.description, inputs: objectSchema(a.input) })),
  },
  {
    name: 'list_items',
    unsafe: false,
    description:
      'Every open item, at any depth, and those closed in the last day: who has it, whether it is their turn, and what blocks it.',
    input: {},
    run: async (_a, { engine }) => {
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
      'approvals), `bot` (assigned to any bot), a bot by its name, or any blocking item type (`script`, `approval`, `item` for ' +
      "a child item: see item_types). Each with the blockers that match, longest-waiting first.",
    input: { on: z.string().describe("A person's username, bot, a bot's name, or a blocking item type.") },
    run: async (a, { engine }) => {
      const on = String(a.on)
      const types: string[] = engine.itemTypes.filter((t) => t.blocking).map((t) => t.type)
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
      'the one most recently touched. Null when nothing needs them.',
    input: { username: z.string().optional() },
    run: async (a, { engine }) => {
      const who = a.username ? engine.resolveAssignee(String(a.username)) : engine.me()
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
    name: 'custom_actions',
    unsafe: false,
    description:
      "The human's own actions, defined per item type on the Types page: each a script over the safe actions. With type, only " +
      'those for that item type. Run one with run_custom_action.',
    input: { type: z.string().optional().describe('An item type, e.g. item.') },
    run: async (a, { engine }) =>
      engine.customActions
        .list()
        .filter((c) => !a.type || c.type === a.type)
        .map(({ id, type, name, description, important }) => ({ id, type, name, description, important })),
  },
  {
    name: 'run_custom_action',
    unsafe: false,
    description: 'Run one of the custom actions on an item of its type: what it says is added in the item, as an action item.',
    input: { action_id: z.string().describe('From custom_actions.'), item_id: z.string().describe('The item (a task, or one in it) to run it on.') },
    run: async (a, ctx) => ctx.engine.customActions.run(String(a.action_id), String(a.item_id), ctx.actor, ctx.runId),
  },
  {
    name: 'get_item',
    unsafe: false,
    description:
      'An item in full: its children (every item in it, with its data and timeline, child items included), its bot runs, and its ' +
      'trace: every thing that happened in it, when, and by whom.',
    input: { item_id: itemId },
    run: async (a, { engine }) => {
      const d = engine.detail(String(a.item_id))
      if (!d) throw new Error(`No item ${a.item_id}.`)
      return d
    },
  },
  {
    name: 'run_script',
    unsafe: false,
    description:
      'Run JavaScript now, once, in the app: `source` is the body of an async function with `context` (what you pass, plus now) and `tools` ' +
      '(every tool here, as tools.<name>(args); never the unsafe ones for an agent) in scope; it may await and return. Answers { result, logs }. ' +
      'For code that should run again, or own a task, add a script item instead (see item_types).',
    input: { source: z.string(), context: z.record(z.unknown()).optional() },
    run: async (a, ctx) => {
      const context = { ...((a.context as Record<string, unknown>) ?? {}), now: new Date(ctx.engine.now()).toISOString(), trigger: ctx.author === 'human' ? 'press' : 'agent' }
      const ran = await runScript(String(a.source), context, (name, args) => ctx.engine.callAction(name, args, { ...ctx, actor: ctx.author === 'human' ? ctx.actor : 'script' }))
      return { result: ran.value, logs: ran.logs }
    },
  },
  {
    name: 'edit_log',
    unsafe: false,
    description:
      'What happened to an item: every value set on it (null: cleared) and every link made or removed to or from it, by anyone, the newest ' +
      "first, as egdb keeps them (`limit`, 10 by default). With no item_id, everything. Narrow by authors (usernames, a bot's actor `bot:<id>`), " +
      'keys (value keys or link types) and a time range (ms). For what someone did, see activity_log.',
    input: {
      item_id: z.string().optional(),
      authors: z.array(z.string()).optional(),
      keys: z.array(z.string()).optional(),
      since: z.number().optional(),
      until: z.number().optional(),
      limit: z.number().int().positive().max(2000).default(10),
    },
    run: async (a, { engine }) => {
      const { item_id, ...rest } = a as { item_id?: string } & Record<string, unknown>
      return engine.store.log({ ...rest, ...(item_id ? { entities: [item_id] } : {}) })
    },
  },
  {
    name: 'activity_log',
    unsafe: false,
    description:
      'What someone did: every value they set and every link they made or removed, on anything, the newest first (`limit`, 10 by default). ' +
      "`who` is a username, or a bot's name or id (its runs' writes). For what happened to an item, see edit_log.",
    input: { who: z.string(), limit: z.number().int().positive().max(2000).default(10), since: z.number().optional() },
    run: async (a, { engine }) => {
      const who = String(a.who).replace(/^@/, '')
      const bot = engine.findAgent(who)
      return engine.store.log({ authors: bot ? [`bot:${bot.id}`] : [who.toLowerCase()], limit: Number(a.limit), ...(a.since ? { since: Number(a.since) } : {}) })
    },
  },
  {
    name: 'get_children',
    unsafe: false,
    description: "The items in an item, oldest first. With open, only those still open: the item's blockers.",
    input: { item_id: itemId, open: z.boolean().optional() },
    run: async (a, { engine }) => engine.children(String(a.item_id), Boolean(a.open)),
  },
  {
    name: 'add_items',
    unsafe: false,
    description:
      'Add items in a task (or in a bot\'s chat): notes, blockers, approvals, child tasks. With no item_id, add tasks, bots or people that sit in none.',
    input: { item_id: itemId.optional(), items: z.array(spec) },
    run: async (a, ctx) => {
      return { ids: await ctx.engine.addItems(a.item_id ? String(a.item_id) : null, a.items as ItemSpec[], ctx.actor, ctx.author) }
    },
  },
  {
    name: 'link_items',
    unsafe: false,
    description:
      'Add `contains` links from a task to items that already exist elsewhere (messages, other tasks, anything): they show in its chat too, ' +
      'and stay where they sit. For traceability: link a new task to what it came from, and what it came from to it.',
    input: { item_id: itemId, item_ids: z.array(z.string()) },
    run: async (a, ctx) => {
      await ctx.engine.link(String(a.item_id), a.item_ids as string[], ctx.actor)
      return 'Linked.'
    },
  },
  {
    name: 'search_sections',
    unsafe: false,
    description: "The search page's sections, as configured, and how they work (one list for the whole app). See set_search_sections.",
    input: {},
    run: async (_a, { engine }) => ({ sections: engine.searchSections(), how: SEARCH_DOC }),
  },
  {
    name: 'set_search_sections',
    unsafe: false,
    description: "Replace the search page's sections (the Tasks view and every picker), or put the defaults back (sections: null). " + SEARCH_DOC,
    input: { sections: z.array(z.record(z.unknown())).nullable() },
    run: async (a, ctx) => {
      await ctx.engine.setSearchSections(a.sections as SearchSection[] | null, ctx.actor)
      return { sections: ctx.engine.searchSections() }
    },
  },
  {
    name: 'set_value',
    unsafe: false,
    description:
      "Set one value on an item (null clears it): any of its type's params (see item_types), e.g. a task's title, a widget's " +
      "widget_state, a bot's instructions or cycle_minutes. Not how it stands (type, resolved…: those have their own tools). " +
      'Whatever shows it is told at once.',
    input: { item_id: itemId, key: z.string(), value: z.unknown() },
    run: async (a, ctx) => {
      await ctx.engine.setValue(String(a.item_id), String(a.key), a.value, ctx.actor)
      return 'Set.'
    },
  },
  {
    name: 'cancel_item',
    unsafe: false,
    description: 'End a blocker without it happening (a task is closed as cancelled).',
    input: { item_id: z.string() },
    run: async (a, ctx) => {
      await ctx.engine.cancel(String(a.item_id), ctx.actor)
      return 'Cancelled.'
    },
  },
  {
    name: 'reopen_item',
    unsafe: false,
    description: 'Open a closed task again: who has it is unchanged, and what its closing cancelled stays cancelled.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      await ctx.engine.reopen(String(a.item_id), ctx.actor)
      return 'Reopened.'
    },
  },
  {
    name: 'close_item',
    unsafe: false,
    description: 'Close an item: its open blockers are cancelled. Tasks in it are left open.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      await ctx.engine.close(String(a.item_id), { actor: ctx.actor })
      return 'Closed.'
    },
  },
]
