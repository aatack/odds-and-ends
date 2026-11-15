// The actions on items themselves. With the integrations' actions, these are
// the one list both MCP scopes serve and scripts are handed.

import { z } from 'zod'
import { runScript } from './script'
import type { ItemSpec } from '../shared/types'
import type { Action } from './integrations'
import { GUIDES, guide } from './paradigm'
import { objectSchema } from './schema'
import { TUNABLE } from './settings'
import type { Engine } from './engine'

const itemId = z.string().describe('The id of the item.')
const spec = z
  .object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional(), pin: z.boolean().optional().describe('Pin it to the item it is added in: it shows beside that item, as something useful in actioning it. A link, file or Slack message added so sits in no chat, only there; a topic stays in the chat too (a subtask is pinned anyway).') })
  .describe('An item: see item_types. ref names it, so an after in the same list can wait for it.')


/**
 * The tools' names from when items were called items: still answered, as the new ones, so bots in sessions that
 * began before know them, but not listed (mcp.ts). Their old parameters are taken too (LEGACY_PARAMS).
 */
export const LEGACY_NAMES: Record<string, string> = {
  event_types: 'item_types',
  add_events: 'add_items',
  link_events: 'pin',
  link_items: 'pin',
  cancel_event: 'cancel_item',
}

/** Each tool's parameters by their old names (from when items were called items), and what they are called now. */
export const LEGACY_PARAMS: Record<string, Record<string, string>> = {
  add_items: { events: 'items' },
  pin: { event_ids: 'item_ids' },
  cancel_item: { event_id: 'item_id' },
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

/**
 * The settings bots may see and change: TUNABLE (counts), and each integration's setup values that are not secret
 * (text, as `integration.<id>.<key>`). Secrets and how the app is reached are not among them.
 */
function tunable(engine: Engine): { key: string; description: string; value: unknown; default: unknown; set: (v: unknown, by: string) => void }[] {
  return [
    ...TUNABLE.map((t) => ({
      key: t.key,
      description: `${t.description} A whole number, at least 1; null puts the default back.`,
      value: engine.settings.get(t.key, t.default),
      default: t.default,
      set: (v: unknown, by: string) => {
        if (v !== null && !(Number.isInteger(v) && (v as number) >= 1)) throw new Error(`${t.key} is a whole number, at least 1 (or null for the default).`)
        // A stored null would read as 0 (and run nothing), so the default is kept instead.
        engine.settings.set(t.key, v ?? t.default, by)
      },
    })),
    ...engine.integrations.flatMap((i) =>
      i.setup
        .filter((f) => !f.secret)
        .map((f) => ({
          key: `integration.${i.id}.${f.key}`,
          description: `${i.name}: ${f.label}. ${f.instructions} Text; null or "" removes it.`,
          value: engine.settings.integrationValue(i.id, f.key),
          default: null,
          set: (v: unknown, by: string) => {
            if (v !== null && typeof v !== 'string') throw new Error(`integration.${i.id}.${f.key} is text (or null to remove it).`)
            engine.settings.setIntegrationValue(i.id, f.key, v, false, by)
          },
        })),
    ),
  ]
}

export const itemActions: Action[] = [
  {
    name: 'paradigm',
    unsafe: false,
    description:
      'How the app works: read it first. `page` picks the guide: home (the default: topics, assignees, comments, notes, pins, views, bots and scripts), integrations (what the app reaches outside, and its tools) or types (the kinds of item).',
    input: { page: z.enum(GUIDES as [string, ...string[]]).optional() },
    run: async (a) => guide((a.page as (typeof GUIDES)[number] | undefined) ?? 'home', 'agent'),
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
    name: 'list_items',
    unsafe: false,
    description:
      'Every open topic (and anything else someone has), and the topics closed in the last day: who has it and who owns it, whether it is their turn, and what blocks it.',
    input: {},
    run: async (_a, { engine }) => {
      return engine.overview().items.map((i) => ({
        id: i.id,
        title: i.title,
        parentId: i.parentId,
        closed: i.closed,
        assignees: i.assignees,
        owner: i.owner,
        withYou: i.withYou,
        blockers: i.blockers.map((b) => ({ id: b.id, type: b.type, summary: b.summary })),
      }))
    },
  },
  {
    name: 'items_waiting_on',
    unsafe: false,
    description:
      "The open items waiting on one thing: a person (by username: their turn, so assigned to them and blocked by nothing), " +
      '`bot` (assigned to any bot), a bot by its name, or any blocking item type (`script`, `item` for ' +
      "a child item: see item_types). Each with the blockers that match, longest-waiting first.",
    input: { on: z.string().describe("A person's username, bot, a bot's name, or a blocking item type.") },
    run: async (a, { engine }) => {
      const on = String(a.on)
      const types: string[] = engine.itemTypes.filter((t) => t.blocking).map((t) => t.type)
      const held = on === 'bot' ? null : engine.holder(on)
      const person = held && engine.isUser(held) ? engine.user(held) : null
      if (on !== 'bot' && !held && !types.includes(on))
        throw new Error(`Waiting on what? One of: a person (${engine.users().map((u) => u.id).join(', ')}), bot, a bot's name, ${types.join(', ')}.`)
      const items = engine.overview(person?.id).items.filter((i) => !i.closed)
      const waits = (i: (typeof items)[number]) =>
        person ? i.withYou
        : on === 'bot' ? i.assignees.some((x) => engine.info(x)?.type === 'bot')
        : held ? i.assignees.includes(held)
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
      'Assign an item to a person (by username) or a bot or script (by name or id), or take it off them (on: false): an `assignees` link ' +
      'from the item. Anything open can be assigned. Assigning a bot makes it due on the item at once, even one that has it already. ' +
      'Your updates can also say who has an item, with `assign`.',
    input: { item_id: itemId, to: z.string().describe("A person's username, or a bot's name or id."), on: z.boolean().optional() },
    run: async (a, ctx) => {
      await ctx.engine.assign(String(a.item_id), String(a.to), a.on !== false, ctx.actor, ctx.runId)
      return { assignees: ctx.engine.assignees(String(a.item_id)) }
    },
  },
  {
    name: 'set_owner',
    unsafe: false,
    description:
      'Give an item (a topic, a bot, a script) to an owner: a person (by username) or a bot (by name or id), or to nobody (owner: null). ' +
      'Its owner is who it goes back to when a bot lets go of it, and it is listed under what they own. An `owns` link to it. ' +
      'A script may be owned by an item type instead (owner: "topic", "bot", "file"…): it is then offered in the right pane of every ' +
      'item of that type, to run on it.',
    input: { item_id: itemId, owner: z.string().nullable() },
    run: async (a, ctx) => {
      await ctx.engine.setOwner(String(a.item_id), a.owner === null ? null : String(a.owner), ctx.actor)
      return { owner: ctx.engine.owner(String(a.item_id)) }
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
        cycleMinutes: b.cycleMinutes,
        running: b.runId !== null,
        assigned: b.assigned,
      })),
  },
  {
    name: 'get_item',
    unsafe: false,
    description:
      'An item in full: its values (what set_value set on it, and its params), who owns it and who has it, and for a topic (or a bot, ' +
      'or a script) its comments (`children`: every item in its chat, with its data). For a topic that shows its notes, or anything ' +
      'else, `focus`: the whole of it as text, as its view shows it (a topic\'s notes as an indented tree with ids), cut off at 100 ' +
      'lines with the ids to ask for the rest. And what is pinned to it (`pins`) and where it is pinned (`pinnedIn`), each with its id. ' +
      'With `size`, only the item as text at that size: `pill` (its type, short title and id, as `[id]()` shows it), `card` (all the text ' +
      'it holds, as a chat shows it) or `focus` (its default view: a chat as its messages, the latest 100; notes as nested bullets, the ' +
      'first 100 rows; anything else its values), with ids in a column on the left. With `size: focus`, `limit` says how many, and ' +
      '`offset` skips that many (from the latest messages, or the first rows); what is left out is said, with the offset to ask for it.',
    input: { item_id: itemId, size: z.enum(['pill', 'card', 'focus']).optional(), offset: z.number().int().min(0).optional(), limit: z.number().int().min(1).optional() },
    run: async (a, { engine }) => {
      const id = String(a.item_id)
      const info = engine.info(id)
      if (!info) throw new Error(`No item ${id}.`)
      if (a.size) return { text: engine.view(id, a.size as 'pill' | 'card' | 'focus', { offset: a.offset as number | undefined, limit: a.limit as number | undefined }) }
      const d = engine.detail(id)
      const brief = (ids: string[]) => ids.map((i) => engine.info(i)).filter((i) => !!i).map((i) => ({ id: i!.id, type: i!.type, summary: i!.summary }))
      const pinned = { pins: brief(info.pins), pinnedIn: brief(info.pinnedIn) }
      const notes = info.type === 'topic' && engine.notes(id).length ? { focus: engine.view(id, 'focus') } : {}
      if (d && info.params.display !== 'notes') return { ...d, values: info.params, ...pinned, ...(engine.notes(id).length ? { notes: engine.notes(id).map((n) => ({ id: n.id, type: n.type, summary: n.summary })) } : {}) }
      return { item: d?.item ?? null, parent: d?.parent ?? null, focus: engine.view(id, 'focus'), values: info.params, ...pinned, ...notes }
    },
  },
  {
    name: 'run_script',
    unsafe: false,
    description:
      'Run JavaScript now, once, in the app: `source` is the body of an async function with `context` (what you pass, plus now) and `tools` ' +
      '(every tool here, as tools.<name>(args)) in scope; it may await and return. Answers { result, logs }. ' +
      'For code that should run again, or hold a topic, add a script item instead (see item_types).',
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
    description: "The items in an item's comments (its chat), oldest first; with notes, those in its notes. With open, only the open blockers: what the item waits on.",
    input: { item_id: itemId, open: z.boolean().optional(), notes: z.boolean().optional() },
    run: async (a, { engine }) => (a.notes ? engine.notes(String(a.item_id)) : engine.children(String(a.item_id), Boolean(a.open))),
  },
  {
    name: 'add_items',
    unsafe: false,
    description:
      "Add items in a topic's comments (or in a bot's chat): comments (topics with just text), scripts, open topics to split work off. " +
      'With `via: notes`, in its notes (its tree) instead. With no item_id, add topics, bots, scripts or people that sit in none.',
    input: { item_id: itemId.optional(), items: z.array(spec), via: z.enum(['comments', 'notes']).optional() },
    run: async (a, ctx) => {
      return { ids: await ctx.engine.addItems(a.item_id ? String(a.item_id) : null, a.items as ItemSpec[], ctx.actor, ctx.author, (a.via as 'comments' | 'notes') ?? 'comments') }
    },
  },
  {
    name: 'pin',
    unsafe: false,
    description:
      'Pin items to an item (`pins` links): what is pinned to it shows beside it (cards on the right of a topic), as useful in actioning it: ' +
      'open topics made in it (pinned as they are made), pull requests, preview links, files, the topic it came from. Followed backwards, they say where an item is ' +
      'referenced (get_item: pins and pinnedIn). With unpin, take them off. Pinning moves nothing: each item stays where it sits.',
    input: { item_id: itemId, item_ids: z.array(z.string()), unpin: z.boolean().optional() },
    run: async (a, ctx) => {
      await ctx.engine.pin(String(a.item_id), a.item_ids as string[], !a.unpin, ctx.actor)
      return a.unpin ? 'Unpinned.' : 'Pinned.'
    },
  },
  {
    name: 'move_item',
    unsafe: false,
    description:
      "Move items into another item: into its comments (its chat; the default) or its notes, after what is there; or out of any (to: null). " +
      'Who made each, and when, stay. For a message that went in the wrong topic.',
    input: { item_ids: z.array(z.string()), to: z.string().nullable(), into: z.enum(['comments', 'notes']).optional() },
    run: async (a, ctx) => {
      for (const id of a.item_ids as string[]) await ctx.engine.moveItem(id, a.to as string | null, ctx.actor, (a.into as 'comments' | 'notes' | undefined) ?? 'comments')
      return 'Moved.'
    },
  },
  {
    name: 'settings',
    unsafe: false,
    description:
      "The app's settings that bots may change (with set_setting): each one's key, what it does, its value now and its default. " +
      'Secrets, and how the app is reached (the MCP and phone listeners, the tailnet), are not among them.',
    input: {},
    run: async (_a, { engine }) => tunable(engine).map(({ set: _, ...t }) => t),
  },
  {
    name: 'set_setting',
    unsafe: false,
    description: 'Change one of the settings that `settings` lists (null puts its default back). It takes at once.',
    input: { key: z.string().describe('A key that `settings` lists.'), value: z.unknown() },
    run: async (a, { engine, actor }) => {
      const t = tunable(engine).find((x) => x.key === a.key)
      if (!t) throw new Error(`${String(a.key)} is not a setting bots may change: see settings.`)
      t.set(a.value ?? null, actor)
      const { set: _, ...now } = tunable(engine).find((x) => x.key === a.key)!
      return now
    },
  },
  {
    name: 'set_value',
    unsafe: false,
    description:
      "Set one value on an item (null clears it): any key, any value (a type's params, in item_types, are only advice), e.g. a " +
      "topic's text, open (its status) or display, or a script's state on a topic it holds. Bots are items too: this is how to change a bot's instructions, name or cycle_minutes, keeping its session and topics. " +
      'Not how it stands (type, resolved…: those have their own tools). ' +
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
    description: 'The old name for close_item: close any item (a blocker ends without happening; a bot is deleted).',
    input: { item_id: z.string() },
    run: async (a, ctx) => {
      await ctx.engine.cancel(String(a.item_id), ctx.actor)
      return 'Cancelled.'
    },
  },
  {
    name: 'reopen_item',
    unsafe: false,
    description: 'Open a closed item again (its status open): its owner and assignees come back as they were when it closed. What its closing cancelled stays cancelled. A cancelled script or bot comes back too, holding nothing.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      await ctx.engine.reopen(String(a.item_id), ctx.actor)
      return 'Reopened.'
    },
  },
  {
    name: 'close_item',
    unsafe: false,
    description: 'Close any item (its status closed): its owner and assignees are taken off it, and come back if it is reopened. A topic\'s open blockers are cancelled (topics in it are left open); a bot is deleted; a script cancelled.',
    input: { item_id: itemId },
    run: async (a, ctx) => {
      await ctx.engine.close(String(a.item_id), { actor: ctx.actor })
      return 'Closed.'
    },
  },
]
