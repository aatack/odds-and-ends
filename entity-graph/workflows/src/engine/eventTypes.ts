// The event types. What each takes and resolves with is the contract
// docs/events.md sets out; keep the two in step.

import { z } from 'zod'
import type { EventType } from './integrations'
import { runScript } from './script'

const timeout = { timeout_minutes: z.number().optional().describe('Resolve as timed_out after this long.') }

export const task: EventType = {
  type: 'task',
  description:
    'A task: a thing to be done. Its events sit in it, child tasks included, and it blocks the task it sits in until it closes. ' +
    'Add one to split work off: give it its first events (e.g. a note) in `events`, and who has it in `assignees`. With no ' +
    "assignees, it is its maker's if a person made it, and nobody's otherwise.",
  params: {
    title: z.string(),
    events: z
      .array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() }))
      .optional()
      .describe('Events to put in it as it is created. They are added in it, and not kept in its values.'),
    key: z.string().optional().describe('Identifies it, so that it is added only once.'),
    detached: z
      .boolean()
      .optional()
      .describe('Put it in no task, rather than the one it is added in: for work unrelated to this task, which should not block it.'),
    assignees: z
      .array(z.string())
      .optional()
      .describe('Who has it from the start: people by username, bots by name or id. Not kept in its values: they are links.'),
  },
  data: '{ how: "closed" | "cancelled" }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.title),
  said: (p, d) => `${p.title}: ${d.how === 'cancelled' ? 'cancelled' : 'closed'}`,
}

export const note: EventType = {
  type: 'note',
  description: 'Something written in an item (or in a bot\'s own chat): by a person, a bot, or a script.',
  params: { text: z.string() },
  data: '{}',
  blocking: false,
  bots: true,
  describe: (p) => String(p.text),
}

export const approval: EventType = {
  type: 'approval',
  description:
    'An unsafe action for the human to approve, change, or turn down. While only approvals are open, the item is the human\'s turn.',
  params: {
    action: z.string().describe('The unsafe action, by name: see unsafe_actions.'),
    inputs: z.record(z.unknown()).describe('Its inputs, filled in as far as you can. The human may change them.'),
    reason: z.string().optional().describe('Why, in a line.'),
  },
  data: '{ ran: true, inputs, changed: string[], output }',
  blocking: true,
  human: true,
  bots: true,
  describe: (p) => String(p.reason ?? `Run ${p.action}?`),
  said: (p, d) => `Ran ${p.action}${(d.changed as string[] | undefined)?.length ? ` (changed ${(d.changed as string[]).join(', ')})` : ''}`,
}

/** What a script's run answers, read loosely: anything but `{ done: true }` means not yet. */
const scriptResult = z.union([
  z.null(),
  z.object({ done: z.literal(false), state: z.unknown().optional() }).passthrough(),
  z.object({ done: z.literal(true), data: z.unknown().optional(), message: z.string().optional() }).passthrough(),
])

export const script: EventType = {
  type: 'script',
  description:
    'A script that runs on a schedule until it says it is done, and blocks until then. Use it to wait for anything the other ' +
    'types do not cover, by checking with the actions. `source` is a JavaScript function expression, ' +
    '`async (ctx, actions) => result`:\n' +
    '- ctx is { item: { id, title }, event: { id, description, createdAt, runs }, now (ISO time), state }: where it runs, how many ' +
    'times it has run before, and the state its last run handed on (null the first time).\n' +
    "- actions.<name>(args) calls any action (see the MCP tools) and resolves to its result; a script a bot wrote gets no unsafe ones.\n" +
    '- It returns null or { done: false, state } to keep waiting (state is handed to the next run), or ' +
    '{ done: true, data, message } to finish: data is what the event resolves with, and message, if given, is what the chat says.\n' +
    '- console.log is kept. A run that throws ends the event with the error.\n' +
    'Example, waiting until a time: async (ctx) => (ctx.now >= "2026-11-01T09:00:00Z" ? { done: true, message: "Time to look again" } : null)\n' +
    'A loop is a script that never says it is done: it runs on its schedule (the last gap repeating) until it is cancelled. ' +
    'To give a bot something every hour, say: schedule [3600], and a source that adds a note each run, e.g. async (ctx, actions) => { await actions.add_events({ item_id: ctx.item.id, ' +
    'events: [{ type: "note", params: { text: "Check the open PRs and fix any failing CI" } }] }); return { done: false } }',
  params: {
    source: z.string().describe('async (ctx, actions) => null | { done: false, state? } | { done: true, data?, message? }'),
    description: z.string().optional().describe('What it waits for, in a line: the blocker row says this.'),
    schedule: z
      .array(z.number().min(0))
      .optional()
      .describe('Seconds to wait before the first run, then between each run, the last repeating; e.g. [1, 5, 10]. Runs are at least a second apart. Default [1, 5, 10, 30, 60].'),
    ...timeout,
  },
  data: '{ result: the data it finished with, message?, runs } or { error, logs }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.description ?? 'Script'),
  said: (p, d) => (d.error ? `The script failed: ${d.error}` : String(d.message ?? `${p.description ?? 'Script'}: done`)),
  schedule: (p) => (p.schedule as number[] | undefined) ?? [1, 5, 10, 30, 60],
  check: async (p, { engine, item, event, author, memo, setMemo, runs }) => {
    const ctx = {
      item: { id: item.id, title: item.title },
      event: { id: event.id, description: p.description ?? null, createdAt: new Date(event.createdAt).toISOString(), runs },
      now: new Date(engine.now()).toISOString(),
      state: memo ?? null,
    }
    let ran: { value: unknown; logs: string[] }
    try {
      ran = await runScript(String(p.source), ctx, (name, args) =>
        engine.callAction(name, args, { engine, author, actor: 'script', itemId: item.id, runId: null }),
      )
    } catch (e) {
      return { error: (e as Error).message }
    }
    const parsed = scriptResult.safeParse(ran.value)
    if (!parsed.success) return { error: `It returned ${JSON.stringify(ran.value)?.slice(0, 200)}, not null, { done: false } or { done: true, data }.`, logs: ran.logs }
    const result = parsed.data
    if (result?.done) return { result: result.data ?? null, ...(result.message ? { message: result.message } : {}), runs: runs + 1, logs: ran.logs }
    if (result && 'state' in result) setMemo(result.state)
    return null
  },
}

export const bot: EventType = {
  type: 'bot',
  description:
    'A bot: a name, instructions, and how it runs. It sits in no item, and is open while it lasts (cancelling it deletes it). ' +
    'Items are assigned to it (the assign action). It wakes when something happens to it: an item is assigned to it, its ' +
    'settings change, or someone writes in its own chat (notes in it); and every 15 minutes while it has items that something ' +
    'happened in.',
  params: {
    name: z.string().min(1).describe('What it is called: people @ it by this name.'),
    instructions: z.string().optional().describe('What it is for and how it works: given with every run.'),
    provider: z.string().optional().describe("Who runs it: 'claude' (the default)."),
    model: z.string().optional().describe('The model, as the provider names it (opus, sonnet, haiku or a full id): opus if left out.'),
    debounce_minutes: z.number().min(0).optional().describe('How long to wait after the last thing that happened before it wakes: 1 if left out.'),
    folder: z.string().optional().describe('Its working folder; its own, under the app\'s, if left out.'),
    session_id: z.string().optional().describe("Its Claude session, set by the app once it first runs. Don't set it."),
  },
  data: '{ cancelled: true }, once deleted',
  blocking: true,
  bots: true,
  describe: (p) => `@${p.name}`,
}

export const user: EventType = {
  type: 'user',
  description:
    'A person who uses the app: its id is their username, which they sign in with. It sits in no item, and is open while it lasts. ' +
    'Items are assigned to people as to bots.',
  params: {
    username: z.string().regex(/^[a-z0-9][a-z0-9_.-]*$/, 'Lower-case letters, digits, dots, dashes and underscores.').describe('Their username: the id of this event.'),
    name: z.string().optional().describe('What they are called, if not their username.'),
  },
  data: '{ cancelled: true }, once removed',
  blocking: true,
  bots: true,
  describe: (p) => String(p.name || p.username),
}

export const EVENT_TYPES: EventType[] = [task, note, approval, script, bot, user]
