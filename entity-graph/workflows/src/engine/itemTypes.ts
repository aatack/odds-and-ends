// The item types. What each takes and resolves with is the contract
// docs/items.md sets out; keep the two in step.

import { z } from 'zod'
import type { ItemType } from './integrations'
import { runScript } from './script'
import { slackChannelParams, slackMessageParams } from './slack'

const timeout = { timeout_minutes: z.number().optional().describe('Resolve as timed_out after this long.') }

export const task: ItemType = {
  type: 'task',
  description:
    'A task: a thing to be done. Its items sit in it, other tasks included; a task in another blocks nothing and is not closed with it. ' +
    'Add one to split work off: give it its first items (e.g. a note) in `items`, and who has it in `assignees`. With no ' +
    "assignees, it is its maker's if a person made it, and nobody's otherwise.",
  params: {
    title: z.string(),
    items: z
      .array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() }))
      .optional()
      .describe('Items to put in it as it is created (`items`, the old name, is taken too). They are added in it, and not kept in its values.'),
    key: z.string().optional().describe('Identifies it, so that it is added only once.'),
    detached: z
      .boolean()
      .optional()
      .describe('Put it in no task, rather than the one it is added in: for work unrelated to this task.'),
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

export const note: ItemType = {
  type: 'note',
  description: 'Something written in an item (or in a bot\'s own chat): by a person, a bot, or a script.',
  params: { text: z.string() },
  data: '{}',
  blocking: false,
  bots: true,
  describe: (p) => String(p.text),
}

export const approval: ItemType = {
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
/** What a script's run may return; null (or nothing) is `{}`. */
const scriptResult = z
  .object({
    messages: z.array(z.object({ item: z.string().optional(), text: z.string() })).optional(),
    assign: z.array(z.object({ item: z.string(), to: z.array(z.string()) })).optional(),
    next: z.union([z.number(), z.string(), z.null()]).optional(),
    state: z.unknown().optional(),
    done: z.boolean().optional(),
    data: z.unknown().optional(),
    // The older shape: one message, in the item it sits in.
    message: z.string().optional(),
  })
  .passthrough()
  .nullable()

/** When a script asked to run next: ms since the epoch, an ISO time, or `+30s`, `+5m`, `+2h`, `+1d` from now. */
export function whenNext(next: number | string, now: number): number {
  if (typeof next === 'number') return next
  const rel = /^\+(\d+(?:\.\d+)?)\s*([smhd])$/.exec(next.trim())
  if (rel) return now + Number(rel[1]) * { s: 1_000, m: 60_000, h: 3_600_000, d: 86_400_000 }[rel[2] as 's' | 'm' | 'h' | 'd']
  const at = Date.parse(next)
  if (Number.isNaN(at)) throw new Error(`next must be ms, an ISO time or +<n><s|m|h|d>, not ${JSON.stringify(next)}.`)
  return at
}

export const SCRIPT_DOC =
  'A script: JavaScript the app runs for you, with no model. Use one to own repetitive work that does not need an LLM: ' +
  'snoozing a task, waiting for a branch to merge, checking something every hour. It costs nothing to run.\n' +
  'Two uses:\n' +
  '1. A button: add it in a task owning nothing. It sits in the chat with a play button, and runs only when a person presses it.\n' +
  '2. An owner: give it `owns` (task ids). It has those tasks, as a bot or a person would, and wakes as a bot does: once anything ' +
  '(but itself) happens in them, debounce_minutes (1) after; and every cycle_minutes (15) while it owns them, news or not. Each run ' +
  'checks what it waits for (the time, a branch, a reply); when it has happened, it hands its tasks on and says done.\n' +
  'It never blocks the task it sits in: a task waits on a script by being assigned to it.\n' +
  '`source` is the body of an async function: `context` and `tools` are in scope, and it may await and return.\n' +
  '- context: { script: { id, description, runs, state }, item: { id, title } (the task it sits in), tasks: [{ id, title }] (those it owns), ' +
  "now (ISO time), trigger: 'press' | 'schedule' }. state is what its last run returned as state (null the first time).\n" +
  '- tools.<name>(args) calls any tool the MCP has, and resolves to its result. Unsafe tools (anything acting outside the app) are given only ' +
  'when a person presses play; on its own, never.\n' +
  '- It returns (all optional): { messages: [{ item, text }] (notes, in any tasks; item defaults to the one it sits in), assign: [{ item, to: [who] }] ' +
  "(who has each task from now on: usernames, bot names, or the script's own id to keep it), state (handed to its next run), " +
  'done: true (it closes; tasks it still owns go back to their owners), data }.\n' +
  '- console.log is kept. A run that throws ends it with the error.\n' +
  'Example, a snooze until 9am: params { description: "Snooze until 9am", owns: ["<task id>"], cycle_minutes: 5, source: ' +
  '"if (context.now < \'2026-11-02T09:00:00Z\') return null; return { messages: [{ text: \'Back from snooze\' }], assign: [{ item: context.tasks[0].id, to: [\'alex\'] }], done: true }" }.\n' +
  'Example, a check every ten minutes: cycle_minutes: 10, source: "const pr = await tools.some_read_tool({ ... }); if (!pr.merged) return null; ' +
  'return { messages: [{ text: \'Merged\' }], assign: [{ item: context.tasks[0].id, to: [\'Threepio\'] }], done: true }".'

export const script: ItemType = {
  type: 'script',
  description: SCRIPT_DOC,
  params: {
    source: z.string().describe('The body of an async function, with context and tools in scope; it returns { messages?, assign?, next?, state?, done?, data? }.'),
    description: z.string().optional().describe('What it does, in a line: its card and the task list say this.'),
    owns: z.array(z.string()).optional().describe('Tasks it takes on as it is added: each is assigned to it, as to a bot.'),
    debounce_minutes: z.number().min(0).optional().describe('How long after something happens in its tasks it runs: 1 if left out.'),
    cycle_minutes: z.number().min(0).optional().describe('How often it runs while it owns tasks, news or not: 15 if left out; 0, never.'),
    wake_at: z.union([z.number(), z.string()]).optional().describe('Older scripts: not before this time. Check the time in the script instead.'),
    schedule: z.array(z.number().min(0)).optional().describe('Older scripts: seconds between runs. Use cycle_minutes instead.'),
    ...timeout,
  },
  data: '{ result: the data it finished with, runs, logs } or { error, logs }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.description ?? 'Script'),
  said: (p, d) => (d.error ? `The script failed: ${d.error}` : String(d.message ?? `${p.description ?? 'Script'}: done`)),
  schedule: (p) => (p.schedule as number[] | undefined) ?? null,
  check: async (p, { engine, item, info, memo, setMemo, runs, trigger, pressedBy }) => {
    const pressed = trigger === 'press' && !!pressedBy && engine.isUser(pressedBy)
    const owned = engine.assignedTo(info.id)
    const context = {
      script: { id: info.id, description: p.description ?? null, runs, state: memo ?? null },
      item: { id: item.id, title: item.title },
      tasks: owned.map((id) => ({ id, title: engine.get(id)?.title ?? id })),
      now: new Date(engine.now()).toISOString(),
      trigger,
      // The older names.
      event: { id: info.id, description: p.description ?? null, createdAt: new Date(info.createdAt).toISOString(), runs },
      state: memo ?? null,
    }
    // Unsafe tools only for a person's own press (AGENTS.md, the unsafe rule).
    const author = pressed ? 'human' : 'bot'
    const actor = pressed ? pressedBy! : 'script'
    let ran: { value: unknown; logs: string[] }
    try {
      ran = await runScript(String(p.source), context, (name, args) => engine.callAction(name, args, { engine, author, actor, itemId: item.id, runId: null }))
    } catch (e) {
      return { error: (e as Error).message }
    }
    const parsed = scriptResult.safeParse(ran.value)
    if (!parsed.success)
      return { error: `It returned ${JSON.stringify(ran.value)?.slice(0, 200)}, not { messages?, assign?, next?, state?, done?, data? }.`, logs: ran.logs }
    const r = parsed.data ?? {}
    try {
      for (const m of [...(r.messages ?? []), ...(r.message && !r.done ? [{ text: r.message }] : [])])
        await engine.addItems(m.item ?? item.id, [{ type: 'note', params: { text: m.text } }], 'script', author)
      for (const a of r.assign ?? []) {
        const to = new Set(a.to.map((x) => (x === info.id ? x : engine.resolveAssignee(x))))
        const now = engine.assignees(a.item)
        for (const x of to) if (!now.includes(x)) await engine.assign(a.item, x, true, 'script')
        for (const x of now) if (!to.has(x)) await engine.assign(a.item, x, false, 'script')
      }
      if ('state' in r) setMemo(r.state)
    } catch (e) {
      return { error: (e as Error).message, logs: ran.logs }
    }
    if (r.done) return { result: r.data ?? null, ...(r.message ? { message: r.message } : {}), runs: runs + 1, logs: ran.logs }
    return null
  },
}

export const bot: ItemType = {
  type: 'bot',
  description:
    'A bot: a name, instructions, and how it runs. It sits in no item, and is open while it lasts (cancelling it deletes it). ' +
    'Items are assigned to it (the assign action). It wakes when anything affects it (a task is assigned to it, something new in ' +
    'its tasks or its own chat, its settings change), its debounce after; and every cycle_minutes (15) while it has tasks.',
  params: {
    name: z.string().min(1).describe('What it is called: people @ it by this name.'),
    instructions: z.string().optional().describe('What it is for and how it works: given with every run.'),
    provider: z.string().optional().describe("Who runs it: 'claude' (the default)."),
    model: z.string().optional().describe('The model, as the provider names it (opus, sonnet, haiku or a full id): opus if left out.'),
    debounce_minutes: z.number().min(0).optional().describe('How long to wait after the last thing that happened before it wakes: 1 if left out.'),
    cycle_minutes: z.number().min(0).optional().describe('How often it wakes while it has tasks, news or not: 15 if left out; 0, never.'),
    folder: z.string().optional().describe('Its working folder; its own, under the app\'s, if left out.'),
    session_id: z.string().optional().describe("Its Claude session, set by the app once it first runs. Don't set it."),
  },
  data: '{ cancelled: true }, once deleted',
  blocking: true,
  bots: true,
  describe: (p) => `@${p.name}`,
}

export const SEARCH_DOC =
  "The search page's sections: one list for the whole app (the Tasks view, and every picker: @ in the chat input, and the rest). " +
  'It is kept on the one `search` item, which sits in no task; search_sections reads it, set_search_sections replaces it. Each section, in order:\n' +
  "- { kind: 'builtin', id, label? }: one of the finder's own: viewed (recently viewed), needsYou, withBots, waiting, closed (today).\n" +
  "- { kind: 'query', label, filter, limit? }: a filter, as egdb's find takes it ({ eq: { key, value } }, ne, lt/le/gt/ge, contains, " +
  'contains_ignore_case, prefix, in: { key, values }, is_null, not_null, and: [], or: [], not), over every item\'s values (type, ' +
  'title, text, description, resolved, the rest of its params) and these made from its links: parent (the task it sits in), assignees ' +
  '(who has it: eq matches any one), created_by, created_at. Rows are the items that match, the newest first.\n' +
  "- { kind: 'function', label, source }: JavaScript, run as a script's body is (context and tools in scope, no unsafe tools), with " +
  'context.text the filter typed so far; it returns rows: [{ id, type, left, right? }] (left and right in markdown; id is what Enter opens).\n' +
  'The label heads the section. What is typed filters every section at once; a function runs again as it changes.\n' +
  "Example: [{ kind: 'builtin', id: 'needsYou' }, { kind: 'query', label: 'Open scripts', filter: { and: [{ eq: { key: 'type', value: 'script' } }, " +
  "{ eq: { key: 'resolved', value: false } }] } }]"

export const search: ItemType = {
  type: 'search',
  description: SEARCH_DOC,
  params: { sections: z.array(z.record(z.unknown())).describe('The sections, in order: see the description.') },
  data: '—',
  blocking: false,
  bots: false,
  describe: () => 'Search page sections',
}

export const user: ItemType = {
  type: 'user',
  description:
    'A person who uses the app: its id is their username, which they sign in with. It sits in no item, and is open while it lasts. ' +
    'Items are assigned to people as to bots.',
  params: {
    username: z.string().regex(/^[a-z0-9][a-z0-9_.-]*$/, 'Lower-case letters, digits, dots, dashes and underscores.').describe('Their username: the id of this item.'),
    name: z.string().optional().describe('What they are called, if not their username.'),
  },
  data: '{ cancelled: true }, once removed',
  blocking: true,
  bots: true,
  describe: (p) => String(p.name || p.username),
}

export const WIDGET_DOC =
  'A widget: a small piece of UI with state of its own, shown in the chat (and larger in its view). Use one when a person should see or ' +
  'fill in structured data: a checklist, a form, a table to edit, a dashboard.\n' +
  '- `html` is an HTML fragment (with <script> and <style> in it), run in a sandbox with no way out: no network, no app. It is styled as ' +
  'the app is (its fonts and colours as CSS variables: --ink, --muted, --panel, --bg, --line, --accent), so plain HTML (button, input, ' +
  'table, select) looks native.\n' +
  '- In its scripts: `onState(fn)` calls fn with the state now and whenever it changes (from anywhere); `setState(next)` or ' +
  '`setState(current => next)` changes it (kept on the item: other items, bots and scripts read it as `widget_state`); `getState()`; ' +
  'and helpers that answer HTML strings: `esc(text)`, `pill(text)`, `card(title, bodyHtml)`.\n' +
  '- Update the page in place in onState (set .textContent, .value, .checked) rather than rewriting innerHTML round an input, so a ' +
  'person typing keeps their place.\n' +
  '- `widget_state` is its first state (any JSON). set_value (key widget_state) changes it from outside, and an open widget hears of it at once.\n' +
  'Example: html: \'<button id="b">Add one</button> <span id="n"></span><script>onState(s => n.textContent = s.count); ' +
  'b.onclick = () => setState(s => ({ ...s, count: s.count + 1 }))</script>\', widget_state: { count: 0 }'

export const widget: ItemType = {
  type: 'widget',
  description: WIDGET_DOC,
  params: {
    title: z.string().optional().describe('What it is, in a line.'),
    html: z.string().describe('The HTML fragment, scripts and styles included: see the description.'),
    widget_state: z.unknown().optional().describe('Its state: any JSON. Its first, as it is added.'),
  },
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => String(p.title ?? 'Widget'),
}

/** Retired for now: kept so the process items already stored still read. None can be added (Engine.prepare). */
export const proc: ItemType = {
  type: 'process',
  description: 'Switched off for now: no process can be added.',
  params: { command: z.string(), cwd: z.string().optional(), description: z.string().optional() },
  data: '—',
  blocking: true,
  bots: false,
  describe: (p) => String(p.description || `\`${p.command}\``),
}

/** When, to the minute, from a Slack ts or any time slack.ts reads. */
const at = (when: unknown) => {
  const s = String(when)
  const ms = /^\d{9,10}(\.\d+)?$/.test(s) ? Number(s) * 1000 : Date.parse(s)
  return Number.isNaN(ms) ? s : new Date(ms).toLocaleString([], { dateStyle: 'medium', timeStyle: 'short' })
}

export const slackMessage: ItemType = {
  type: 'slack_message',
  description:
    'A Slack message, pulled into the task: from its link (or its channel and ts). The chat shows who said it and what, read from Slack as it ' +
    "is shown; its view shows the thread it is in. A Slack link in a person's note is pulled in this way by itself.",
  params: slackMessageParams,
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => (p.link ? `Slack message ${p.link}` : `Slack message ${p.ts} in ${p.channel}`),
}

export const slackChannel: ItemType = {
  type: 'slack_channel',
  description: 'A Slack channel over a range of time, pulled into the task: its view shows the messages, with their threads.',
  params: slackChannelParams,
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => `Slack ${p.channel}, ${at(p.oldest)} to ${p.latest ? at(p.latest) : 'now'}`,
}

export const ITEM_TYPES: ItemType[] = [task, note, approval, script, proc, widget, search, slackMessage, slackChannel, bot, user]
