// The event types. What each takes and resolves with is the contract
// docs/events.md sets out; keep the two in step.

import { homedir } from 'node:os'
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
  'Three uses:\n' +
  '1. A button: add it in a task with no wake_at. It sits in the chat with a play button, and runs when a person presses it.\n' +
  '2. An owner on a timer (a snooze): give it wake_at and `owns`. It has those tasks, as a bot or a person would, until it hands them on.\n' +
  '3. A check: an owner that runs again and again (return next each time) until what it waits for happens, then hands its tasks on and says done.\n' +
  'It never blocks the task it sits in: a task waits on a script by being assigned to it.\n' +
  '`source` is the body of an async function: `context` and `tools` are in scope, and it may await and return.\n' +
  '- context: { script: { id, description, runs, state }, item: { id, title } (the task it sits in), tasks: [{ id, title }] (those it owns), ' +
  "now (ISO time), trigger: 'press' | 'schedule' }. state is what its last run returned as state (null the first time).\n" +
  '- tools.<name>(args) calls any tool the MCP has, and resolves to its result. Unsafe tools (anything acting outside the app) are given only ' +
  'when a person presses play; on a schedule, never.\n' +
  '- It returns (all optional): { messages: [{ item, text }] (notes, in any tasks; item defaults to the one it sits in), assign: [{ item, to: [who] }] ' +
  '(who has each task from now on: usernames, bot names, or the script\'s own id to keep it), next (when to run again: ms, an ISO time, or "+5m"; ' +
  'left out, it waits for a press), state (handed to its next run), done: true (it closes; tasks it still owns go back to their owners), data }.\n' +
  '- console.log is kept. A run that throws ends it with the error.\n' +
  'Example, a snooze until 9am: params { description: "Snooze until 9am", wake_at: "2026-11-02T09:00:00Z", owns: ["<task id>"], source: ' +
  '"return { messages: [{ text: \'Back from snooze\' }], assign: [{ item: context.tasks[0].id, to: [\'alex\'] }], done: true }" }.\n' +
  'Example, a check every ten minutes: source: "const pr = await tools.some_read_tool({ ... }); if (!pr.merged) return { next: \'+10m\' }; ' +
  'return { messages: [{ text: \'Merged\' }], assign: [{ item: context.tasks[0].id, to: [\'Threepio\'] }], done: true }".'

export const script: EventType = {
  type: 'script',
  description: SCRIPT_DOC,
  params: {
    source: z.string().describe('The body of an async function, with context and tools in scope; it returns { messages?, assign?, next?, state?, done?, data? }.'),
    description: z.string().optional().describe('What it does, in a line: its card and the task list say this.'),
    wake_at: z
      .union([z.number(), z.string()])
      .optional()
      .describe('When it first runs (ms, an ISO time, or "+5m"); left out, it runs when a person presses play.'),
    owns: z.array(z.string()).optional().describe('Tasks it takes on as it is added: each is assigned to it, as to a bot.'),
    schedule: z.array(z.number().min(0)).optional().describe('Older scripts: seconds between runs, the last repeating. Use wake_at and next instead.'),
    ...timeout,
  },
  data: '{ result: the data it finished with, runs, logs } or { error, logs }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.description ?? 'Script'),
  said: (p, d) => (d.error ? `The script failed: ${d.error}` : String(d.message ?? `${p.description ?? 'Script'}: done`)),
  schedule: (p) => (p.schedule as number[] | undefined) ?? null,
  check: async (p, { engine, item, event, memo, setMemo, runs, trigger, pressedBy, setNext }) => {
    const pressed = trigger === 'press' && !!pressedBy && engine.isUser(pressedBy)
    const owned = engine.assignedTo(event.id)
    const context = {
      script: { id: event.id, description: p.description ?? null, runs, state: memo ?? null },
      item: { id: item.id, title: item.title },
      tasks: owned.map((id) => ({ id, title: engine.get(id)?.title ?? id })),
      now: new Date(engine.now()).toISOString(),
      trigger,
      // The older names.
      event: { id: event.id, description: p.description ?? null, createdAt: new Date(event.createdAt).toISOString(), runs },
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
        await engine.addEvents(m.item ?? item.id, [{ type: 'note', params: { text: m.text } }], 'script', author)
      for (const a of r.assign ?? []) {
        const to = new Set(a.to.map((x) => (x === event.id ? x : engine.resolveAssignee(x))))
        const now = engine.assignees(a.item)
        for (const x of to) if (!now.includes(x)) await engine.assign(a.item, x, true, 'script')
        for (const x of now) if (!to.has(x)) await engine.assign(a.item, x, false, 'script')
      }
      if ('state' in r) setMemo(r.state)
      if (r.next !== undefined) setNext(r.next === null ? null : whenNext(r.next, engine.now()))
      else if (!p.schedule) setNext(null)
    } catch (e) {
      return { error: (e as Error).message, logs: ran.logs }
    }
    if (r.done) return { result: r.data ?? null, ...(r.message ? { message: r.message } : {}), runs: runs + 1, logs: ran.logs }
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

export const proc: EventType = {
  type: 'process',
  description:
    'A command the app keeps running (a dev server, a watcher) until a person or a bot stops it (cancel_event), or it exits; it blocks until then. ' +
    'It starts at once, in a shell, in `cwd` (by default the folder of the bot that added it, or the home folder). Its view shows the end of its ' +
    'output and a Stop button: hand the task to a person to switch it off. It stops when its task closes, and when the app stops.',
  params: {
    command: z.string().describe('What to run, as a bash command line.'),
    cwd: z.string().optional().describe('Where to run it: an absolute path.'),
    description: z.string().optional().describe('What it is, in a line: the blocker row says this.'),
  },
  data: '{ exitCode, signal, error?, output } once it exits (the last lines it wrote); { cancelled: true } once stopped',
  blocking: true,
  bots: true,
  describe: (p) => String(p.description || `Running \`${p.command}\``),
  said: (p, d) => `${p.description || `\`${p.command}\``} ${d.error ? `failed: ${d.error}` : d.signal ? `ended (${d.signal})` : `exited with ${d.exitCode ?? 'nothing'}`}`,
  // Checked often: the first check starts it, and each after says whether it has ended.
  schedule: () => [0, 1, 2],
  check: async (p, ctx) => {
    const id = ctx.event.id
    const s = ctx.engine.processes.status(id)
    if (!s) {
      // Started before, by a backend that has since stopped: it stopped with it.
      if (ctx.memo) return { exitCode: null, signal: null, error: 'The app stopped while it ran.', output: [] }
      const by = ctx.event.createdBy.startsWith('bot:') ? ctx.engine.agent(ctx.event.createdBy.slice(4)) : null
      ctx.engine.processes.start(id, String(p.command), String(p.cwd || by?.folder || homedir()))
      ctx.setMemo({ started: true })
      return null
    }
    if (s.running) return null
    return { exitCode: s.exitCode, signal: s.signal, ...(s.error ? { error: s.error } : {}), output: ctx.engine.processes.output(id, 20) }
  },
}

export const EVENT_TYPES: EventType[] = [task, note, approval, script, proc, bot, user]
