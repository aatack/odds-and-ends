// The event types. What each takes and resolves with is the contract
// docs/events.md sets out; keep the two in step.

import { z } from 'zod'
import type { EventSpec } from '../shared/types'
import { checkPr, githubEventParams, startPr } from './github'
import type { EventType } from './integrations'
import { runScript } from './script'
import { checkSlack, slackEventParams, startSlack } from './slack'

const timeout = { timeout_minutes: z.number().optional().describe('Resolve as timed_out after this long.') }

export const item: EventType = {
  type: 'item',
  description:
    'An item: a thing to be done. Its events sit in it, child items included, and it blocks the item it sits in until it closes. ' +
    'Add one to split work off: give it its first events (e.g. a message) in `events`, and who has it in `assignees`. With no ' +
    'assignees, it is its maker\'s if a person made it; otherwise it goes to the person who has (or last had) the item it sits in.',
  params: {
    title: z.string(),
    events: z
      .array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() }))
      .optional()
      .describe('Events to put in it as it is created. They are added in it, and not kept in its params.'),
    key: z.string().optional().describe('Identifies it, so a source polling again adds it only once.'),
    detached: z
      .boolean()
      .optional()
      .describe('Put it in no item, rather than the one it is added in: for work unrelated to this item, which should not block it.'),
    assignees: z
      .array(z.string())
      .optional()
      .describe("Who has it from the start: people by username, bots by name or id. Not kept in its params: they are links."),
    sessionId: z.string().optional().describe('From before bots were their own items: the Claude session it had. Kept for the record.'),
  },
  data: '{ how: "closed" | "handled_outside" | "cancelled" }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.title),
  said: (p, d) => `${p.title}: ${d.how === 'handled_outside' ? 'done outside the tool' : d.how === 'cancelled' ? 'cancelled' : 'closed'}`,
  start: async (p, { engine, event, author }) => {
    const { events, assignees: _given, ...params } = p
    if (Array.isArray(events) && events.length) engine.add(event.id, engine.prepare(events as EventSpec[], author), event.createdBy)
    return { params }
  },
}

export const message: EventType = {
  type: 'message',
  description: 'Something said in the item: by the human, a bot, or a source.',
  params: { text: z.string() },
  data: '{}',
  blocking: false,
  // A bot speaks to the human through its output's message; it adds message events to brief a child item.
  bots: true,
  describe: (p) => String(p.text),
}

export const attachment: EventType = {
  type: 'attachment',
  description:
    'Another event, attached to this item so that it is gathered here: an idea to build with others, say. It points at the event; ' +
    'the event itself stays where it was. Add one with the attach_event action, which fills in the params.',
  params: {
    event: z.string().describe('The id of the event attached.'),
    title: z.string().describe('What it was as it was attached: its title, for an item; its summary otherwise.'),
    item: z.string().describe('The item to read it in: the event itself, for an item; the item it sits in otherwise.'),
  },
  data: '{}',
  blocking: false,
  bots: true,
  describe: (p) => `Attached: ${p.title} [${p.item === p.event ? p.event : `${p.event} in ${p.item}`}]`,
  said: (p) => `Attached: ${p.title}`,
}

export const rename: EventType = {
  type: 'rename',
  description: "Renames the item: its latest rename event's title is its title from then on. The first title stays in the item's own params.",
  params: { title: z.string().min(1).describe('The new title.') },
  data: '{}',
  blocking: false,
  bots: true,
  describe: (p) => `Renamed to “${p.title}”`,
}

export const priority: EventType = {
  type: 'priority',
  description:
    "Marks the item as a priority, or not: its latest priority event says which. Next takes the human's priority items " +
    'first (the most recently active first), then the rest (the longest waiting first).',
  params: { on: z.boolean().describe('Whether it is a priority from now on.') },
  data: '{}',
  blocking: false,
  bots: true,
  describe: (p) => (p.on ? 'Marked as a priority' : 'No longer a priority'),
}

export const action: EventType = {
  type: 'action',
  description: 'An action run on the item, with what it was given and what it answered.',
  params: { name: z.string(), inputs: z.record(z.unknown()), output: z.unknown().optional() },
  data: '{}',
  blocking: false,
  describe: (p) => `Ran ${p.name}`,
  // A custom action says what it returned; anything else that it ran.
  said: (p) => {
    const out = p.output as { message?: unknown } | undefined
    return out && typeof out === 'object' && typeof out.message === 'string' && out.message ? out.message : `Ran ${p.name}`
  },
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

/** From before bots were their own items: a blocker that a worker answered. Kept so old items read as they did; none is added now. */
export const bot: EventType = {
  type: 'bot',
  description: 'Old, and not added any more: to hand an item to a bot, assign it to the bot (the assign action).',
  params: { instructions: z.string().optional().describe('What to do, when there is no message from the human to go on.') },
  data: '{ runId, message?, costUsd?, output } or { error }',
  blocking: true,
  describe: (p) => String(p.instructions ?? 'Bot'),
  // Its reply is a message event of its own; only a failure needs saying here.
  said: (_p, d) => (d.error ? `The bot failed: ${d.error}` : null),
}

export const agent: EventType = {
  type: 'agent',
  description:
    'A bot: a name, instructions, and how it runs. It sits in no item, and is open while it lasts (cancelling it deletes it). ' +
    'Items are assigned to it (the assign action). It wakes when something happens to it: an item is assigned to it, or its ' +
    'settings change; and every 15 minutes while it has items that something happened in. Its settings change with configure ' +
    'events in it.',
  params: {
    name: z.string().min(1).describe('What it is called: people @ it by this name.'),
    instructions: z.string().optional().describe('What it is for and how it works: given with every run.'),
    provider: z.string().optional().describe("Who runs it: 'claude' (the default)."),
    model: z.string().optional().describe('The model, as the provider names it (e.g. opus, sonnet); the default if left out.'),
    debounce_minutes: z.number().min(0).optional().describe('How long to wait after the last thing that happened before it wakes.'),
    folder: z.string().optional().describe('Its working folder; its own, under the app\'s, if left out.'),
    sessionId: z.string().optional().describe("Its Claude session, set by the app once it first runs. Don't set it."),
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

export const configure: EventType = {
  type: 'configure',
  description: "Changes a bot's settings: the fields given replace its own, the latest configure event's winning.",
  params: {
    name: z.string().min(1).optional(),
    instructions: z.string().optional(),
    provider: z.string().optional(),
    model: z.string().optional(),
    debounce_minutes: z.number().min(0).optional(),
    folder: z.string().optional(),
  },
  data: '{}',
  blocking: false,
  bots: true,
  describe: (p) => `Settings: ${Object.keys(p).join(', ')}`,
}


export const github: EventType = {
  type: 'github',
  description: 'Wait on a pull request, checked every few minutes.',
  params: githubEventParams,
  data: '{ outcome: "passed" | "failed" | "activity" | "merged" | "closed" | "not_found" | "timed_out", detail, failed? }',
  blocking: true,
  bots: true,
  describe: (p) =>
    p.repo && p.number ? `${p.repo}#${p.number}${p.title ? ` ${p.title}` : ''}: until ${p.until}` : `“${p.query}”: until ${p.until}`,
  said: (p, d) => String(d.detail ?? d.outcome),
  start: (p, { engine }) => startPr(p, engine.exec),
  check: (p, { engine }) => checkPr(p, engine.exec),
}

export const slack: EventType = {
  type: 'slack',
  description: "Optionally post a message as the operator, then wait for somebody else's reply in its thread.",
  params: slackEventParams,
  data: '{ outcome: "replied" | "timed_out", channel, thread_ts, replies?, detail }',
  blocking: true,
  bots: true,
  expand: (p, author) => {
    if (!p.message || author === 'human') return [{ type: 'slack', params: p }]
    // Posting is unsafe: a bot's message becomes an approval, and the wait follows it.
    const { message: text, ...rest } = p
    const ref = `post-${Math.random().toString(36).slice(2, 10)}`
    return [
      { type: 'approval', ref, params: { action: 'slack_post', inputs: { workspace: p.workspace, channel: p.channel, text, thread_ts: p.thread_ts }, reason: 'Send this Slack message?' } },
      { type: 'after', params: { event: ref, then: [{ type: 'slack', params: { ...rest, thread_of: ref } }] } },
    ]
  },
  describe: (p) => `${p.message ? `Post “${p.message}”, then w` : 'W'}ait for a reply in ${p.channel}`,
  said: (_p, d) => String(d.detail ?? d.outcome),
  start: async (p, { engine, item }) => startSlack(p, engine, item.id),
  check: (p, { engine }) => checkSlack(p, engine),
}

export const after: EventType = {
  type: 'after',
  description:
    'Wait for another event to resolve, then add events: `then` as given, or what `script` returns. This is how events chain, ' +
    'e.g. wait on CI, then assign a bot if it failed and leave it with the human if it passed; or wait for a child item to close.',
  params: {
    event: z.string().describe("The event to wait for: an id, or the ref of an event added alongside this one."),
    then: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })).optional(),
    script: z
      .string()
      .optional()
      .describe(
        'A JavaScript function expression: async (input, actions) => events. input is { item: { id, title }, event: { type, params, data } }, ' +
          'the event waited for. actions.<name>(args) calls an action; a script a bot wrote gets no unsafe ones. ' +
          'Return an event ({ type, params }), a list of them, or null. console.log is kept.',
      ),
    description: z.string().optional().describe('What it does, in a line.'),
  },
  data: '{ added: string[] (ids), logs?: string[] }, { skipped: true } when the event was cancelled, or { error }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.description ?? 'Then what comes next'),
  // What it added says itself; only a failed script needs saying.
  said: (_p, d) => (d.error ? `A script failed: ${d.error}` : null),
  check: async (p, { engine, item, author }) => {
    const waited = engine.event(String(p.event))
    if (!waited) throw new Error(`No event ${p.event}.`)
    if (!waited.resolved) return null
    if (waited.cancelled) return { skipped: true, detail: 'The event it waited for was cancelled.' }
    let next = (p.then as EventSpec[] | undefined) ?? []
    let logs: string[] | undefined
    // A script that fails, or answers events that can't be added, won't do better next time: it resolves with the error.
    try {
      if (p.script) {
        const ran = await runScript(
          String(p.script),
          { item: { id: item.id, title: item.title }, event: { type: waited.type, params: waited.params, data: waited.data } },
          (name, args) => engine.callAction(name, args, { engine, author, actor: 'script', itemId: item.id, runId: null }),
        )
        next = ran.value === null ? [] : Array.isArray(ran.value) ? ran.value : [ran.value as EventSpec]
        logs = ran.logs
      }
      next = engine.prepare(next, author)
    } catch (e) {
      return { error: (e as Error).message, ...(logs ? { logs } : {}) }
    }
    const added = engine.add(item.id, next, author === 'bot' ? 'script:bot' : 'script')
    return { added, ...(logs ? { logs } : {}), detail: p.description ? String(p.description) : undefined }
  },
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
    'Example, waiting for a PR to get two approvals: async (ctx, actions) => { const pr = await actions.github_pr_view({ repo: "o/r", ' +
    'number: 7 }); const n = pr.reviews.filter((r) => r.state === "APPROVED").length; return n >= 2 ? { done: true, data: { approvals: n }, ' +
    'message: "o/r#7 has two approvals" } : { done: false } }\n' +
    'A loop is a script that never says it is done: it runs on its schedule (the last gap repeating) until it is cancelled. ' +
    'To have a bot work on something every hour, say: schedule [3600], and a source that adds a message for it each run (it wakes ' +
    'on anything new in an item assigned to it), e.g. async (ctx, actions) => { await actions.add_events({ item_id: ctx.item.id, ' +
    'events: [{ type: "message", params: { text: "Check the open PRs and fix any failing CI" } }] }); return { done: false } }',
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
  check: async (p, { engine, item, event, author, memo, setMemo }) => {
    const runs = event.times.checks
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

export const directive: EventType = {
  type: 'directive',
  description:
    "A note from the operator (or a bot, passing on their feedback) on what they are working on at the moment: a bit of markdown " +
    'that gives agents the broad picture. It sits in no item, wherever it is added, and holds until it is deleted (cancelled). ' +
    'Nothing is told them: read the latest with the directives action (list_items with type directive) when an item needs more context.',
  params: {
    text: z.string().describe('Markdown: what the operator is working on, or how they want things done.'),
    replaces: z.string().optional().describe('An open directive this one is an edit of: it is deleted as this one is added.'),
  },
  data: '{ cancelled: true }, once deleted; { cancelled: true, replacedBy }, once edited',
  // Open while it holds; deleting it is cancelling it. It blocks nothing, since it sits in no item.
  blocking: true,
  bots: true,
  describe: (p) => String(p.text).split('\n').find((l) => l.trim())?.replace(/^#+\s*/, '') ?? 'Directive',
}

/** When a snooze wakes, in ms: its `until`, an ISO time. */
const wakesAt = (p: Record<string, unknown>) => Date.parse(String(p.until))

export const snooze: EventType = {
  type: 'snooze',
  description:
    'Puts the item away until a time: it blocks the item, so Next leaves it out, and resolves once the time comes (checked every ' +
    'minute), when the item is the human\'s turn again. Cancelling it wakes the item at once.',
  params: {
    until: z
      .string()
      .refine((s) => !Number.isNaN(Date.parse(s)), 'An ISO time, such as 2026-10-24T09:00:00Z.')
      .describe('When to wake the item: an ISO time, such as 2026-10-24T09:00:00Z.'),
  },
  data: '{ woke: ISO time }',
  blocking: true,
  bots: true,
  describe: (p) => `Snoozed until ${new Date(wakesAt(p)).toLocaleString(undefined, { dateStyle: 'medium', timeStyle: 'short' })}`,
  said: () => 'Woke up',
  schedule: () => [0, 60],
  check: async (p, { engine }) => (engine.now() >= wakesAt(p) ? { woke: new Date(engine.now()).toISOString() } : null),
}

/** How a summary is to be written: ASD-STE100, so it reads plainly. */
const SUMMARY_SYSTEM = `You summarise one task for the person who owns it. Write in ASD-STE100 Simplified Technical English:
- Use short sentences (20 words or fewer). Use one instruction or one statement in each sentence.
- Use the active voice and simple verb tenses. Use the approved meanings of common words. Do not use idioms or slang.
- Write only facts. Do not use filler, enthusiasm, hedges, or comments about the summary itself.

Tell the current state, not the history. Do not tell who said what, or when. Give:
- What the task is.
- What is done or decided, if it is still important.
- What is open, and who or what it waits for.
- What the owner must do next, if anything.

Use 2 to 5 markdown bullets. Use no headings and no other text.`

export const summary: EventType = {
  type: 'summary',
  description:
    'A summary of where the item stands now, from a cheap model (Haiku, no tools), in ASD-STE100. It blocks while it is written ' +
    '(a few seconds), then says the summary in the chat. Ctrl+S adds one.',
  params: {},
  data: '{ text } or { error }',
  blocking: true,
  bots: true,
  describe: () => 'Summary',
  said: (_p, d) => (d.error ? `The summary failed: ${d.error}` : String(d.text ?? '')),
  schedule: () => [0],
  check: async (_p, { engine, item, event }) => {
    const lines = engine
      .children(item.id)
      .filter((e) => e.type !== 'summary' && e.createdAt <= event.createdAt)
      .map((e) => `- ${e.createdBy} ${e.type}${e.resolved ? '' : ' (still open)'}: ${e.type === 'message' ? String(e.params.text ?? '') : (e.message ?? e.summary)}`)
    const state = item.closed ? 'resolved' : 'open'
    const prompt = [`Task: ${item.title} (${state})`, 'Its events, oldest first:', ...lines].join('\n')
    try {
      const out = await engine.exec('claude', [
        '-p', prompt, '--model', 'haiku', '--tools', '', '--strict-mcp-config', '--no-session-persistence', '--system-prompt', SUMMARY_SYSTEM,
      ])
      return { text: out.trim() }
    } catch (e) {
      return { error: (e as Error).message }
    }
  },
}

export const EVENT_TYPES: EventType[] = [
  item, message, attachment, rename, priority, action, approval, github, slack, after, script, summary, directive, snooze, agent, configure,
  user, bot,
]
