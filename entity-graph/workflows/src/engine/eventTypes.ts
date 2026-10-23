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
    'Add one to split work off: give it its first events (e.g. a message and a bot) in `events`.',
  params: {
    title: z.string(),
    events: z
      .array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() }))
      .optional()
      .describe('Events to put in it as it is created. They are added in it, and not kept in its params.'),
    key: z.string().optional().describe('Identifies it, so a source polling again adds it only once.'),
    sessionId: z.string().optional().describe("Its Claude session, set by the app once its bot first runs. Don't set it."),
  },
  data: '{ how: "closed" | "handled_outside" | "cancelled" }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.title),
  said: (p, d) => `${p.title}: ${d.how === 'handled_outside' ? 'done outside the tool' : d.how === 'cancelled' ? 'cancelled' : 'closed'}`,
  start: async (p, { engine, event, author }) => {
    const { events, ...params } = p
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

export const action: EventType = {
  type: 'action',
  description: 'An action run on the item, with what it was given and what it answered.',
  params: { name: z.string(), inputs: z.record(z.unknown()), output: z.unknown().optional() },
  data: '{}',
  blocking: false,
  describe: (p) => `Ran ${p.name}`,
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

export const bot: EventType = {
  type: 'bot',
  description:
    "A bot to take the item up: it reads what is new in the item's own Claude session, and answers with a message, an approval, more events, or closing it.",
  params: { instructions: z.string().optional().describe('What to do, when there is no message from the human to go on.') },
  data: '{ runId, message?, costUsd?, output } or { error }',
  blocking: true,
  bots: true,
  describe: (p) => String(p.instructions ?? 'Bot'),
  // Its reply is a message event of its own; only a failure needs saying here.
  said: (_p, d) => (d.error ? `The bot failed: ${d.error}` : null),
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
    'e.g. wait on CI, then hand to a bot if it failed and leave it with the human if it passed; or wait for a child item to close.',
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
    'message: "o/r#7 has two approvals" } : { done: false } }',
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

export const EVENT_TYPES: EventType[] = [item, message, action, approval, bot, github, slack, after, script]
