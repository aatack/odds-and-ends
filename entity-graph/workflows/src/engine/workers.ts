// The bots' runner. Each bot (an `agent` event) runs one thing at a time, in a
// Claude session of its own and a folder of its own, both of which last: so a
// bot remembers its work, and keeps its repositories, from one run to the next.
// A bot wakes when anything happens in an item assigned to it, or that it
// watches (Engine.wake), once its debounce has passed; a run is told what is
// new in each of those items, and answers with an update for each as
// structured output, which `Engine.endRun` takes from there.

import { spawn } from 'node:child_process'
import { mkdirSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'
import type { Agent, Item, ItemEvent } from '../shared/types'
import type { Engine } from './engine'
import { botOutputSchema } from './schema'

export interface BotProcess {
  done: Promise<{ exitCode: number | null }>
  kill: () => void
}

/** Starts one bot; `onOutput` gets its stdout (stream-json) as it comes. Handed in, so the tests needn't run claude. */
export type SpawnBot = (args: {
  prompt: string
  /** The app's instructions, then the bot's own. */
  system: string
  schema: Record<string, unknown>
  mcpUrl: string
  /** The bot's session: resumed when `resume` is true; otherwise started with this id. */
  sessionId: string
  resume: boolean
  /** The model, as the provider names it; empty for its default. */
  model: string
  /** The bot's own folder, to run in. */
  cwd: string
  onOutput: (chunk: string) => void
}) => BotProcess

const RUN_LIMIT_MS = 60 * 60_000

export const SYSTEM = `You are a bot in the operator's workflow system: one of its bots, with a name, instructions of your own (below, if any), a Claude session of your own and a working folder of your own, all of which last from one run to the next. You run one thing at a time.

- The "workflows" MCP server is the system: items, events, bots, and the integrations. event_types describes every kind of event. Its tools take item ids, and you may act on any item.
- Items are assigned to the human or to bots. You are woken when anything happens in an item assigned to you, or that you watch. Each prompt is what is new in those items, item by item: the operator's messages as they wrote them, and anything else (other events, results, instructions) in [square brackets]. The first time you see an item, you get its whole history; get_item has it any time.
- Do what the operator's latest messages ask, as far as you can without them.
- You also have the usual tools (a shell, files). Your working folder is yours alone, and it lasts: keep clones and worktrees there. Read a repository's AGENTS.md or CLAUDE.md before working in it, since you were not started in it. A shell's cd only lasts inside your folder; elsewhere, use absolute paths and git -C.
- Actions that change things outside this app are unsafe, and you never do them: not with the MCP tools (you do not have the unsafe ones; unsafe_actions lists them), and not with the shell either (no posting, commenting, reviewing, merging, pushing to shared branches, or sending). Work on a local branch, local commits included, is not unsafe. To get one done, add an approval event, filled in as far as you can.
- When you stop, return an update for each item you were woken for: a message to the operator if there is something to say, events to add, who has the item now, and whether it is done. An item you say nothing of, or leave unassigned, goes back to the human; one you added a wait to stays yours, and you wake when the wait resolves. Chain blockers with after events: e.g. a github wait, then an after whose script assigns you again (an assign event) if the checks failed.
- To hand an item to another bot, assign it to them. To keep an eye on an item that is not yours, watch it (a watch event).
- To wait for something no other event type covers, add a script event: a function the app runs on a schedule, which checks with the actions and says when it is done. event_types shows how to write one.
- To split work off, add a child item with its first events (e.g. a message and an assign). It blocks its parent until it closes.
- For work unrelated to an item, add an item with detached: true. It sits in no item and blocks nothing.
- [itemId]() in text is a reference to that item, or to a bot (the operator's @ inserts them), and is shown as its title or name. Use the same to refer to an item in your messages.
- The operator's directives say what they are working on at the moment. They are not in your prompt: when you need more context than an item gives, read the most recent with the directives tool. When the operator gives feedback that should hold beyond one item, add a directive event with add_events (it sits in no item, wherever it is added); to change one, add a directive that replaces it.
- Ask the operator only what nobody else can answer, as one direct question: no preamble, no summary, no waffle.`

/** The app's instructions and the bot's own, as a run is given them. */
export const systemFor = (agent: Agent): string =>
  `${SYSTEM}\n\nYou are @${agent.name}.${agent.instructions.trim() ? `\n\nYour own instructions, from the operator:\n\n${agent.instructions.trim()}` : ''}`

/**
 * Claude Code, in auto mode, with the workflows MCP server and the bot's
 * output schema. The instructions go in as an appended system prompt, which is
 * sent with every request, so compacting the session never loses them; a
 * SessionStart hook says them again into the conversation after a compaction too,
 * so the summary does not leave the bot unsure of who it is.
 */
export const spawnClaude: SpawnBot = ({ prompt, system, schema, mcpUrl, sessionId, resume, model, cwd, onOutput }) => {
  const mcp = JSON.stringify({ mcpServers: { workflows: { type: 'http', url: mcpUrl } } })
  const own = join(cwd, '.workflows')
  mkdirSync(own, { recursive: true })
  const instructions = join(own, 'instructions.md')
  writeFileSync(instructions, system)
  const settings = JSON.stringify({
    hooks: { SessionStart: [{ matcher: 'compact', hooks: [{ type: 'command', command: `cat ${JSON.stringify(instructions)}` }] }] },
  })
  const child = spawn(
    'claude',
    ['-p', prompt,
      ...(resume ? ['--resume', sessionId] : ['--session-id', sessionId]),
      ...(model ? ['--model', model] : []),
      '--append-system-prompt', system, '--settings', settings, '--mcp-config', mcp, '--strict-mcp-config',
      '--allowedTools', 'mcp__workflows', '--permission-mode', 'auto',
      '--json-schema', JSON.stringify(schema), '--output-format', 'stream-json', '--verbose'],
    { cwd, stdio: ['ignore', 'pipe', 'pipe'] },
  )
  child.stdout.setEncoding('utf8').on('data', onOutput)
  let stderr = ''
  child.stderr.setEncoding('utf8').on('data', (c: string) => (stderr += c))
  return {
    done: new Promise((resolve, reject) => {
      child.on('error', reject)
      child.on('close', (exitCode) => (exitCode && stderr ? reject(new Error(stderr.trim().slice(-2000))) : resolve({ exitCode })))
    }),
    kill: () => child.kill(),
  }
}

/** Who runs a bot, by its `provider`. Another provider is another entry here. */
export const PROVIDERS: Record<string, SpawnBot> = { claude: spawnClaude }

/** How often the prompt reminds the bot which item it is on: every this many lines of an item, starting with the first. */
const ID_EVERY = 10

/** The human, or an outside client acting as them: their words go to the bot as they are. */
const HUMAN_SAID = new Set(['human', 'mcp'])

/**
 * One event as a line of the prompt: what the human said, as they said it;
 * anything else in square brackets. An after that ran says nothing.
 */
export function promptLine(e: ItemEvent, who: (actor: string) => string = (a) => a): string | null {
  if (e.type === 'message') return HUMAN_SAID.has(e.createdBy) ? String(e.params.text ?? '') : `[${who(e.createdBy)}: ${e.params.text ?? ''}]`
  // A summary is for the human; the bot has the events themselves.
  if (e.type === 'summary') return null
  if (e.type === 'bot') return e.params.instructions ? `[instructions: ${e.params.instructions}]` : null
  if (e.type === 'assign') return `[${e.summary} by ${who(e.createdBy)}]`
  // An after that ran says nothing of its own: what it added is in the prompt too.
  if (e.type === 'after' && e.resolved && !e.cancelled && !e.data?.error) return null
  if (e.cancelled) return `[${e.message ?? `Cancelled: ${e.summary}`}]`
  if (!e.resolved) return `[${e.type} added by ${who(e.createdBy)}: ${e.summary}]`
  return `[${e.message ?? `${e.summary}: done`}]`
}

/**
 * A run's prompt: for each item, what is new in it since the bot was last
 * prompted (its whole history, the first time), with little direction — the
 * human's messages as they are, anything else in brackets — and, every
 * ID_EVERY lines from the first, which item it is.
 */
export function promptFor(items: { item: Item; events: ItemEvent[]; first: boolean }[], who?: (actor: string) => string): string {
  return items
    .map(({ item, events, first }) => {
      const lines = events.map((e) => promptLine(e, who)).filter((l): l is string => l !== null)
      const head = first ? [`[new to you: ${item.title}]`] : [`[item: ${item.title}]`]
      const all = [...head, ...(lines.length ? lines : ['[nothing new]'])]
      return all.flatMap((l, i) => (i % ID_EVERY === 0 ? [`[current item ID: ${item.id}]`, l] : [l])).join('\n\n')
    })
    .join('\n\n---\n\n')
}

/** What a stream-json transcript says: the assistant's text, and the result message. */
export function readStream(lines: string[]): { text: string[]; result: Record<string, unknown> | null } {
  const text: string[] = []
  let result: Record<string, unknown> | null = null
  for (const line of lines) {
    let msg: { type?: string; message?: { content?: { type: string; text?: string }[] } }
    try {
      msg = JSON.parse(line)
    } catch {
      continue // not a whole message; kept in the transcript regardless
    }
    if (msg.type === 'assistant') for (const c of msg.message?.content ?? []) if (c.type === 'text' && c.text) text.push(c.text)
    if (msg.type === 'result') result = msg as Record<string, unknown>
  }
  return { text, result }
}

/** Runs the bots: each, when it is due and not running already. */
export class BotRunner {
  private running = new Map<string, { runId: string; kill: () => void }>()
  private timer: NodeJS.Timeout | null = null
  private ticker: NodeJS.Timeout | null = null

  constructor(
    private engine: Engine,
    private mcpUrl: (runId: string) => string,
    private providers: Record<string, SpawnBot> = PROVIDERS,
  ) {
    engine.on('change', () => this.schedule())
    engine.onCancel = (agentId) => this.running.get(agentId)?.kill()
  }

  /** Looks for bots that are due every second (debounces come due on their own), until `stop`. */
  start(): void {
    this.ticker ??= setInterval(() => this.fill(), 1000)
    this.fill()
  }

  stop(): void {
    if (this.ticker) clearInterval(this.ticker)
    this.ticker = null
    for (const r of this.running.values()) r.kill()
  }

  /** The bots running now. */
  busy(): string[] {
    return [...this.running.keys()]
  }

  /** Starts what is due, once whatever change set this off has settled. */
  schedule(): void {
    if (this.timer) return
    this.timer = setTimeout(() => {
      this.timer = null
      this.fill()
    }, 0)
  }

  fill(): void {
    for (const agent of this.engine.agents()) {
      if (this.running.has(agent.id)) continue
      const claimed = this.engine.claim(agent.id)
      if (claimed) this.run(claimed.agent, claimed.items, claimed.resume)
    }
  }

  private run(agent: Agent, items: { item: Item; events: ItemEvent[]; first: boolean }[], resume: boolean): void {
    const prompt = promptFor(items, (a) => this.engine.actorName(a))
    const runId = this.engine.startRun(agent.id, items.map((i) => i.item.id), prompt)
    const lines: string[] = []
    let buffer = ''
    const onOutput = (chunk: string) => {
      this.engine.appendTranscript(runId, chunk)
      buffer += chunk
      const parts = buffer.split('\n')
      buffer = parts.pop() ?? ''
      lines.push(...parts)
    }
    const end = (exitCode: number | null, error?: string) => {
      const { text, result } = readStream([...lines, buffer])
      return this.engine.endRun(runId, {
        exitCode,
        error: error ?? (result?.is_error ? `The run failed: ${String(result.result ?? result.subtype)}` : result ? undefined : 'The run ended without a result.'),
        structured: result?.structured_output,
        output: text.join('\n\n'),
        costUsd: result?.total_cost_usd as number | undefined,
        turns: result?.num_turns as number | undefined,
      })
    }
    let proc: BotProcess
    try {
      const spawnBot = this.providers[agent.provider]
      if (!spawnBot) throw new Error(`No provider ${agent.provider}: one of ${Object.keys(this.providers).join(', ')}.`)
      proc = spawnBot({
        prompt, system: systemFor(agent), schema: botOutputSchema(this.engine.eventTypes), mcpUrl: this.mcpUrl(runId),
        sessionId: agent.sessionId!, resume, model: agent.model, cwd: this.engine.agentFolder(agent.id), onOutput,
      })
    } catch (e) {
      void end(null, (e as Error).message)
      return
    }
    const limit = setTimeout(() => proc.kill(), RUN_LIMIT_MS)
    this.running.set(agent.id, { runId, kill: proc.kill })
    proc.done
      .then(
        ({ exitCode }) => end(exitCode),
        (e: Error) => end(null, e.message),
      )
      .finally(() => {
        clearTimeout(limit)
        this.running.delete(agent.id)
        this.schedule()
      })
  }
}
