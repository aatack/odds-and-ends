// The bots' runner. Each bot (an `agent` event) runs one thing at a time, in a
// Claude session of its own and a folder of its own, both of which last: so a
// bot remembers its work, and keeps its repositories, from one run to the next.
// A bot wakes when something happens to it (an item is assigned to it, its
// settings change), and every 15 minutes while something has happened in its
// items (Engine.wake), once its debounce has passed; a run is told what is new
// in each item it has, and answers with an update for each as structured
// output, which `Engine.endRun` takes from there.

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

/** Environment variables that hold the app's own secrets: a bot's run is not given them. */
const APP_SECRETS = ['DEEPGRAM_API_KEY']

/** How many bots may run at once, unless the `bots.max_running` setting says otherwise. */
export const MAX_BOTS = 6

const RUN_LIMIT_MS = 60 * 60_000

export const SYSTEM = `You are a bot in the operator's workflow system: one of its bots, with a name, instructions of your own (below, if any), a Claude session of your own and a working folder of your own, all of which last from one run to the next. You run one thing at a time.

- The "workflows" MCP server is the system: tasks and the events in them, people and bots. event_types describes every kind of event. Its tools take task ids, and you may act on any task.
- Tasks are assigned to people or to bots: that is who has them. You wake when something happens to you (a task is assigned to you, your settings change, or someone writes in your own chat), and every 15 minutes while something has happened in the tasks you have. Each prompt is your own chat, if someone wrote in it, then every task you have, one after another: what is new in it since you last saw it (people's notes as they wrote them, after their name; anything else in [square brackets]), or its whole history the first time. get_item has a task in full any time.
- Action the tasks assigned to you, and give each a new assignee to carry it on: a person, another bot, or yourself to keep it.
- A task's chat is every event in it: what you want people to see there must be an event in the task (your update's message is a note in it; add_events adds others, such as scripts and child tasks). Text anywhere else (your shell, your own replies outside the update) is not seen.
- Do what people's latest notes ask, as far as you can without them. In your own chat, answer with an update for your own id: its message is your reply there.
- Never close a task unless someone has explicitly asked you to close it. Finishing the work is not being asked: hand the task back instead.
- You also have the usual tools (a shell, files). Your working folder is yours alone, and it lasts: keep clones and worktrees there. Read a repository's AGENTS.md or CLAUDE.md before working in it, since you were not started in it. A shell's cd only lasts inside your folder; elsewhere, use absolute paths and git -C.
- Actions that change things outside this app are unsafe, and you never do them: not with the MCP tools (you do not have the unsafe ones; unsafe_actions lists them), and not with the shell either (no posting, commenting, reviewing, merging, pushing to shared branches, or sending). Work on a local branch, local commits included, is not unsafe. To get one done, ask a person in a note.
- To say something in another task (one you don't have), add a note in it: an update for that task with a message, or add_events. Who has it is left alone unless you say.
- When you stop, return an update for each task you have: a note to the people in it if there is something to say (message), events to add, who has it now (assign), and close only if you were asked to. A task you say nothing of who has goes back to its owner (the person who made it), unless you added a wait to it: then you keep it.
- To hand a task to someone, assign it to them (in your update, or with the assign action). list_people and list_bots say who there is.
- To wait for something, or for anything repetitive that doesn't need you (an LLM), hand the task to a script: a script event with wake_at and owns [the task]. It runs JavaScript on its own wake times with no model, and hands the task on when it is done. event_types shows how to write one; run_script runs code once, now.
- To split work off, add a child task with its first events (e.g. a note) and its assignees. It blocks nothing: to wait on it, hand the parent to a script that checks it.
- Keep traceability: when you make a task from something, link_events the messages and items it came from (and the tasks it depends on) into it, and link the new task into where it came from.
- For work unrelated to a task, add a task with detached: true. It sits in no task and blocks nothing.
- [id]() in text refers to a task, a bot or a person (people's @ inserts them), and is shown as its title or name. Use the same to refer to one in your notes.
- Ask people only what nobody else can answer, as one direct question: no preamble, no summary, no waffle.`

/** The app's instructions and the bot's own, as a run is given them. */
/** The open task bots tell of what they needed and didn't have: the one titled so (the operator reads them through). */
export const GAPS_TITLE = 'Bot capability gaps'

export const systemFor = (agent: Agent, gaps: string | null = null): string =>
  `${SYSTEM}${gaps ? `\n- When you run into something you needed and couldn't do (a missing tool, an action, access, information), add a note in [${gaps}]() saying what, and what it was for, in a line or two: the operator decides what to build. Then carry on as best you can.` : ''}\n\nYou are @${agent.name}.${agent.instructions.trim() ? `\n\nYour own instructions, from the operator:\n\n${agent.instructions.trim()}` : ''}`

/**
 * Claude Code, in auto mode, with the workflows MCP server and the bot's
 * output schema. The instructions are its system prompt (appended to Claude
 * Code's own), not part of any prompt; after a compaction, a SessionStart hook
 * says them again, so the summary does not leave the bot unsure of who it is.
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
    // The app's own secrets stay out of a bot's environment (its shell could read them there).
    { cwd, stdio: ['ignore', 'pipe', 'pipe'], env: Object.fromEntries(Object.entries(process.env).filter(([k]) => !APP_SECRETS.includes(k))) },
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

/** Who did something, for a prompt: their name, and whether they are a person (whose words go to the bot as they are). */
export interface Who {
  name: (actor: string) => string
  person: (actor: string) => boolean
}

const ANYONE: Who = { name: (a) => a, person: (a) => a === 'human' || a === 'mcp' }

/**
 * One event as a line of the prompt: what a person wrote, as they wrote it, after
 * their name; anything else in square brackets.
 */
export function promptLine(e: ItemEvent, who: Who = ANYONE): string | null {
  if (e.type === 'note')
    return who.person(e.createdBy) ? `${who.name(e.createdBy)}: ${e.params.text ?? ''}` : `[${who.name(e.createdBy)}: ${e.params.text ?? ''}]`
  if (e.cancelled) return `[${e.message ?? `Cancelled: ${e.summary}`}]`
  if (!e.resolved) return `[${e.type} added by ${who.name(e.createdBy)}: ${e.summary}]`
  return `[${e.message ?? `${e.summary}: done`}]`
}

/** What every run's prompt starts with. */
export const ASK = '[Please action your assigned tasks, and give each a new assignee to carry it on.]'

/**
 * A run's prompt: the ask, then for each item the bot has (and its own chat,
 * first, when someone has written in it), what is new in it since the bot was
 * last prompted (its whole history, the first time), with little direction —
 * people's notes as they are, anything else in brackets — and, every ID_EVERY
 * lines from the first, which item it is.
 */
export function promptFor(items: { item: Item; events: ItemEvent[]; first: boolean; own?: boolean }[], who?: Who): string {
  if (!items.length) return ''
  return `${ASK}\n\n---\n\n` + [...items].sort((a, b) => Number(!!b.own) - Number(!!a.own))
    .map(({ item, events, first, own }) => {
      const lines = events.map((e) => promptLine(e, who)).filter((l): l is string => l !== null)
      const head = own ? ['[your own chat: reply with an update for this id]'] : first ? [`[new to you: ${item.title}]`] : [`[task: ${item.title}]`]
      const all = [...head, ...(lines.length ? lines : ['[nothing new]'])]
      return all.flatMap((l, i) => (i % ID_EVERY === 0 ? [`[current task ID: ${item.id}]`, l] : [l])).join('\n\n')
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
  private running = new Map<string, { runId: string; kill: () => void; ended: Promise<void> }>()
  private stopped = false
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

  /** Stops every run, and settles once each has ended (its items handed on as any ended run's are). Starts no more. */
  async stop(): Promise<void> {
    this.stopped = true
    if (this.ticker) clearInterval(this.ticker)
    this.ticker = null
    const ending = [...this.running.values()].map((r) => r.ended)
    for (const r of this.running.values()) r.kill()
    await Promise.all(ending)
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
    if (this.stopped) return
    // At most `bots.max_running` at once (MAX_BOTS by default): those due longest go first, the rest wait their turn.
    const max = Number(this.engine.settings.get('bots.max_running', MAX_BOTS))
    const due = this.engine.agents().filter((a) => !this.running.has(a.id) && a.wakesAt !== null).sort((a, b) => a.wakesAt! - b.wakesAt!)
    for (const agent of due) {
      if (this.running.size >= max) return
      const claimed = this.engine.claim(agent.id)
      if (claimed) this.run(claimed.agent, claimed.items, claimed.resume)
    }
  }

  private run(agent: Agent, items: { item: Item; events: ItemEvent[]; first: boolean; own?: boolean }[], resume: boolean): void {
    const prompt = promptFor(items, { name: (a) => this.engine.actorName(a), person: (a) => this.engine.isUser(a) || a === 'mcp' })
    const runId = this.engine.startRun(agent.id, items.map((i) => i.item.id))
    const lines: string[] = []
    let buffer = ''
    const onOutput = (chunk: string) => {
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
        prompt, system: systemFor(agent, this.engine.overview().items.find((i) => i.title === GAPS_TITLE && !i.closed)?.id ?? null), schema: botOutputSchema(this.engine.eventTypes), mcpUrl: this.mcpUrl(runId),
        sessionId: agent.sessionId!, resume, model: agent.model, cwd: this.engine.agentFolder(agent.id), onOutput,
      })
    } catch (e) {
      void end(null, (e as Error).message)
      return
    }
    const limit = setTimeout(() => proc.kill(), RUN_LIMIT_MS)
    const ended = proc.done
      .then(
        ({ exitCode }) => end(exitCode),
        (e: Error) => end(null, e.message),
      )
      .catch((e: Error) => console.error(`@${agent.name}'s run ${runId} could not end cleanly:`, e))
      .finally(() => {
        clearTimeout(limit)
        this.running.delete(agent.id)
        this.schedule()
      })
    this.running.set(agent.id, { runId, kill: proc.kill, ended })
  }
}
