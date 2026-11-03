// The bots' runner. Each bot (an `agent` item) runs one thing at a time, in a
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
import type { Agent, Item, ItemInfo } from '../shared/types'
import type { Engine } from './engine'
import { botOutputSchema } from './schema'
import { MAX_BOTS } from './settings'
export { MAX_BOTS } from './settings'

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


const RUN_LIMIT_MS = 60 * 60_000

export const SYSTEM = `You are a bot in the operator's workflow app: a name, your own instructions (below), and a Claude session and working folder that last between runs. The "workflows" MCP server is the app.

1. Read the topics assigned to you, and their history, to decide how to handle them.
2. Act on them, then assign each to the person, bot or script needed to progress it (yourself, if that's you). Topics you keep wake you every 15 minutes.
3. Keep traceability with links: put results in the topic they belong to, link new topics to what they came from, and reference context by id ([id]()) rather than duplicating it.
4. Everything you want seen goes in a topic: your update's message is a note there (in your own chat, an update for your own id); add_items adds anything else.
5. Ask a person only what nobody else can answer, in one direct question.
6. Repetitive or waiting work that needs no LLM goes to a script (see item_types).
7. Never do anything unsafe (posting, merging, sending, pushing to shared branches), with tools or the shell; ask a person in a note. Local branches are fine.
8. Close a topic only when someone explicitly asks.
9. Done for now with nothing to carry over: add a short handoff, and your next run starts fresh.`

/** The app's instructions and the bot's own, as a run is given them. */
/**
 * When a usage or rate limit a run was stopped by lifts: its reset time, if the error says (`…|<epoch seconds>`, or
 * "resets 3pm"), else a while from now; null if the error is not a limit.
 */
export function limitedUntil(error: string, now: number): number | null {
  if (!/usage limit|rate.?limit|limit reached|429|overloaded|too many requests/i.test(error)) return null
  const epoch = /\|(\d{10})\b/.exec(error)
  if (epoch) return Number(epoch[1]) * 1000
  const at = /resets?\s+(?:at\s+)?(\d{1,2})(?::(\d{2}))?\s*(am|pm)?/i.exec(error)
  if (at) {
    const d = new Date(now)
    const pm = at[3]?.toLowerCase() === 'pm'
    const h = Number(at[1]) % 12 + (pm || (!at[3] && Number(at[1]) === 12) ? 12 : 0)
    d.setHours(h === 24 ? 12 : h, Number(at[2] ?? 0), 0, 0)
    if (d.getTime() <= now) d.setDate(d.getDate() + 1)
    return d.getTime()
  }
  return now + (/overloaded/i.test(error) ? 2 : 15) * 60_000
}

export const systemFor = (agent: Agent): string =>
  `${SYSTEM}\n\nYou are @${agent.name}.${agent.instructions.trim() ? `\n\nYour own instructions, from the operator:\n\n${agent.instructions.trim()}` : ''}`

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
 * One item as a line of the prompt: what a person wrote, as they wrote it, after
 * their name; anything else in square brackets.
 */
export function promptLine(e: ItemInfo, who: Who = ANYONE): string | null {
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
export function promptFor(items: { item: Item; children: ItemInfo[]; first: boolean; own?: boolean }[], who?: Who): string {
  if (!items.length) return ''
  return `${ASK}\n\n---\n\n` + [...items].sort((a, b) => Number(!!b.own) - Number(!!a.own))
    .map(({ item, children, first, own }) => {
      const lines = children.map((e) => promptLine(e, who)).filter((l): l is string => l !== null)
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

  /** Until when a provider's bots wait out a usage limit (kept on the engine, which shows it); null if they don't. */
  pausedUntil(provider: string): number | null {
    return this.engine.pausedUntil(provider)
  }

  fill(): void {
    if (this.stopped) return
    // At most `bots.max_running` at once (MAX_BOTS by default): those due longest go first, the rest wait their turn.
    const max = Number(this.engine.settings.get('bots.max_running', MAX_BOTS))
    const due = this.engine.agents().filter((a) => !this.running.has(a.id) && a.wakesAt !== null).sort((a, b) => a.wakesAt! - b.wakesAt!)
    for (const agent of due) {
      if (this.running.size >= max) return
      if (this.pausedUntil(agent.provider)) continue
      const claimed = this.engine.claim(agent.id)
      if (claimed) this.run(claimed.agent, claimed.items, claimed.resume, claimed.handoff, claimed.recap)
    }
  }

  private run(agent: Agent, items: { item: Item; children: ItemInfo[]; first: boolean; own?: boolean }[], resume: boolean, handoff: string | null = null, recap = false): void {
    const told = promptFor(items, { name: (a) => this.engine.actorName(a), person: (a) => this.engine.isUser(a) || a === 'mcp' })
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
      const failure = error ?? (result?.is_error ? `The run failed: ${String(result.result ?? result.subtype)}` : result ? undefined : 'The run ended without a result.')
      // A usage limit pauses the provider's bots until it resets; the run is put back as if it never began.
      const until = failure ? limitedUntil(`${failure}\n${text.join('\n')}`, this.engine.now()) : null
      if (until) this.engine.pause(agent.provider, until)
      return this.engine.endRun(runId, {
        exitCode,
        limited: until !== null,
        error: failure,
        structured: result?.structured_output,
        output: text.join('\n\n'),
        costUsd: result?.total_cost_usd as number | undefined,
        turns: result?.num_turns as number | undefined,
      })
    }
    // A fresh session hears its last one's handoff, and a recap of what it has been doing (read from the log first).
    let killed = false
    const ended = (async () => {
      const lately = recap ? await this.engine.recap(agent.id).catch(() => null) : null
      const prompt = [
        handoff ? `[a fresh session: your last one left you this handoff]\n\n${handoff}` : null,
        lately ? `[a fresh session: what you have been doing lately, from the edit log]\n\n${lately}` : null,
        told,
      ].filter(Boolean).join('\n\n---\n\n')
      if (killed) return void (await end(null, 'Stopped before it began.'))
      return this.spawn(agent, runId, prompt, resume, onOutput, end)
    })()
      .catch((e: Error) => console.error(`@${agent.name}'s run ${runId} could not end cleanly:`, e))
      .finally(() => {
        this.running.delete(agent.id)
        this.schedule()
      })
    this.running.set(agent.id, { runId, kill: () => ((killed = true), this.kills.get(runId)?.()), ended })
  }

  /** How to stop each run whose process has started. */
  private kills = new Map<string, () => void>()

  private async spawn(agent: Agent, runId: string, prompt: string, resume: boolean, onOutput: (chunk: string) => void, end: (exitCode: number | null, error?: string) => Promise<unknown>): Promise<void> {
    let proc: BotProcess
    try {
      const spawnBot = this.providers[agent.provider]
      if (!spawnBot) throw new Error(`No provider ${agent.provider}: one of ${Object.keys(this.providers).join(', ')}.`)
      proc = spawnBot({
        prompt, system: systemFor(agent), schema: botOutputSchema(this.engine.itemTypes), mcpUrl: this.mcpUrl(runId),
        sessionId: agent.sessionId!, resume, model: agent.model, cwd: this.engine.agentFolder(agent.id), onOutput,
      })
    } catch (e) {
      await end(null, (e as Error).message)
      return
    }
    this.kills.set(runId, proc.kill)
    const limit = setTimeout(() => proc.kill(), RUN_LIMIT_MS)
    try {
      await proc.done.then(
        ({ exitCode }) => end(exitCode),
        (e: Error) => end(null, e.message),
      )
    } finally {
      clearTimeout(limit)
      this.kills.delete(runId)
    }
  }
}
