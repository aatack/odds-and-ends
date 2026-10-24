// A fixed number of workers, each running one `claude -p` at a time. Every item
// has a Claude session of its own: its first run starts it, and each later run
// resumes it, so a bot remembers the item. A run answers the item's bot
// blockers with structured output, which `Engine.endRun` takes from there.

import { spawn } from 'node:child_process'
import { mkdirSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import type { Item, ItemEvent } from '../shared/types'
import type { Engine } from './engine'
import { botOutputSchema } from './schema'

export interface BotProcess {
  done: Promise<{ exitCode: number | null }>
  kill: () => void
}

/** Starts one bot; `onOutput` gets its stdout (stream-json) as it comes. Handed in, so the tests needn't run claude. */
export type SpawnBot = (args: {
  prompt: string
  system: string
  schema: Record<string, unknown>
  mcpUrl: string
  /** The item's session: resumed when `resume` is true; otherwise started with this id, forked from `forkFrom` if given. */
  sessionId: string
  resume: boolean
  forkFrom: string | null
  /** The worker's own folder, to run in. */
  cwd: string
  onOutput: (chunk: string) => void
}) => BotProcess

const RUN_LIMIT_MS = 30 * 60_000

/**
 * Each worker's own folder, in which its bots run and make their worktrees.
 * Sessions are found by id from any folder, so an item's session survives
 * moving between workers.
 */
export function workerFolder(worker: number): string {
  const dir = join(tmpdir(), 'workflows-bots', `worker-${worker}`)
  mkdirSync(dir, { recursive: true })
  return dir
}

export const SYSTEM = `You are a bot in the operator's workflow system. You work on one item, in a session of its own: you have seen it before if this conversation has.

- The "workflows" MCP server is the system: items, events, and the integrations. event_types describes every kind of event. Its tools take item ids: yours is in the prompt, and you may act on other items too.
- You also have the usual tools (a shell, files). Your working folder is your own scratch space, shared with nobody else's run at the same time. To work on a repository, make a git worktree of it under your folder and work there (a shell's cd only lasts inside your folder; elsewhere, use absolute paths and git -C). Read the repository's AGENTS.md or CLAUDE.md first, since you were not started in it. Clean up worktrees you no longer need.
- Each prompt is what is new in your item: the operator's messages as they wrote them, and anything else (other events, results, instructions) in [square brackets]. get_item has the full history.
- Do what the operator's latest messages ask, as far as you can without them.
- Actions that change things outside this app are unsafe, and you never do them: not with the MCP tools (you do not have the unsafe ones; unsafe_actions lists them), and not with the shell either (no posting, commenting, reviewing, merging, pushing to shared branches, or sending). To get one done, add an approval event, filled in as far as you can.
- When you stop, return your output: a message to the operator if there is something to say, the blockers to add, and whether the item is done. Chain blockers with after events: e.g. a github wait, then an after whose script adds a bot if the checks failed.
- To wait for something no other event type covers, add a script event: a function the app runs on a schedule, which checks with the actions and says when it is done. event_types shows how to write one.
- To split work off, add a child item with its first events (e.g. a message and a bot). It blocks this item until it closes, and its bot starts from this conversation.
- For work unrelated to this item, add an item with detached: true. It sits in no item and blocks nothing.
- Ask the operator only what nobody else can answer, as one direct question: no preamble, no summary, no waffle.`

export const spawnClaude: SpawnBot = ({ prompt, system, schema, mcpUrl, sessionId, resume, forkFrom, cwd, onOutput }) => {
  const mcp = JSON.stringify({ mcpServers: { workflows: { type: 'http', url: mcpUrl } } })
  const child = spawn(
    'claude',
    ['-p', prompt,
      ...(resume ? ['--resume', sessionId] : forkFrom ? ['--resume', forkFrom, '--fork-session', '--session-id', sessionId] : ['--session-id', sessionId]),
      '--append-system-prompt', system, '--mcp-config', mcp, '--strict-mcp-config',
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

/** How often the prompt reminds the bot which item it is on: every this many lines, starting with the first. */
const ID_EVERY = 10

/** The human, or an outside client acting as them: their words go to the bot as they are. */
const HUMAN = new Set(['human', 'mcp'])

/**
 * One event as a line of the prompt: what the human said, as they said it;
 * anything else in square brackets. A bare bot event (the "a bot, please" that
 * comes with a message) and an after that ran say nothing.
 */
export function promptLine(e: ItemEvent): string | null {
  if (e.type === 'message') return HUMAN.has(e.createdBy) ? String(e.params.text ?? '') : `[${e.createdBy}: ${e.params.text ?? ''}]`
  // A summary is for the human; the bot has the events themselves.
  if (e.type === 'summary') return null
  if (e.type === 'bot') return e.params.instructions ? `[instructions: ${e.params.instructions}]` : null
  // An after that ran says nothing of its own: what it added is in the prompt too.
  if (e.type === 'after' && e.resolved && !e.cancelled && !e.data?.error) return null
  if (e.cancelled) return `[${e.message ?? `Cancelled: ${e.summary}`}]`
  if (!e.resolved) return `[${e.type} added by ${e.createdBy}: ${e.summary}]`
  return `[${e.message ?? `${e.summary}: done`}]`
}

/**
 * The run's prompt: what is new since the bot was last prompted, with little
 * direction — the human's messages as they are, anything else in brackets — and,
 * every ID_EVERY lines from the first, which item it is on. The first time, the
 * item's title (and, when the session was forked from the item above, that it
 * is a new one) comes first.
 */
export function promptFor(engine: Engine, item: Item, _bots: ItemEvent[], resume: boolean, forked: boolean): string {
  const events = engine.newSinceLastRun(item.id).map(promptLine).filter((l): l is string => l !== null)
  const head = resume ? [] : [forked ? `[new item, split off from the one above (this conversation is its own from here on): ${item.title}]` : `[new item: ${item.title}]`]
  const lines = [...head, ...events]
  if (!events.length) lines.push('[nothing new]')
  return lines
    .flatMap((l, i) => (i % ID_EVERY === 0 ? [`[current item ID: ${item.id}]`, l] : [l]))
    .join('\n\n')
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

export class WorkerPool {
  private running = new Map<number, { itemId: string; kill: () => void }>()
  private timer: NodeJS.Timeout | null = null

  constructor(
    private engine: Engine,
    private mcpUrl: (runId: string) => string,
    readonly size = 4,
    private spawnBot: SpawnBot = spawnClaude,
  ) {
    engine.on('change', () => this.schedule())
    engine.onCancel = (itemId) => {
      for (const r of this.running.values()) if (r.itemId === itemId) r.kill()
    }
  }

  status(): { id: number; itemId: string | null }[] {
    return Array.from({ length: this.size }, (_, id) => ({ id, itemId: this.running.get(id)?.itemId ?? null }))
  }

  /** Fills idle workers, once whatever change set this off has settled. */
  schedule(): void {
    if (this.timer) return
    this.timer = setTimeout(() => {
      this.timer = null
      this.fill()
    }, 0)
  }

  fill(): void {
    for (let w = 0; w < this.size; w++) {
      if (this.running.has(w)) continue
      const claimed = this.engine.claim(w)
      if (!claimed) return
      this.run(w, claimed.item, claimed.events, claimed.sessionId, claimed.resume, claimed.forkFrom)
    }
  }

  private run(worker: number, item: Item, bots: ItemEvent[], sessionId: string, resume: boolean, forkFrom: string | null): void {
    const prompt = promptFor(this.engine, item, bots, resume, forkFrom !== null)
    const runId = this.engine.startRun(item, bots, worker, prompt)
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
      proc = this.spawnBot({ prompt, system: SYSTEM, schema: botOutputSchema(this.engine.eventTypes), mcpUrl: this.mcpUrl(runId), sessionId, resume, forkFrom, cwd: workerFolder(worker), onOutput })
    } catch (e) {
      void end(null, (e as Error).message)
      return
    }
    const limit = setTimeout(() => proc.kill(), RUN_LIMIT_MS)
    this.running.set(worker, { itemId: item.id, kill: proc.kill })
    proc.done
      .then(
        ({ exitCode }) => end(exitCode),
        (e: Error) => end(null, e.message),
      )
      .finally(() => {
        clearTimeout(limit)
        this.running.delete(worker)
        this.schedule()
      })
  }
}
