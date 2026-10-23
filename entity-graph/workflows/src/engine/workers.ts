// A fixed number of workers, each running one `claude -p` at a time on the
// bot handoff that has waited longest. A run ends by returning its next
// handoffs as structured output, which `Engine.endRun` takes from there.

import { spawn } from 'node:child_process'
import type { Handoff, Item } from '../shared/types'
import { describeResolution, type Engine } from './engine'
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
  onOutput: (chunk: string) => void
}) => BotProcess

const RUN_LIMIT_MS = 30 * 60_000

export const SYSTEM = `You are one of several workers in the operator's workflow system. You work one handoff on one item.

- Use the tools of the "workflows" MCP server. They are all you have. handoff_types describes every kind of handoff.
- Do what your instructions ask, as far as you can without the human.
- Actions that change things outside this app are unsafe, and you do not have them. unsafe_actions lists them. To get one done, return a human handoff with an approval, filled in as far as you can.
- When you stop, return your output: the handoffs that come next, in order. Chain them: e.g. a github wait, then a script that routes on its result.
- Hand to the human only what nobody else can do. Their "needed" is one direct question or request: no preamble, no summary, no waffle.`

export const spawnClaude: SpawnBot = ({ prompt, system, schema, mcpUrl, onOutput }) => {
  const mcp = JSON.stringify({ mcpServers: { workflows: { type: 'http', url: mcpUrl } } })
  const child = spawn(
    'claude',
    ['-p', prompt, '--append-system-prompt', system, '--mcp-config', mcp, '--strict-mcp-config',
      '--tools', '', '--allowedTools', 'mcp__workflows', '--permission-mode', 'dontAsk',
      '--json-schema', JSON.stringify(schema), '--output-format', 'stream-json', '--verbose'],
    { stdio: ['ignore', 'pipe', 'pipe'] },
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

export function promptFor(engine: Engine, item: Item, handoff: Handoff): string {
  const all = engine.handoffs(item.id)
  const resolved = all.filter((h) => h.status === 'resolved')
  // Back to the last time a person or a bot had it: the waits and scripts since are why this run is happening.
  const lastHand = resolved.findLastIndex((h) => h.type === 'human' || h.type === 'bot')
  const since = resolved.slice(Math.max(lastHand, 0))
  const queued = all.filter((h) => h.status === 'queued')
  const brief = (h: Handoff) => {
    const { source: _, ...params } = h.params
    return JSON.stringify({ type: h.type, params, resolution: h.resolution && { ...h.resolution, output: undefined } })
  }
  return [
    `Item ${item.id}: ${item.title}`,
    item.url && `Link: ${item.url}`,
    `Your instructions: ${String(handoff.params.instructions)}`,
    since.length && `What came before, oldest first:\n${since.map((h) => `- ${describeResolution(h)} ${brief(h)}`).join('\n')}`,
    queued.length && `Already queued after you (your handoffs go in front of these):\n${queued.map((h) => JSON.stringify({ type: h.type, params: h.params })).join('\n')}`,
    'get_item has the full history.',
  ]
    .filter(Boolean)
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
      this.run(w, claimed.item, claimed.handoff)
    }
  }

  private run(worker: number, item: Item, handoff: Handoff): void {
    const prompt = promptFor(this.engine, item, handoff)
    const runId = this.engine.startRun(item, handoff, worker, prompt)
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
      proc = this.spawnBot({ prompt, system: SYSTEM, schema: botOutputSchema(this.engine.handoffTypes), mcpUrl: this.mcpUrl(runId), onOutput })
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
