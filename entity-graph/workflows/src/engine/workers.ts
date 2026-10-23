// A fixed number of workers, each running one `claude -p` at a time on the
// agent-blocked item that has waited longest.

import { spawn } from 'node:child_process'
import type { Item } from '../shared/types'
import type { Engine } from './engine'

export interface AgentProcess {
  done: Promise<{ exitCode: number | null }>
  kill: () => void
}

/** Starts one agent; `onOutput` gets its stdout as it comes. Handed in, so the tests needn't run claude. */
export type SpawnAgent = (args: { prompt: string; system: string; mcpUrl: string; onOutput: (chunk: string) => void }) => AgentProcess

const RUN_LIMIT_MS = 30 * 60_000

export const SYSTEM = `You are one of several workers in the operator's workflow system. You work on exactly one item.

- Use the tools of the "workflows" MCP server. They are all you have.
- Start with get_item: it has the instruction, the state, and what happened so far.
- Do what the operator's instruction asks, as far as you can without them.
- Keep the item's state current with set_state. Record anything worth keeping with note.
- Never act where other people can see without the operator's approval: use request_approval, filled in as far as you can.
- End your run with exactly one of: ask_operator, request_approval, wait_for, wait_for_items, close_item. Then stop.`

export const spawnClaude: SpawnAgent = ({ prompt, system, mcpUrl, onOutput }) => {
  const mcp = JSON.stringify({ mcpServers: { workflows: { type: 'http', url: mcpUrl } } })
  const child = spawn(
    'claude',
    ['-p', prompt, '--append-system-prompt', system, '--mcp-config', mcp, '--strict-mcp-config',
      '--tools', '', '--allowedTools', 'mcp__workflows', '--permission-mode', 'dontAsk',
      '--output-format', 'stream-json', '--verbose'],
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

export function promptFor(engine: Engine, item: Item): string {
  const recent = engine
    .events(item.id)
    .slice(-12)
    .map((e) => `- ${new Date(e.at).toISOString()} ${e.actor} ${e.kind}: ${e.text}`)
    .join('\n')
  return [
    `Item ${item.id}: ${item.title}`,
    item.url && `Link: ${item.url}`,
    `State: ${item.state || '(none yet)'}`,
    `Operator's instruction: ${item.instruction ?? '(none)'}`,
    `Latest history (the last line is why you are running now):\n${recent}`,
  ]
    .filter(Boolean)
    .join('\n\n')
}

export class WorkerPool {
  private running = new Map<number, { itemId: string; kill: () => void }>()
  private timer: NodeJS.Timeout | null = null

  constructor(
    private engine: Engine,
    private mcpUrl: (runId: string) => string,
    readonly size = 4,
    private spawnAgent: SpawnAgent = spawnClaude,
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
      const item = this.engine.claim(w)
      if (!item) return
      this.run(w, item)
    }
  }

  private run(worker: number, item: Item): void {
    const prompt = promptFor(this.engine, item)
    const runId = this.engine.startRun(item.id, worker, prompt)
    let buffer = ''
    let last: { total_cost_usd?: number; num_turns?: number; result?: string } = {}
    const onOutput = (chunk: string) => {
      this.engine.appendTranscript(runId, chunk)
      buffer += chunk
      const lines = buffer.split('\n')
      buffer = lines.pop() ?? ''
      for (const line of lines) {
        try {
          const msg = JSON.parse(line)
          if (msg.type === 'result') last = msg
        } catch {
          // not a whole message; kept in the transcript regardless
        }
      }
    }
    let proc: AgentProcess
    try {
      proc = this.spawnAgent({ prompt, system: SYSTEM, mcpUrl: this.mcpUrl(runId), onOutput })
    } catch (e) {
      this.engine.endRun(runId, { exitCode: null, error: (e as Error).message })
      return
    }
    const limit = setTimeout(() => proc.kill(), RUN_LIMIT_MS)
    this.running.set(worker, { itemId: item.id, kill: proc.kill })
    proc.done
      .then(
        ({ exitCode }) =>
          this.engine.endRun(runId, { exitCode, costUsd: last.total_cost_usd, turns: last.num_turns, result: last.result }),
        (e: Error) => this.engine.endRun(runId, { exitCode: null, error: e.message }),
      )
      .finally(() => {
        clearTimeout(limit)
        this.running.delete(worker)
        this.schedule()
      })
  }
}
