// The processes `process` events run: held by the backend while it lasts, each
// with the tail of its output. Nothing here is stored: a process outlives
// neither its event's resolving nor the backend stopping.

import { spawn, type ChildProcess } from 'node:child_process'
import type { ProcessStatus } from '../shared/types'

/** How many lines of a process's output are kept. */
const KEEP = 500

interface Held extends ProcessStatus {
  child: ChildProcess | null
  lines: string[]
  /** What came after the last newline, from stdout and stderr each. */
  partial: { out: string; err: string }
}

export class Processes {
  private held = new Map<string, Held>()

  constructor(private now: () => number = Date.now) {}

  /** Starts `command` in a shell, in `cwd`, for the event `id`. */
  start(id: string, command: string, cwd: string): void {
    if (this.held.get(id)?.running) return
    const h: Held = {
      running: true, pid: null, exitCode: null, signal: null, error: null, startedAt: this.now(), endedAt: null,
      child: null, lines: [], partial: { out: '', err: '' },
    }
    this.held.set(id, h)
    // Its own process group, so stopping it stops what it started (a dev server's children) too.
    const child = spawn('bash', ['-lc', command], { cwd, detached: true, stdio: ['ignore', 'pipe', 'pipe'] })
    h.child = child
    h.pid = child.pid ?? null
    const take = (which: 'out' | 'err') => (chunk: Buffer) => {
      const parts = (h.partial[which] + chunk.toString()).split('\n')
      h.partial[which] = parts.pop() ?? ''
      h.lines.push(...parts)
      if (h.lines.length > KEEP) h.lines.splice(0, h.lines.length - KEEP)
    }
    child.stdout?.on('data', take('out'))
    child.stderr?.on('data', take('err'))
    const end = (code: number | null, signal: string | null, error: string | null = null) => {
      if (!h.running) return
      for (const w of ['out', 'err'] as const) if (h.partial[w]) h.lines.push(h.partial[w])
      Object.assign(h, { running: false, exitCode: code, signal, error, endedAt: this.now(), child: null })
    }
    child.on('exit', (code, signal) => end(code, signal))
    child.on('error', (e) => end(null, null, e.message))
  }

  /** How it stands, or null for a process this backend never started (it ran under one that has stopped). */
  status(id: string): ProcessStatus | null {
    const h = this.held.get(id)
    if (!h) return null
    const { child: _c, lines: _l, partial: _p, ...status } = h
    return status
  }

  /** The last `n` lines it wrote, stdout and stderr together. */
  output(id: string, n = KEEP): string[] {
    return this.held.get(id)?.lines.slice(-n) ?? []
  }

  /** Stops it and what it started: SIGTERM, then SIGKILL if it is still there after a few seconds. */
  stop(id: string): void {
    const h = this.held.get(id)
    if (!h?.running || !h.child?.pid) return
    const group = -h.child.pid
    const kill = (signal: NodeJS.Signals) => {
      try {
        process.kill(group, signal)
      } catch {
        // Gone already.
      }
    }
    kill('SIGTERM')
    setTimeout(() => h.running && kill('SIGKILL'), 5_000).unref()
  }

  /** Stops every process: the backend is stopping. */
  stopAll(): void {
    for (const id of this.held.keys()) this.stop(id)
  }
}
