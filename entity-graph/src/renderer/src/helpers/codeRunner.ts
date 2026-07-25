import { atom } from '../state/atom'
import type { RunRequest, RunResponse } from './codeRunner.worker'

// Local execution of `type: code` entities in one sandboxed QuickJS worker.
// A browser service rather than state: the results are runtime-only and never
// written back, and the worker is created lazily on the first run and torn down
// on Stop (terminating it is how a runaway script is interrupted).
//
// v0 caveat: one worker runs scripts sequentially, and Stop kills it — so
// stopping one run aborts any other in flight. In practice one runs at a time.

export type CodeRunState =
  | { status: 'running' }
  | { status: 'done'; logs: string[]; result?: unknown; hasResult: boolean }
  | { status: 'error'; logs: string[]; error: string }

export const codeRunsAtom = atom<Record<string, CodeRunState>>({})

const setRun = (id: string, state: CodeRunState): void =>
  codeRunsAtom.set((runs) => ({ ...runs, [id]: state }))

let worker: Worker | null = null

function ensureWorker(): Worker {
  if (worker) return worker
  const next = new Worker(new URL('./codeRunner.worker.ts', import.meta.url), { type: 'module' })
  next.onmessage = (e: MessageEvent<RunResponse>): void => {
    const r = e.data
    if (r.ok) setRun(r.id, { status: 'done', logs: r.logs, result: r.result, hasResult: !!r.hasResult })
    else setRun(r.id, { status: 'error', logs: r.logs, error: r.error ?? 'Error' })
  }
  worker = next
  return next
}

export function runCode(id: string, code: string): void {
  setRun(id, { status: 'running' })
  const request: RunRequest = { id, code }
  ensureWorker().postMessage(request)
}

/** Interrupt whatever is running by killing the worker; the next run respawns it. */
export function stopCode(): void {
  worker?.terminate()
  worker = null
  codeRunsAtom.set((runs) => {
    const next = { ...runs }
    for (const [id, state] of Object.entries(next)) {
      if (state.status === 'running') next[id] = { status: 'error', logs: [], error: 'Interrupted' }
    }
    return next
  })
}

export const isRunning = (runs: Record<string, CodeRunState>, id: string): boolean =>
  runs[id]?.status === 'running'
