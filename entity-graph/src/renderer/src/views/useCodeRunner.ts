import { useCallback, useEffect, useRef, useState } from 'react'
import type { RunRequest, RunResponse } from './codeRunner.worker'

// ---------------------------------------------------------------------------
// Local (non-persisted) run state for `type: code` entities.
// ---------------------------------------------------------------------------

export type CodeRunState =
  | { status: 'running' }
  | { status: 'done'; logs: string[]; result?: unknown; hasResult: boolean }
  | { status: 'error'; logs: string[]; error: string }

export interface CodeRunner {
  /** Latest result keyed by entity id. Purely local — never written back. */
  runs: Map<string, CodeRunState>
  run: (id: string, code: string) => void
  stop: (id: string) => void
  isRunning: (id: string) => boolean
}

/**
 * Owns a single QuickJS worker and the local run results for the entities in
 * one editor. The worker is created lazily on first run and torn down on Stop
 * (terminating it is how we interrupt a runaway script) or unmount; it's
 * recreated on the next run.
 *
 * v0 caveat: one worker per editor runs scripts sequentially, and Stop
 * terminates the worker — so stopping one run also aborts any other in flight.
 * In practice you run one block at a time; note it if that changes.
 */
export function useCodeRunner(): CodeRunner {
  const [runs, setRuns] = useState<Map<string, CodeRunState>>(new Map())
  const workerRef = useRef<Worker | null>(null)

  const setRun = useCallback((id: string, state: CodeRunState) => {
    setRuns((prev) => {
      const next = new Map(prev)
      next.set(id, state)
      return next
    })
  }, [])

  const ensureWorker = useCallback((): Worker => {
    if (workerRef.current) return workerRef.current
    const worker = new Worker(new URL('./codeRunner.worker.ts', import.meta.url), {
      type: 'module',
    })
    worker.onmessage = (e: MessageEvent<RunResponse>): void => {
      const r = e.data
      if (r.ok) setRun(r.id, { status: 'done', logs: r.logs, result: r.result, hasResult: !!r.hasResult })
      else setRun(r.id, { status: 'error', logs: r.logs, error: r.error ?? 'Error' })
    }
    workerRef.current = worker
    return worker
  }, [setRun])

  const teardownWorker = useCallback(() => {
    workerRef.current?.terminate()
    workerRef.current = null
  }, [])

  const run = useCallback(
    (id: string, code: string) => {
      setRun(id, { status: 'running' })
      const worker = ensureWorker()
      const msg: RunRequest = { id, code }
      worker.postMessage(msg)
    },
    [ensureWorker, setRun],
  )

  const stop = useCallback(
    (_id: string) => {
      // Terminating the shared worker aborts whatever it's executing; mark every
      // still-running entry as interrupted, then let the next run respawn it.
      teardownWorker()
      setRuns((prev) => {
        const next = new Map(prev)
        for (const [key, state] of next) {
          if (state.status === 'running') next.set(key, { status: 'error', logs: [], error: 'Interrupted' })
        }
        return next
      })
    },
    [teardownWorker],
  )

  const isRunning = useCallback((id: string) => runs.get(id)?.status === 'running', [runs])

  useEffect(() => () => teardownWorker(), [teardownWorker])

  return { runs, run, stop, isRunning }
}
