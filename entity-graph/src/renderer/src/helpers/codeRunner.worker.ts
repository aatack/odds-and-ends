// A sandboxed QuickJS runtime living in a Web Worker. Running the VM off the
// main thread is what makes the Stop button real: a runaway script (e.g.
// `while (true) {}`) can't freeze the UI, and the main thread stops it simply by
// terminating this worker. The VM itself has no ambient authority — no DOM, no
// `fetch`, no `require` — only what we inject (currently just `console`).
//
// v0 captures `console.log`/`warn`/`error`/`info` output and the completion
// value. A graph API (query/writeValue/…) would be added here as async host
// functions, marshalled back to the main thread.
import {
  newQuickJSWASMModuleFromVariant,
  shouldInterruptAfterDeadline,
  type QuickJSWASMModule,
  type QuickJSContext,
  type QuickJSHandle,
} from 'quickjs-emscripten-core'
// The singlefile variant inlines the WASM as base64, so there's no separate
// asset to locate — the one thing that reliably works inside a bundled worker.
import variant from '@jitl/quickjs-singlefile-browser-release-sync'

export interface RunRequest {
  id: string
  code: string
}

export interface RunResponse {
  id: string
  ok: boolean
  logs: string[]
  /** The completion value, JSON-dumped. Only meaningful when `hasResult`. */
  result?: unknown
  hasResult?: boolean
  error?: string
}

// A hard ceiling so a wedged VM can't hold the worker forever even if the user
// never presses Stop. The Stop button terminates the worker long before this.
const DEADLINE_MS = 10_000
const MEMORY_LIMIT = 64 * 1024 * 1024

let modulePromise: Promise<QuickJSWASMModule> | null = null
const getModule = (): Promise<QuickJSWASMModule> =>
  (modulePromise ??= newQuickJSWASMModuleFromVariant(variant))

// Turn a VM-side handle into a printable string. Objects/arrays pretty-print as
// JSON; primitives use their natural form. `dump` deep-converts to a plain JS
// value we own.
function stringifyHandle(ctx: QuickJSContext, handle: QuickJSHandle): string {
  const value = ctx.dump(handle)
  return format(value)
}

function format(value: unknown): string {
  if (typeof value === 'string') return value
  if (value === undefined) return 'undefined'
  try {
    return JSON.stringify(value, null, 2) ?? String(value)
  } catch {
    return String(value)
  }
}

async function run(req: RunRequest): Promise<RunResponse> {
  const { id, code } = req
  const QuickJS = await getModule()
  const logs: string[] = []

  const runtime = QuickJS.newRuntime()
  runtime.setMemoryLimit(MEMORY_LIMIT)
  runtime.setInterruptHandler(shouldInterruptAfterDeadline(Date.now() + DEADLINE_MS))
  const ctx = runtime.newContext()

  try {
    // Inject a minimal `console` whose methods collect their arguments as text.
    const consoleObj = ctx.newObject()
    for (const method of ['log', 'info', 'warn', 'error'] as const) {
      const fn = ctx.newFunction(method, (...args) => {
        logs.push(args.map((a) => stringifyHandle(ctx, a)).join(' '))
      })
      ctx.setProp(consoleObj, method, fn)
      fn.dispose()
    }
    ctx.setProp(ctx.global, 'console', consoleObj)
    consoleObj.dispose()

    const result = ctx.evalCode(code)
    if (result.error) {
      const err = ctx.dump(result.error)
      result.error.dispose()
      return { id, ok: false, logs, error: format(err) }
    }
    const value = ctx.dump(result.value)
    result.value.dispose()
    return { id, ok: true, logs, result: value, hasResult: value !== undefined }
  } catch (err) {
    return { id, ok: false, logs, error: err instanceof Error ? err.message : String(err) }
  } finally {
    ctx.dispose()
    runtime.dispose()
  }
}

self.onmessage = async (e: MessageEvent<RunRequest>): Promise<void> => {
  const response = await run(e.data)
  ;(self as unknown as Worker).postMessage(response)
}
