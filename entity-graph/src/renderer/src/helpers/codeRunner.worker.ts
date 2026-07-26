// A sandboxed QuickJS runtime living in a Web Worker. Running the VM off the
// main thread is what makes the Stop button real: a runaway script (e.g.
// `while (true) {}`) can't freeze the UI, and the main thread stops it simply by
// terminating this worker. The VM itself has no ambient authority — no DOM, no
// `fetch`, no `require` — only what we inject: `console`, the `context` the
// entity was run in, and `tool`, which is how it reaches everything else.
//
// `tool.sendSlackMessage(channel, text)` is a *synchronous* call, and this worker
// is where that is bought: the call is posted to the main thread, which owns the
// tool registry, and this thread then blocks on a shared word until the answer is
// written back beside it. A promise would be the ordinary way to do it, but a
// script here is a few lines in a bullet, and `await` in every one of them is a
// tax on the common case.
import {
  newQuickJSWASMModuleFromVariant,
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
  /** Folded entity values, injected as the script's `context`. */
  context: Record<string, unknown>
}

/** The worker's answer to one {@link RunRequest}. */
export interface RunResponse {
  kind: 'result'
  id: string
  ok: boolean
  logs: string[]
  /** The completion value, JSON-dumped. Only meaningful when `hasResult`. */
  result?: unknown
  hasResult?: boolean
  error?: string
}

/**
 * A tool call a script has made and is blocked on. The two shared buffers are how
 * the answer comes back: `control` is the word this thread waits on plus the
 * reply's byte length, and `reply` takes the JSON itself.
 */
export interface ToolRequest {
  kind: 'tool'
  /** The run that made the call — which context it was made in. */
  id: string
  name: string
  /** Positional arguments, exactly as the script passed them. */
  args: unknown[]
  control: Int32Array
  reply: Uint8Array
}

export type WorkerMessage = RunResponse | ToolRequest

/** What the main thread writes back: the tool's result, or why there isn't one. */
export type ToolReply = { ok: true; value?: unknown } | { ok: false; error: string }

// A hard ceiling so a wedged VM can't hold the worker forever even if the user
// never presses Stop. The Stop button terminates the worker long before this.
// Time spent blocked in a tool call doesn't count against it — a script that
// waits two minutes on Claude has not run away.
const DEADLINE_MS = 10_000
const MEMORY_LIMIT = 64 * 1024 * 1024

/** How long a single tool call may block before the script is told it failed. */
const TOOL_TIMEOUT_MS = 5 * 60_000

// The shared cells the bridge runs over, allocated once and reused: this worker
// runs one script at a time, and one call within it at a time, so a call that has
// been answered leaves nothing behind to collide with the next (a call that timed
// out does, and gives up its buffers accordingly). Room for a large result — a
// hundred pull requests — and an error rather than a truncation past that.
const REPLY_BYTES = 4 * 1024 * 1024
const WAITING = 0
const ANSWERED = 1

let modulePromise: Promise<QuickJSWASMModule> | null = null
const getModule = (): Promise<QuickJSWASMModule> =>
  (modulePromise ??= newQuickJSWASMModuleFromVariant(variant))

const post = (message: WorkerMessage): void => (self as unknown as Worker).postMessage(message)

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

/**
 * What went wrong, as one line. A thrown Error dumps to an object carrying its
 * stack, and the stack is the sandbox's rather than the script's — so the message
 * is all there is worth reading, and a failed tool call ("Channel is required")
 * should say that and nothing else.
 */
function formatThrown(value: unknown): string {
  const { name, message } = (value ?? {}) as { name?: unknown; message?: unknown }
  if (typeof message !== 'string') return format(value)
  return typeof name === 'string' && name ? `${name}: ${message}` : message
}

// --- The synchronous bridge -------------------------------------------------

interface Bridge {
  control: Int32Array
  reply: Uint8Array
}

let bridge: Bridge | null = null

function ensureBridge(): Bridge {
  if (bridge) return bridge
  // Blocking on a word only works if the main thread can see it. Without
  // SharedArrayBuffer there is no way to make a synchronous call at all, so say
  // so plainly rather than failing somewhere further in.
  if (typeof SharedArrayBuffer === 'undefined') {
    throw new Error('Tools are unavailable here: this window has no SharedArrayBuffer')
  }
  bridge = {
    control: new Int32Array(new SharedArrayBuffer(2 * 4)),
    reply: new Uint8Array(new SharedArrayBuffer(REPLY_BYTES)),
  }
  return bridge
}

/** Milliseconds this run has spent blocked on tool calls. */
let blocked = 0

/**
 * Ask the main thread to run a tool and wait for the answer, blocking this
 * thread. Returns the reply as JSON, which the script-side wrapper unpacks — the
 * host only ever hands the VM a string.
 */
function callTool(id: string, name: string, argsJson: string): string {
  const { control, reply } = ensureBridge()
  let args: unknown[]
  try {
    args = JSON.parse(argsJson) as unknown[]
  } catch {
    return JSON.stringify({ ok: false, error: `Arguments to ${name} must be JSON values` })
  }
  Atomics.store(control, 0, WAITING)
  Atomics.store(control, 1, 0)
  post({ kind: 'tool', id, name, args, control, reply })
  const startedAt = Date.now()
  const state = Atomics.wait(control, 0, WAITING, TOOL_TIMEOUT_MS)
  blocked += Date.now() - startedAt
  if (state === 'timed-out' && Atomics.load(control, 0) !== ANSWERED) {
    // These buffers are now spoken for: the call may still answer into them long
    // after this, and that answer must not be mistaken for the next call's. The
    // next one allocates a fresh pair, which is why reuse is safe otherwise.
    bridge = null
    return JSON.stringify({ ok: false, error: `${name} did not answer within five minutes` })
  }
  const length = Atomics.load(control, 1)
  // `slice` copies out of shared memory into a buffer of our own, which is what
  // the decoder will take.
  return new TextDecoder().decode(reply.slice(0, length))
}

/**
 * The script-side half of the bridge, and the whole of the `tool` façade. A
 * proxy rather than an object of injected functions: names are resolved on the
 * main thread, against a registry that isn't fixed at build time (the server's
 * integrations join it when a source opens), so there is no list to inject.
 */
const PRELUDE = `
globalThis.context = Object.freeze(JSON.parse(__contextJson))
globalThis.tool = new Proxy({}, {
  get(_target, name) {
    if (typeof name !== 'string') return undefined
    return (...args) => {
      const reply = JSON.parse(__callTool(name, JSON.stringify(args)))
      if (!reply.ok) throw new Error(reply.error)
      return reply.value
    }
  },
})
`

async function run(req: RunRequest): Promise<RunResponse> {
  const { id, code } = req
  const QuickJS = await getModule()
  const logs: string[] = []

  const runtime = QuickJS.newRuntime()
  runtime.setMemoryLimit(MEMORY_LIMIT)
  blocked = 0
  const startedAt = Date.now()
  runtime.setInterruptHandler(() => Date.now() > startedAt + DEADLINE_MS + blocked)
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

    // The two things the prelude builds `context` and `tool` out of. Both cross
    // as strings: JSON in, JSON out, so nothing has to be marshalled by hand.
    const contextJson = ctx.newString(json(req.context ?? {}))
    ctx.setProp(ctx.global, '__contextJson', contextJson)
    contextJson.dispose()
    const callFn = ctx.newFunction('__callTool', (nameHandle, argsHandle) =>
      ctx.newString(callTool(id, ctx.getString(nameHandle), ctx.getString(argsHandle))),
    )
    ctx.setProp(ctx.global, '__callTool', callFn)
    callFn.dispose()

    const prelude = ctx.evalCode(PRELUDE)
    if (prelude.error) {
      const err = ctx.dump(prelude.error)
      prelude.error.dispose()
      return { kind: 'result', id, ok: false, logs, error: formatThrown(err) }
    }
    prelude.value.dispose()

    const result = ctx.evalCode(code)
    if (result.error) {
      const err = ctx.dump(result.error)
      result.error.dispose()
      return { kind: 'result', id, ok: false, logs, error: formatThrown(err) }
    }
    const value = ctx.dump(result.value)
    result.value.dispose()
    return { kind: 'result', id, ok: true, logs, result: value, hasResult: value !== undefined }
  } catch (err) {
    return { kind: 'result', id, ok: false, logs, error: err instanceof Error ? err.message : String(err) }
  } finally {
    ctx.dispose()
    runtime.dispose()
  }
}

/** JSON that always parses back to something, whatever it was handed. */
function json(value: unknown): string {
  try {
    return JSON.stringify(value) ?? 'null'
  } catch {
    return 'null'
  }
}

self.onmessage = async (e: MessageEvent<RunRequest>): Promise<void> => {
  post(await run(e.data))
}
