import type { CallContext } from '../state/types'
import type {
  RunRequest,
  RunResponse,
  ToolReply,
  ToolRequest,
  WorkerMessage,
} from './codeRunner.worker'

// Local execution of scripts in sandboxed QuickJS workers. A browser
// service rather than state: the results are runtime-only and never written back,
// and workers are made lazily and killed on Stop (terminating one is how a runaway
// script is interrupted).
//
// This side also answers the scripts' tool calls. A script calls a tool
// synchronously (see the worker for why), which it can only do by blocking its
// own thread — so the work happens here, on the thread that owns the registry,
// and the answer is written into the shared buffers the worker is waiting on.
//
// A blocked worker is why there is a pool rather than one worker; see it below.
// Stop kills all of them, so stopping one run aborts any other in flight.

// A pool, and it has to be one. A script blocks its own thread while a tool call
// is answered — that is what buys `tool.x()` its lack of `await` — so a worker
// waiting on a call cannot pick up another script in the meantime. And answering
// a call is exactly when another script may need to run: a tool may itself be a
// script, and calling it would otherwise post it to the very worker that is
// blocked waiting for it, which is a deadlock and looks
// like a call that never comes back.
//
// So a run takes a worker for itself and gives it back when it settles. Nesting
// is what grows the pool; sequential runs reuse the one worker and pay for the
// WASM module once.

/** Free to take. Kept rather than terminated: starting one is not cheap. */
const idle: Worker[] = []

/** Every worker this module has made and not killed, busy or idle. */
const live = new Set<Worker>()

/**
 * How deep the nesting may go. A tool that calls itself would otherwise spawn
 * workers until the tab dies; this turns that into an error naming what happened.
 * Well past any honest nesting — a script calling a tool that calls a tool.
 */
const MAX_WORKERS = 8

/**
 * The context each running script was started in, by run id. Kept here rather
 * than sent along with every tool call: the script is given a copy of its values,
 * but a call it makes is recorded against the whole context, which is not the
 * sandbox's to hand back.
 */
const contexts = new Map<string, CallContext>()

/** Run id → whoever is waiting for that run to finish. */
const waiting = new Map<string, (r: RunResponse) => void>()

/** A worker to run one script on, or null when too many are already nested. */
function takeWorker(): Worker | null {
  const spare = idle.pop()
  if (spare) return spare
  if (live.size >= MAX_WORKERS) return null
  const next = new Worker(new URL('./codeRunner.worker.ts', import.meta.url), { type: 'module' })
  next.onmessage = (e: MessageEvent<WorkerMessage>): void => {
    const r = e.data
    if (r.kind === 'tool') {
      void answerTool(r)
      return
    }
    contexts.delete(r.id)
    const settle = waiting.get(r.id)
    waiting.delete(r.id)
    settle?.(r)
  }
  live.add(next)
  return next
}

/** Hand a settled worker back, unless Stop has since killed everything. */
const releaseWorker = (worker: Worker): void => {
  if (live.has(worker)) idle.push(worker)
}

/** Hand a script to the sandbox and wait for whatever it comes back with. */
function execute(id: string, code: string, context: CallContext): Promise<RunResponse> {
  return new Promise((resolve) => {
    const worker = takeWorker()
    if (!worker) {
      resolve({
        kind: 'result',
        id,
        ok: false,
        logs: [],
        error: `More than ${MAX_WORKERS} scripts are waiting on one another — is a tool calling itself?`,
      })
      return
    }
    contexts.set(id, context)
    waiting.set(id, (r) => {
      releaseWorker(worker)
      resolve(r)
    })
    const request: RunRequest = { id, code, context: context.values }
    worker.postMessage(request)
  })
}

/** Distinguishes one run of the same script from another. */
let scriptRuns = 0

/**
 * Run a script and hand back what it evaluated to, and what it logged. `name`
 * says what the script is, for the run id; it need not be unique, since each run
 * is counted.
 *
 * The context is the caller's, passed straight through: what the script goes on
 * to do is recorded against the frame the user was looking at.
 */
export async function runScript(
  name: string,
  code: string,
  context: CallContext,
): Promise<{ result: unknown; logs: string[] }> {
  // Counted, because the run id is what an answer finds its way back by, so two
  // runs of one script must not share it.
  const response = await execute(`${name}#${++scriptRuns}`, code, context)
  if (!response.ok) throw new Error(response.error ?? 'Error')
  return { result: response.result, logs: response.logs }
}

/** Interrupt everything running by killing the workers; the next run respawns one. */
export function stopCode(): void {
  for (const worker of live) worker.terminate()
  // Emptied before anything is settled, so a settling run doesn't hand a worker
  // that has just been killed back to the pool.
  live.clear()
  idle.length = 0
  contexts.clear()
  // Anything the worker was holding will never answer now, so settle it here
  // rather than leave a caller waiting on a thread that no longer exists.
  for (const [id, settle] of waiting) {
    settle({ kind: 'result', id, ok: false, logs: [], error: 'Interrupted' })
  }
  waiting.clear()
}

// --- Answering a script's tool call -----------------------------------------

const ANSWERED = 1

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

/**
 * Run the tool a blocked script asked for, and unblock it. The tool machine is
 * imported here rather than at the top of the file so that this helper, which the
 * tools themselves depend on, doesn't have to be evaluated after them.
 */
async function answerTool(request: ToolRequest): Promise<void> {
  let reply: ToolReply
  try {
    const { callToolByName } = await import('../tools/call')
    const context = contexts.get(request.id)
    if (!context) throw new Error('The run this call belongs to is no longer open')
    reply = { ok: true, value: await callToolByName(request.name, request.args, context) }
  } catch (e) {
    reply = { ok: false, error: message(e) }
  }
  writeReply(request, reply)
}

/** Put the answer in the shared buffer and wake the worker. */
function writeReply(request: ToolRequest, reply: ToolReply): void {
  const bytes = bytesFor(request, reply)
  request.reply.set(bytes)
  Atomics.store(request.control, 1, bytes.length)
  Atomics.store(request.control, 0, ANSWERED)
  Atomics.notify(request.control, 0)
}

/**
 * The reply as bytes the script can parse. A result that won't serialise, or is
 * too large for the buffer, becomes an error saying so: half a JSON document
 * would fail to parse in the sandbox with nothing to say about why.
 */
function bytesFor(request: ToolRequest, reply: ToolReply): Uint8Array {
  const encoded = encode(reply)
  if (encoded && encoded.length <= request.reply.length) return encoded
  const megabytes = Math.floor(request.reply.length / 1024 / 1024)
  const error = encoded
    ? `${request.name} returned more than ${megabytes}MB, which is too much to hand back`
    : `${request.name} returned something that can't be turned into JSON`
  return encode({ ok: false, error }) ?? new Uint8Array()
}

/** UTF-8 JSON, or null when the value won't serialise. */
function encode(reply: ToolReply): Uint8Array | null {
  try {
    return new TextEncoder().encode(JSON.stringify(reply))
  } catch {
    return null
  }
}
