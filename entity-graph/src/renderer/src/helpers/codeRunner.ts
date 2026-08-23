import { atom } from '../state/atom'
import type { CallContext } from '../state/types'
import type {
  RunRequest,
  RunResponse,
  ToolReply,
  ToolRequest,
  WorkerMessage,
} from './codeRunner.worker'

// Local execution of scripts — `type: code` entities, `events` keys, and the
// bodies of the tools the user wrote — in sandboxed QuickJS workers. A browser
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

export type CodeRunState =
  | { status: 'running' }
  | { status: 'done'; logs: string[]; result?: unknown; hasResult: boolean }
  | { status: 'error'; logs: string[]; error: string }

export const codeRunsAtom = atom<Record<string, CodeRunState>>({})

const setRun = (id: string, state: CodeRunState): void =>
  codeRunsAtom.set((runs) => ({ ...runs, [id]: state }))

// A pool, and it has to be one. A script blocks its own thread while a tool call
// is answered — that is what buys `tool.x()` its lack of `await` — so a worker
// waiting on a call cannot pick up another script in the meantime. And answering
// a call is exactly when another script may need to run: a tool the user wrote is
// a body of its own, and `tool.myTool()` from a script would otherwise post it to
// the very worker that is blocked waiting for it, which is a deadlock and looks
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

/** Run a code entity, recording what it did so its row can show it. */
export function runCode(id: string, code: string, context: CallContext): void {
  setRun(id, { status: 'running' })
  void execute(id, code, context).then((r) => {
    if (r.ok) setRun(id, { status: 'done', logs: r.logs, result: r.result, hasResult: !!r.hasResult })
    else setRun(id, { status: 'error', logs: r.logs, error: r.error ?? 'Error' })
  })
}

/**
 * Run a script for its value alone, with nothing shown for it. This is how an
 * entity's `events` field is evaluated: it is a property of the entity being
 * computed, not something the user asked to run, so it has no place in the run
 * state a row draws its play button from.
 *
 * The id is namespaced away from the entity's own, so evaluating a `type: code`
 * entity's `events` can't be mistaken for running the code in it.
 *
 * Nothing being *shown* is not the same as nothing being knowable: whatever the
 * script logged goes to the devtools console either way, since a script that
 * runs on its own behalf in the background has nowhere else to say anything.
 */
export async function evaluateCode(
  entityId: string,
  code: string,
  values: Record<string, unknown>,
): Promise<unknown> {
  // A script called upon by the entity rather than by the user has no frame and
  // no selection behind it; the context is the entity, as promised.
  const context: CallContext = {
    values: { ...values, entityId },
    path: [entityId],
    groupId: null,
    tabId: null,
    frameId: null,
    startedAt: Date.now(),
  }
  const where = `events on ${entityId}`
  const result = await execute(`events:${entityId}`, code, context)
  for (const line of result.logs) console.log(`[${where}]`, line)
  if (!result.ok) {
    const error = result.error ?? 'Error'
    console.error(`[${where}] ${error}`)
    throw new Error(error)
  }
  return result.result
}

/** Distinguishes one run of the same script from another. See {@link runToolScript}. */
let scriptRuns = 0

/**
 * Run a user-defined tool's body and hand back what it evaluated to. A sibling of
 * {@link evaluateCode}: the same sandbox and the same synchronous `tool` bridge,
 * and likewise nowhere to *show* the run — a tool's output belongs to the call
 * that made it, which is what puts it in the activity log.
 *
 * The context is the *call's*, passed straight through rather than rebuilt around
 * the tool's own entity. A tool is invoked from somewhere, and what it goes on to
 * do should be recorded against the frame the user was looking at; a definition
 * sitting under `@tools` is not where any of it is happening.
 *
 * Logs are returned rather than printed, since unlike an `events` key this has a
 * caller to hand them to.
 */
export async function runToolScript(
  toolId: string,
  code: string,
  context: CallContext,
): Promise<{ result: unknown; logs: string[] }> {
  // Namespaced away from a code entity's own id, so a tool whose body lives on an
  // entity the user can also press play on doesn't overwrite that row's output —
  // and counted, because the run id is what an answer finds its way back by, so
  // two invocations of one tool must not share it.
  const response = await execute(`tool:${toolId}#${++scriptRuns}`, code, context)
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
