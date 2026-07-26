import { atom } from '../state/atom'
import type { CallContext } from '../state/types'
import type { RunRequest, ToolReply, ToolRequest, WorkerMessage } from './codeRunner.worker'

// Local execution of `type: code` entities in one sandboxed QuickJS worker.
// A browser service rather than state: the results are runtime-only and never
// written back, and the worker is created lazily on the first run and torn down
// on Stop (terminating it is how a runaway script is interrupted).
//
// This side also answers the scripts' tool calls. A script calls a tool
// synchronously (see the worker for why), which it can only do by blocking its
// own thread — so the work happens here, on the thread that owns the registry,
// and the answer is written into the shared buffers the worker is waiting on.
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

/**
 * The context each running script was started in, by entity id. Kept here rather
 * than sent along with every tool call: the script is given a copy of its values,
 * but a call it makes is recorded against the whole context, which is not the
 * sandbox's to hand back.
 */
const contexts = new Map<string, CallContext>()

function ensureWorker(): Worker {
  if (worker) return worker
  const next = new Worker(new URL('./codeRunner.worker.ts', import.meta.url), { type: 'module' })
  next.onmessage = (e: MessageEvent<WorkerMessage>): void => {
    const r = e.data
    if (r.kind === 'tool') {
      void answerTool(r)
      return
    }
    contexts.delete(r.id)
    if (r.ok) setRun(r.id, { status: 'done', logs: r.logs, result: r.result, hasResult: !!r.hasResult })
    else setRun(r.id, { status: 'error', logs: r.logs, error: r.error ?? 'Error' })
  }
  worker = next
  return next
}

export function runCode(id: string, code: string, context: CallContext): void {
  setRun(id, { status: 'running' })
  contexts.set(id, context)
  const request: RunRequest = { id, code, context: context.values }
  ensureWorker().postMessage(request)
}

/** Interrupt whatever is running by killing the worker; the next run respawns it. */
export function stopCode(): void {
  worker?.terminate()
  worker = null
  contexts.clear()
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
