// A script calling a tool the user wrote, which is a script calling a script.
//
// This is the case the worker pool exists for. A script blocks its own thread
// while a tool call is answered — that is what buys `tool.x()` its lack of
// `await` — so answering a call by running *another* script cannot use the same
// worker. With one worker it deadlocks: the second run is posted to a thread that
// will not read its queue until the first run finishes, and the first run is
// waiting on the second.
//
// The real sandbox needs a browser, so what stands in for it here is a worker that
// models the *protocol* rather than the JavaScript: it takes a run, optionally
// makes one tool call, and waits on the same shared word the real one waits on.
// The deadlock is a property of the protocol, so that is enough to catch it.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource } from './source.mjs'

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})
Object.defineProperty(globalThis, 'window', { value: { entityGraph: {} } })

// --- A worker that blocks the way the real one does -------------------------

interface Run {
  id: string
  code: string
}

let built = 0
const ANSWERED = 1

class FakeWorker {
  onmessage: ((e: { data: unknown }) => void) | null = null
  /** True from taking a run until answering it — the real one's blocked thread. */
  private busy = false
  /** Anything posted while blocked. The real worker cannot read these either. */
  readonly ignored: Run[] = []

  constructor() {
    built++
  }

  postMessage(run: Run): void {
    if (this.busy) {
      this.ignored.push(run)
      return
    }
    this.busy = true
    // The code is a command rather than JavaScript: `call:<tool>:<a>,<b>` makes
    // one tool call and hands back what it returned; anything else is the result.
    // Searched for rather than matched whole, since a tool's body reaches the
    // sandbox wrapped in the source that applies it to its arguments.
    const call = /call:([^:\s]+):(\S*)/.exec(run.code)
    if (!call) {
      // Standing in for evaluating it: a tool's body arrives wrapped in the source
      // that applies it to its arguments, and what it "returns" is the expression.
      const body = /^const __tool = \(\n([\s\S]*?)\n\)\n/.exec(run.code)
      const result = body ? body[1] : run.code
      this.settle({ kind: 'result', id: run.id, ok: true, logs: [], result, hasResult: true })
      return
    }
    const control = new Int32Array(new ArrayBuffer(8))
    const reply = new Uint8Array(new ArrayBuffer(64 * 1024))
    this.onmessage?.({
      data: { kind: 'tool', id: run.id, name: call[1], args: call[2].split(','), control, reply },
    })
    // The real worker parks on `Atomics.wait`; nothing can park the one thread a
    // test has, so this watches the same word for the same answer instead.
    const poll = (): void => {
      if (Atomics.load(control, 0) !== ANSWERED) {
        setTimeout(poll, 0)
        return
      }
      const length = Atomics.load(control, 1)
      const answer = JSON.parse(new TextDecoder().decode(reply.slice(0, length)))
      this.settle({
        kind: 'result',
        id: run.id,
        ok: answer.ok,
        logs: [],
        result: answer.value,
        hasResult: answer.value !== undefined,
        error: answer.error,
      })
    }
    setTimeout(poll, 0)
  }

  private settle(result: unknown): void {
    this.busy = false
    this.onmessage?.({ data: result })
  }

  terminate(): void {
    this.busy = false
  }
}

Object.defineProperty(globalThis, 'Worker', { value: FakeWorker, writable: true })

const { setSourceTransport } = await import('../src/renderer/src/source/transport')
const { TOOLS_ENTITY_ID, loadUserTools } = await import('../src/renderer/src/tools/userTools')
const { runToolScript, stopCode } = await import('../src/renderer/src/helpers/codeRunner')
const { defaultLayout } = await import('../src/renderer/src/state/types')
const { layoutAtom } = await import('../src/renderer/src/state/store')

// --- Harness ----------------------------------------------------------------

const context = () => ({
  values: {},
  path: [],
  groupId: null,
  tabId: null,
  frameId: null,
  startedAt: Date.now(),
})

async function openWithTool(execute = 'joined'): Promise<void> {
  const source = new MemorySource()
  setSourceTransport({ call: (t, a) => source.call(t, a), user: 'test', sourceId: 'memory' })
  layoutAtom.set(defaultLayout('root'))
  stopCode()
  // A tool whose body is, as far as the stand-in worker is concerned, "return
  // this string" — all that matters is that running it needs a worker.
  for (const [key, value] of Object.entries({ type: 'tool', text: 'join', execute })) {
    await source.call('writeValue', { entityId: 'n1', key, value, author: 'test', timestamp: Date.now() })
  }
  await source.call('writeLink', {
    sourceId: TOOLS_ENTITY_ID,
    destinationId: 'n1',
    action: 0,
    author: 'test',
    timestamp: Date.now(),
  })
  await loadUserTools()
}

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

/** Fail loudly rather than hanging the suite, which is what a deadlock does. */
function within<T>(ms: number, work: Promise<T>, what: string): Promise<T> {
  return Promise.race([
    work,
    new Promise<T>((_, reject) => setTimeout(() => reject(new Error(`${what} did not finish`)), ms)),
  ])
}

// --- Tests ------------------------------------------------------------------

test('runs a tool called from a script, rather than deadlocking on it', async () => {
  await openWithTool()
  const before = built
  const run = runToolScript('outer', 'call:join:left,right', context())
  const { result } = await within(2000, run, 'a script calling a tool the user wrote')

  assert.equal(result, 'joined')
  // The second run cannot have gone to the first worker: it was mid-call, and a
  // real one would not have read its queue until the call it is blocked on ends.
  assert.equal(built - before, 2, 'the nested run should have taken a worker of its own')
})

test('reuses a worker once its run has finished', async () => {
  await openWithTool()
  const before = built
  await within(2000, runToolScript('a', 'one', context()), 'the first run')
  await within(2000, runToolScript('b', 'two', context()), 'the second run')

  assert.equal(built - before, 1, 'runs that do not overlap should share one worker')
})

test('gives up rather than nesting without end', async () => {
  // A tool whose body calls itself. Each level blocks the one above it, so each
  // needs a worker of its own — without a ceiling this takes the tab with it.
  await openWithTool('call:join:x')

  const run = runToolScript('outer', 'call:join:x', context())
  await assert.rejects(
    () => within(5000, run, 'a tool that calls itself'),
    /waiting on one another/,
  )
  stopCode()
})

// --- Run --------------------------------------------------------------------

let failed = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
