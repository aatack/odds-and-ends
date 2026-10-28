// After items' scripts run here: JavaScript in QuickJS, so a script a bot wrote
// can reach nothing but the actions it is handed, and those through the same
// gate as any other call.

import { getQuickJS, type QuickJSContext, type QuickJSHandle } from 'quickjs-emscripten'

const CPU_MS = 5_000
const WALL_MS = 120_000

/** Whether a source is a function expression (`async (context, tools) => …`), rather than a body. */
export const isFunction = (source: string): boolean => /^\s*(async\s+)?(function\b|\([^)]*\)\s*=>|[A-Za-z_$][\w$]*\s*=>)/.test(source)

/**
 * Runs `source` and answers what it returns (awaited), as JSON, with whatever it logged. A source is a **body**:
 * `context` and `tools` are in scope (and `ctx` and `actions`, the older names), and it may `await` and `return`
 * at the top level. A function expression, `async (context, tools) => …`, is called with them too.
 * `tools.<name>(args)` calls `call` and resolves to its result.
 */
export async function runScript(
  source: string,
  input: unknown,
  call: (name: string, args: Record<string, unknown>) => Promise<unknown>,
): Promise<{ value: unknown; logs: string[] }> {
  const QuickJS = await getQuickJS()
  const runtime = QuickJS.newRuntime()
  runtime.setMemoryLimit(32 * 1024 * 1024)
  const deadline = Date.now() + CPU_MS
  runtime.setInterruptHandler(() => Date.now() > deadline)
  const vm = runtime.newContext()
  const logs: string[] = []
  try {
    const fn = (name: string, impl: (...args: QuickJSHandle[]) => QuickJSHandle | void) => {
      const h = vm.newFunction(name, impl)
      vm.setProp(vm.global, name, h)
      h.dispose()
    }
    fn('__log', (...args) => {
      logs.push(args.map((a) => vm.getString(a)).join(' '))
    })
    fn('__call', (nameH, argsH) => {
      const promise = vm.newPromise()
      call(vm.getString(nameH), JSON.parse(vm.getString(argsH)))
        .then(
          (r) => settle(vm, (h) => promise.resolve(h), () => vm.newString(JSON.stringify(r ?? null))),
          (e: Error) => settle(vm, (h) => promise.reject(h), () => vm.newError(e.message)),
        )
      promise.settled.then(() => runtime.alive && runtime.executePendingJobs())
      return promise.handle
    })
    const code = `(async () => {
      const show = (a) => typeof a === 'string' ? a : JSON.stringify(a)
      globalThis.console = { log: (...a) => __log(...a.map(show)), error: (...a) => __log(...a.map(show)) }
      const tools = new Proxy({}, { get: (_, name) => async (args) => JSON.parse(await __call(String(name), JSON.stringify(args ?? {}))) })
      const fn = ${isFunction(source) ? `(${source})` : `async (context, tools) => { const ctx = context, actions = tools
${source}
}`}
      return JSON.stringify((await fn(${JSON.stringify(input)}, tools)) ?? null)
    })()`
    const handle = vm.unwrapResult(vm.evalCode(code, 'script.js'))
    const result = vm.resolvePromise(handle)
    handle.dispose()
    runtime.executePendingJobs()
    let timer: NodeJS.Timeout | undefined
    const settled = await Promise.race([
      result,
      new Promise<never>((_, reject) => (timer = setTimeout(() => reject(new Error(`The script took longer than ${WALL_MS / 1000}s.`)), WALL_MS))),
    ]).finally(() => clearTimeout(timer))
    if (settled.error) {
      const err = vm.dump(settled.error)
      settled.error.dispose()
      throw new Error(`The script failed: ${typeof err === 'object' && err ? (err.message ?? JSON.stringify(err)) : String(err)}`)
    }
    const value = JSON.parse(vm.getString(settled.value))
    settled.value.dispose()
    return { value, logs }
  } finally {
    vm.dispose()
    runtime.dispose()
  }
}

/** Settles a call's promise in the script, unless the script has ended (timed out, or failed) since it made the call. */
function settle(vm: QuickJSContext, to: (h: QuickJSHandle) => void, make: () => QuickJSHandle) {
  if (!vm.alive) return
  const h = make()
  to(h)
  h.dispose()
}
