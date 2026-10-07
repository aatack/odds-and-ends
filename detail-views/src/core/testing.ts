import { Core, type CoreOptions } from './core.ts'

/** Helpers for tests: a core with nothing on disk, and a Slack that answers from a table. */

export function memoryCore(options: Partial<CoreOptions> = {}): Core {
  return new Core({ owned: ':memory:', cache: ':memory:', ...options })
}

/** Answers Slack methods from a table; anything missing fails the test. */
export function fakeSlack(answers: Record<string, (params: URLSearchParams) => unknown>, calls: string[] = []): typeof fetch {
  return (async (input: string | URL | Request, init?: RequestInit) => {
    const method = String(input).split('/').pop()!
    calls.push(method)
    const answer = answers[method]
    if (!answer) throw new Error(`unexpected slack call ${method}`)
    return new Response(JSON.stringify({ ok: true, ...(answer(init!.body as URLSearchParams) as object) }))
  }) as typeof fetch
}
