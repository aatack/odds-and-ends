// WorkflowsAPI over HTTP, straight to the backend: what the app is in a browser (the phone), where there is no
// preload. The same calls the main process passes on for the app on this machine (src/main/backend.ts), with the
// phone's token and the person signed in on each, and changes read from /events. No DOM here, so the tests drive it.

import { API_METHODS, type WorkflowsAPI } from '../../shared/types'

/** Where the token is kept (local storage, in the browser). */
export const TOKEN_KEY = 'workflows.phoneToken'

/** The backend could not be reached (rather than refusing): what was asked may be asked again. */
export class Offline extends Error {
  readonly offline = true
}
export const isOffline = (e: unknown): boolean => !!(e as { offline?: boolean } | null)?.offline

export interface KeptStrings {
  getItem: (key: string) => string | null
  setItem: (key: string, value: string) => void
  removeItem: (key: string) => void
}

export interface HttpOptions {
  /** The backend's address; '' for the page's own. */
  base?: string
  storage: KeptStrings
  fetch?: typeof fetch
  /** The token was refused (and so forgotten): sign in again. */
  onRefused?: () => void
  /** How long to wait before listening for changes again, after losing the stream. */
  retryMs?: number
}

export function httpApi(o: HttpOptions): WorkflowsAPI {
  const base = o.base ?? ''
  const go = o.fetch ?? ((...a: Parameters<typeof fetch>) => fetch(...a))
  let user: string | null = null
  const token = () => o.storage.getItem(TOKEN_KEY)
  const headers = (): Record<string, string> => {
    const t = token()
    return { 'content-type': 'application/json', ...(t ? { authorization: `Bearer ${t}` } : {}), ...(user ? { 'x-workflows-user': user } : {}) }
  }
  const refused = () => {
    o.storage.removeItem(TOKEN_KEY)
    o.onRefused?.()
  }

  const call = async (method: string, args: unknown[]): Promise<unknown> => {
    let res: Response
    try {
      res = await go(`${base}/rpc/${method}`, { method: 'POST', headers: headers(), body: JSON.stringify(args) })
    } catch (e) {
      throw new Offline((e as Error).message)
    }
    const out = (await res.json().catch(() => null)) as { result?: unknown; error?: string } | null
    // Not the backend answering (a proxy with nothing behind it, or a network in the way): it may be tried again.
    if (!out) throw res.status >= 500 ? new Offline(`The backend answered ${res.status}.`) : new Error(`The backend answered ${res.status}.`)
    // Refused for its token: kept to be asked again once signed in afresh.
    if (res.status === 401) {
      refused()
      throw new Offline(out.error ?? 'The token was refused.')
    }
    if (out.error) throw new Error(out.error)
    return out.result
  }
  const calls = Object.fromEntries(API_METHODS.map((name) => [name, (...args: unknown[]) => call(name, args)]))

  const onChange = (cb: () => void) => {
    const stop = new AbortController()
    const listen = async () => {
      while (!stop.signal.aborted) {
        try {
          const res = await go(`${base}/events`, { headers: headers(), signal: stop.signal })
          if (res.status === 401) return refused()
          if (!res.ok || !res.body) throw new Error(`changes: ${res.status}`)
          // Anything may have changed while nobody was listening.
          cb()
          const reader = res.body.getReader()
          const text = new TextDecoder()
          for (let chunk = await reader.read(); !chunk.done; chunk = await reader.read())
            if (text.decode(chunk.value, { stream: true }).includes('event: change')) cb()
        } catch {
          // Offline, or the backend is restarting: try again in a moment.
        }
        if (!stop.signal.aborted) await new Promise((r) => setTimeout(r, o.retryMs ?? 2000))
      }
    }
    void listen()
    return () => stop.abort()
  }

  return {
    ...(calls as unknown as WorkflowsAPI),
    setUser: (u) => {
      user = u
    },
    setToken: (t) => (t?.trim() ? o.storage.setItem(TOKEN_KEY, t.trim()) : o.storage.removeItem(TOKEN_KEY)),
    token,
    // The phone has no choice of source: the backend it reaches is on one. Asking which checks the token.
    openSource: () => call('source', []) as Promise<string>,
    closeSource: async () => o.storage.removeItem(TOKEN_KEY),
    onChange,
    // The browser's own Back is wired in by the page (useWorkflows.ts); there are no keys from a main process.
    onBack: () => () => {},
    onForward: () => () => {},
  }
}
