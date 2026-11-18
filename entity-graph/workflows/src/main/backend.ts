// The backend on a source: one already running on it (its backend.json says
// where), or one the app starts, as a child process run by Electron's own Node
// (ELECTRON_RUN_AS_NODE), and stops when it is done with the source. Either way
// the app reaches it over HTTP: calls as JSON, and changes as server-sent events.

import { fork } from 'node:child_process'
import { join } from 'node:path'
import { dataDirOf, isRemote, runningBackend, type BackendInfo } from '../backend/info'

export interface Backend {
  /** The source's file, as an absolute path. */
  file: string
  info: BackendInfo
  /** Started by the app, so stopped by it. */
  owned: boolean
  /** A call as `user` (the person signed in to the window), or as nobody. */
  call: (method: string, args: unknown[], user?: string | null) => Promise<unknown>
  /** Calls `fn` on every change; again after a lost connection, since anything may have changed meanwhile. */
  onChange: (fn: () => void) => void
  /** Lets go of it: stops it if the app started it, and stops listening to it either way. */
  close: () => Promise<void>
}

/** A backend running on `file` (an absolute path), or a new one. */
export async function backend(file: string): Promise<Backend> {
  const found = await runningBackend(dataDirOf(file))
  if (found) return connect(file, found, false, async () => {})
  const child = fork(join(__dirname, 'backend.js'), ['--file', file], {
    env: { ...process.env, ELECTRON_RUN_AS_NODE: '1' },
    stdio: ['ignore', 'inherit', 'pipe', 'ipc'],
  })
  let stderr = ''
  child.stderr?.setEncoding('utf8').on('data', (c: string) => {
    stderr += c
    process.stderr.write(c)
  })
  const info = await new Promise<BackendInfo>((resolve, reject) => {
    child.on('message', (m: { ready?: BackendInfo }) => m.ready && resolve(m.ready))
    child.on('exit', (code) => reject(new Error(stderr.trim().split('\n').at(-1) || `The backend stopped as it started (${code}).`)))
  })
  const stop = () =>
    new Promise<void>((resolve) => {
      if (child.exitCode !== null) return resolve()
      const timer = setTimeout(() => child.kill('SIGKILL'), 15_000)
      child.once('exit', () => {
        clearTimeout(timer)
        resolve()
      })
      child.send('stop')
    })
  return connect(file, info, true, stop)
}

/**
 * A backend already running elsewhere (a headless host), reached by its URL (its phone listener, which
 * `tailscale serve` takes to the tailnet) and the phone token, as the phone reaches it. Not owned: closing it only
 * stops listening. It behaves as the window's own backend otherwise, so the same calls and the same change stream work.
 *
 * Limits of a remote source: the Phone page's settings (the token, the tailnet, the ports) are refused from here
 * (PHONE_REFUSED) — they are changed on the server; and `copyFile` always sends the file's bytes, since the server's
 * path names nothing on this machine. The token is kept by the main process (src/main/index.ts), never here.
 */
export async function remote(typed: string, token: string): Promise<Backend> {
  const url = typed.trim().replace(/\/$/, '')
  if (!isRemote(url)) throw new Error('A remote source is a URL: https://… or http://…')
  const made = connect(url, { url, token, pid: 0 }, false, async () => {})
  // Check the URL and the token at once, as the browser transport does on opening (transport.ts): ask which source it
  // is. The status says which plain error to give; a wrong host or a down one fails the fetch itself.
  let res: Response
  try {
    res = await fetch(`${url}/rpc/source`, { method: 'POST', headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' }, body: '[]' })
  } catch {
    throw new Error(`No backend answers at ${url}.`)
  }
  if (res.status === 401) throw new Error('The token is refused.')
  const out = (await res.json().catch(() => null)) as { result?: unknown; error?: string } | null
  // The tailnet's 403 message, or anything else the backend says, passes through as it is.
  if (!out || out.error) throw new Error(out?.error ?? `No backend answers at ${url}.`)
  return made
}

function connect(file: string, info: BackendInfo, owned: boolean, stop: () => Promise<void>): Backend {
  const headers = { authorization: `Bearer ${info.token}`, 'content-type': 'application/json' }
  const listening = new AbortController()
  return {
    file,
    info,
    owned,
    close: async () => {
      listening.abort()
      if (owned) await stop()
    },
    call: async (method, args, user) => {
      const res = await fetch(`${info.url}/rpc/${method}`, {
        method: 'POST',
        headers: { ...headers, ...(user ? { 'x-workflows-user': user } : {}) },
        body: JSON.stringify(args),
      })
      const out = (await res.json()) as { result?: unknown; error?: string }
      if (out.error) throw new Error(out.error)
      return out.result
    },
    onChange: (fn) => {
      const listen = async () => {
        while (!listening.signal.aborted) {
          try {
            const res = await fetch(`${info.url}/events`, { headers, signal: listening.signal })
            if (!res.body) throw new Error('No stream.')
            fn()
            const reader = res.body.getReader()
            for (let chunk = await reader.read(); !chunk.done; chunk = await reader.read())
              if (new TextDecoder().decode(chunk.value).includes('event: change')) fn()
          } catch {
            // The backend is restarting, or gone: try again in a moment, unless the app is done with it.
          }
          await new Promise((r) => setTimeout(r, 1000))
        }
      }
      void listen()
    },
  }
}
