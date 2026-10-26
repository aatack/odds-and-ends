// The backend on a source: one already running on it (its backend.json says
// where), or one the app starts, as a child process run by Electron's own Node
// (ELECTRON_RUN_AS_NODE), and stops when it is done with the source. Either way
// the app reaches it over HTTP: calls as JSON, and changes as server-sent events.

import { fork } from 'node:child_process'
import { join } from 'node:path'
import { dataDirOf, runningBackend, type BackendInfo } from '../backend/info'

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
