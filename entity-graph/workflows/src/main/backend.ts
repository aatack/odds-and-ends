// The app's backend: one that is running already on the data folder (its
// backend.json says where), or one the app starts and stops with itself, as a
// child process run by Electron's own Node (ELECTRON_RUN_AS_NODE). Either way
// the app reaches it over HTTP: calls as JSON, and changes as server-sent events.

import { fork } from 'node:child_process'
import { join } from 'node:path'
import { runningBackend, type BackendInfo } from '../backend/info'

export interface Backend {
  info: BackendInfo
  /** Started by the app, so stopped with it. */
  owned: boolean
  /** A call as `user` (the person signed in to the window), or as nobody. */
  call: (method: string, args: unknown[], user?: string | null) => Promise<unknown>
  /** Calls `fn` on every change; again after a lost connection, since anything may have changed meanwhile. */
  onChange: (fn: () => void) => void
  stop: () => Promise<void>
}

/** WORKFLOWS_BACKEND_URL and _TOKEN, a backend running on `dir`, or a new one. */
export async function backend(dir: string): Promise<Backend> {
  const given = process.env.WORKFLOWS_BACKEND_URL
    ? { url: process.env.WORKFLOWS_BACKEND_URL.replace(/\/+$/, ''), token: process.env.WORKFLOWS_BACKEND_TOKEN ?? '', pid: 0 }
    : null
  const found = given ?? (await runningBackend(dir))
  if (found) return connect(found, false, async () => {})
  const child = fork(join(__dirname, 'backend.js'), ['--data', dir], {
    env: { ...process.env, ELECTRON_RUN_AS_NODE: '1' },
    stdio: ['ignore', 'inherit', 'inherit', 'ipc'],
  })
  const info = await new Promise<BackendInfo>((resolve, reject) => {
    child.on('message', (m: { ready?: BackendInfo }) => m.ready && resolve(m.ready))
    child.on('exit', (code) => reject(new Error(`The backend stopped as it started (${code}): see the app's output.`)))
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
  return connect(info, true, stop)
}

function connect(info: BackendInfo, owned: boolean, stop: () => Promise<void>): Backend {
  const headers = { authorization: `Bearer ${info.token}`, 'content-type': 'application/json' }
  return {
    info,
    owned,
    stop,
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
        for (;;) {
          try {
            const res = await fetch(`${info.url}/events`, { headers })
            if (!res.body) throw new Error('No stream.')
            fn()
            const reader = res.body.getReader()
            for (let chunk = await reader.read(); !chunk.done; chunk = await reader.read())
              if (new TextDecoder().decode(chunk.value).includes('event: change')) fn()
          } catch {
            // The backend is restarting, or gone: try again in a moment.
          }
          await new Promise((r) => setTimeout(r, 1000))
        }
      }
      void listen()
    },
  }
}
