// The window's egdb: egdb-client's cache and hooks (`useEntity`, `useWalk`, `useFind`, `useWrite`), on the store's
// branch, through the backend's `egdb` call (egdbProxy.ts) rather than to egdb-server, whose token never leaves the
// machine. The backend decides the branch and who is writing; reads leave settings out, and a write is the engine's.
// One per App (a source and a person signed in), kept up to date by the change feed (`follow`) while it is open.

import { Egdb, EgdbClient } from 'egdb-client'
import { isOffline } from './transport'
import type { WorkflowsAPI } from '../../shared/types'

/**
 * A `fetch` for EgdbClient that answers `egdb:/<route>` with the backend's `egdb` call: what it answers as a 200,
 * what it refuses as a 400 (egdb's error shape), and the backend out of reach as a network error (tried again).
 */
export function egdbFetch(api: () => WorkflowsAPI): typeof fetch {
  return (async (input: unknown, init?: { body?: unknown }) => {
    const route = String(input).replace(/^egdb:\/*/, '')
    const body = typeof init?.body === 'string' ? (JSON.parse(init.body) as unknown) : {}
    try {
      return new Response(JSON.stringify((await api().egdb(route, body)) ?? null), { status: 200 })
    } catch (e) {
      if (isOffline(e)) throw e
      const message = (e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, '')
      return new Response(JSON.stringify({ error: 'invalid', message }), { status: 400 })
    }
  }) as typeof fetch
}

/** A new egdb for the window, as `user`. `follow()` it while it is in use. */
export function newEgdb(api: () => WorkflowsAPI, user: string, onError?: (e: unknown) => void): Egdb {
  const client = new EgdbClient({ url: 'egdb:', fetch: egdbFetch(api) })
  // The branch is the backend's to say: this name is never used.
  return new Egdb({ client, branch: 'workflows', author: user, onError })
}
