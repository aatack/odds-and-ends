// The window's way to egdb: `/egdb/<route>` on both listeners, behind their tokens (the app's, or the phone's), as
// the person signed in. egdb-server's own token never leaves this machine; what may pass is decided here:
// - reads in `READS` go to egdb-server on the store's branch (the branch the request names is ignored), and anything
//   on a settings entity (`wf.*`, where the secrets are) is taken out of what comes back;
// - `/egdb/write` is the engine's `write` (raw ops, checked, as the person), so the engine and egdb stay the same;
// - `/egdb/undo` is the engine's `undo`, as the person: only their own newest change, named by its first seq;
// - everything else (archiving, branches) is refused.

import type { Engine } from '../engine/engine'
import type { Op } from 'egdb-client'

/** The egdb routes that only read, which the window may ask. */
export const READS = new Set(['values', 'value_events', 'value', 'links', 'children', 'parents', 'exists', 'find', 'walk', 'log', 'branch'])

export interface EgdbTarget {
  url: string
  token: string
  branch: string
}

/** Whether a value names a settings entity (where the secrets are): an id that is, or an object with one. */
const secret = (x: unknown): boolean =>
  typeof x === 'string'
    ? x.startsWith('wf.')
    : !!x && typeof x === 'object' && !Array.isArray(x) && ['id', 'entity', 'source', 'destination'].some((k) => typeof (x as Record<string, unknown>)[k] === 'string' && secret((x as Record<string, unknown>)[k]))

/** What came back, without settings entities: as keys of a map, as items of a list, or as objects naming one. */
export function withoutSecrets(x: unknown): unknown {
  if (Array.isArray(x)) return x.filter((v) => !secret(v)).map(withoutSecrets)
  if (!x || typeof x !== 'object') return x
  return Object.fromEntries(Object.entries(x).filter(([k, v]) => !k.startsWith('wf.') && !secret(v)).map(([k, v]) => [k, withoutSecrets(v)]))
}

/** Answers one `/egdb/<route>` call, as `user`: what egdb (or the engine) answers, or throws what went wrong. */
export async function egdbCall(target: EgdbTarget, engine: Engine, route: string, body: unknown, user: string): Promise<unknown> {
  const args = body && typeof body === 'object' && !Array.isArray(body) ? (body as Record<string, unknown>) : {}
  if (route === 'write') {
    if (!Array.isArray(args.ops)) throw new Error('A write is { ops: [...] }.')
    const { made, landed } = await engine.writeLanding(args.ops as Op[], user)
    await engine.store.flush()
    // In egdb before answering (where it landed says so), so a read made next (egdb-client reads back what it wrote)
    // has it. Its seqs are what an undo names; none (first after last) when it did not land as one batch of the person's.
    return { branch: target.branch, first_seq: landed?.first ?? 1, last_seq: landed?.last ?? 0, timestamp: landed?.timestamp ?? Date.now(), made }
  }
  if (route === 'undo') {
    if (typeof args.seq !== 'number') throw new Error('An undo is { seq }: the first seq of the change it takes off.')
    const removed = await engine.undo(args.seq, user)
    return removed && { removed, events: 0, restored: 0 }
  }
  if (!READS.has(route)) throw new Error(`/egdb/${route} is not open to the app.`)
  if (JSON.stringify(args).includes('"wf.')) throw new Error('Settings are not read this way.')
  const res = await fetch(`${target.url}/${route}`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', authorization: `Bearer ${target.token}` },
    body: JSON.stringify({ ...args, branch: target.branch }),
  })
  const out = (await res.json()) as unknown
  if (!res.ok) throw new Error(String((out as { message?: string })?.message ?? `egdb answered ${res.status}.`))
  return withoutSecrets(out)
}
