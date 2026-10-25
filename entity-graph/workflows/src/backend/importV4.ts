// Brings a store from before egdb (the SQLite file `workflows-v4.db`) into an
// empty egdb store, once: every event with its id, seq and state; the trace
// with its ids; every run with its transcript; the settings and custom actions.
// The old file is left as it was.
//
// What changes on the way: there was one person ("human"), who becomes the
// first person, by a username, and has every item still open; bots were per
// item then, and are their own items now, so a `bot` blocker still open is
// cancelled, and a run is recorded as one on its item, by no bot.

import type { Store } from '../engine/store'

type Row = Record<string, unknown>
interface Sqlite {
  prepare(sql: string): { all(): unknown[] }
  close(): void
}

/** node:sqlite where Node has it; else better-sqlite3 (built for Electron, so for a backend the app runs). */
async function open(file: string): Promise<Sqlite> {
  try {
    const { DatabaseSync } = await import('node:sqlite')
    return new DatabaseSync(file, { readOnly: true }) as unknown as Sqlite
  } catch {
    const { default: Database } = await import('better-sqlite3')
    return new Database(file, { readonly: true }) as unknown as Sqlite
  }
}

const json = <T>(s: unknown): T | null => (typeof s === 'string' && s ? (JSON.parse(s) as T) : null)
const n = (x: unknown): number | null => (x === null || x === undefined ? null : Number(x))

/**
 * `person` is who "human" was: the first person, by their username. What the human did is theirs from now on, and
 * every item open as it comes in is assigned to them.
 */
export async function importV4(file: string, store: Store, now: number, person: string): Promise<{ events: number; trace: number; runs: number }> {
  const who = (actor: unknown) => (actor === 'human' ? person : String(actor))
  if (store.events.size) throw new Error('The store is not empty: an import goes only into an empty one.')
  const db = await open(file)
  try {
    const all = (sql: string) => db.prepare(sql).all() as Row[]
    const has = (table: string) => all(`SELECT name FROM sqlite_master WHERE type = 'table' AND name = '${table}'`).length > 0
    const events = all('SELECT * FROM events ORDER BY parent_id, seq')
    const trace = all('SELECT * FROM trace ORDER BY id')
    const runs = all('SELECT * FROM runs ORDER BY started_at')
    store.batch(() => {
      for (const r of events) {
        const legacyBot = r.type === 'bot' && !r.resolved
        store.insertEvent({
          id: String(r.id),
          parent_id: (r.parent_id as string) ?? null,
          seq: Number(r.seq),
          type: String(r.type),
          params: json(r.params) ?? {},
          created_by: who(r.created_by),
          created_at: Number(r.created_at),
          resolved: legacyBot || Boolean(r.resolved),
          resolved_at: legacyBot ? now : n(r.resolved_at),
          resolved_by: legacyBot ? 'system' : r.resolved_by ? who(r.resolved_by) : null,
          cancelled: legacyBot || Boolean(r.cancelled),
          started: Boolean(r.started),
          data: legacyBot ? { cancelled: true, detail: 'Bots are their own items now: assign the task to one.' } : json(r.data),
          memo: json(r.memo),
          check_step: 0,
          next_check_at: null,
          last_error: null,
        })
      }
      for (const t of trace)
        store.appendTrace({
          id: Number(t.id),
          at: Number(t.at),
          item_id: String(t.item_id),
          event_id: (t.event_id as string) ?? null,
          run_id: (t.run_id as string) ?? null,
          actor: who(t.actor),
          kind: String(t.kind),
          data: json(t.data),
        })
      // The bot blockers cancelled on the way, in the trace like any other cancel.
      for (const r of events)
        if (r.type === 'bot' && !r.resolved)
          store.appendTrace({ at: now, item_id: String(r.parent_id), event_id: String(r.id), run_id: null, actor: 'system', kind: 'event_cancelled', data: null })
      for (const r of runs) {
        const id = String(r.id)
        store.insertRun(
          {
            id,
            agent_id: null,
            item_ids: [String(r.item_id)],
            event_ids: json(r.event_ids) ?? [],
            started_at: Number(r.started_at),
            trace_mark: Number(r.trace_mark ?? 0),
            ended_at: n(r.ended_at) ?? now,
            cost_usd: n(r.cost_usd),
            turns: n(r.turns),
            result: (r.result as string) ?? null,
            prompt: String(r.prompt ?? ''),
          },
          String(r.transcript ?? ''),
        )
        store.endTranscript(id)
      }
      if (has('settings')) for (const s of all('SELECT * FROM settings')) store.setSetting(String(s.key), json(s.value))
      if (has('custom_actions'))
        for (const a of all('SELECT * FROM custom_actions'))
          store.putCustomAction({
            id: String(a.id),
            type: String(a.type),
            name: String(a.name),
            description: String(a.description ?? ''),
            notes: String(a.notes ?? ''),
            source: String(a.source ?? ''),
            icon: String(a.icon ?? 'zap'),
            important: Boolean(a.important),
            created_at: Number(a.created_at),
            updated_at: Number(a.updated_at),
          })
      // The one person there was: made, and given every item still open.
      store.insertEvent({
        id: person, parent_id: null, seq: store.nextSeq(null), type: 'user', params: { username: person }, created_by: person, created_at: now,
        resolved: false, resolved_at: null, resolved_by: null, cancelled: false, started: true, data: null, memo: null, check_step: 0,
        next_check_at: null, last_error: null,
      })
      store.appendTrace({ at: now, item_id: person, event_id: person, run_id: null, actor: 'system', kind: 'event_added', data: { type: 'user' } })
      for (const r of events)
        if (r.type === 'item' && !r.resolved) {
          store.setAssigned(String(r.id), person, true, now)
          store.appendTrace({ at: now, item_id: String(r.id), event_id: null, run_id: null, actor: 'system', kind: 'assigned', data: { to: person } })
          store.appendTrace({ at: now, item_id: person, event_id: null, run_id: null, actor: 'system', kind: 'assigned', data: { item: String(r.id) } })
        }
    })
    return { events: events.length, trace: trace.length, runs: runs.length }
  } finally {
    db.close()
  }
}
