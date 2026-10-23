import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { z } from 'zod'
import type { BotRun, EventSpec, EventTimes, Item, ItemDetail, ItemEvent, Overview, TraceEntry } from '../shared/types'
import { itemActions } from './actions'
import { migrate, transaction, type Db } from './db'
import { EVENT_TYPES } from './eventTypes'
import { githubActions, githubSources } from './github'
import { execCli, type Action, type ActionContext, type Author, type EventType, type Exec, type Source } from './integrations'
import { PRESETS, type Preset } from './presets'
import { plainSecrets, Settings, type Secrets } from './settings'
import { slackActions } from './slack'

/** How long a blocker sleeps between checks: at once, then a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000

/** What a bot's run returns, as its structured output. */
export interface BotOutput {
  /** To the human: what it did, or one direct question. */
  message?: string
  /** Blockers to add: more bots, waits, approvals, afters. */
  events?: EventSpec[]
  close?: boolean
}

type Row = Record<string, unknown>
const json = <T>(s: unknown): T | null => (typeof s === 'string' && s ? (JSON.parse(s) as T) : null)

const NO_TIMES: EventTimes = {
  added: null, firstChecked: null, lastChecked: null, checks: 0, firstRunStarted: null, firstRunEnded: null, runs: 0, resolved: null,
}

const toEvent = (r: Row, times: EventTimes = NO_TIMES): ItemEvent => ({
  id: r.id as string,
  itemId: r.item_id as string,
  type: r.type as ItemEvent['type'],
  params: json(r.params) ?? {},
  createdBy: r.created_by as string,
  createdAt: r.created_at as number,
  resolved: Boolean(r.resolved),
  resolvedAt: (r.resolved_at as number) ?? null,
  resolvedBy: (r.resolved_by as string) ?? null,
  cancelled: Boolean(r.cancelled),
  data: json(r.data),
  nextCheckAt: (r.next_check_at as number) ?? null,
  lastError: (r.last_error as string) ?? null,
  times,
})

const toTrace = (r: Row): TraceEntry => ({
  id: r.id as number,
  at: r.at as number,
  itemId: r.item_id as string,
  eventId: (r.event_id as string) ?? null,
  runId: (r.run_id as string) ?? null,
  actor: r.actor as string,
  kind: r.kind as string,
  data: json(r.data),
})

const toRun = (r: Row): BotRun => ({
  id: r.id as string,
  itemId: r.item_id as string,
  eventIds: json(r.event_ids) ?? [],
  worker: r.worker as number,
  startedAt: r.started_at as number,
  endedAt: (r.ended_at as number) ?? null,
  costUsd: (r.cost_usd as number) ?? null,
  turns: (r.turns as number) ?? null,
  result: (r.result as string) ?? null,
})

/** A human added it, or the system or a source did; a bot, or a bot's script, is a bot. */
const authorOf = (createdBy: string): Author => (createdBy === 'bot' || createdBy === 'script:bot' ? 'bot' : 'human')

/** The params that name another event, by id or by a ref given alongside. */
const REF_KEYS = new Set(['event', 'thread_of'])

/** Replaces refs with the ids they stand for, in the params that name events, at any depth (an after's `then` too). */
function resolveRefs(value: unknown, ids: Map<string, string>, key?: string): unknown {
  if (typeof value === 'string') return key && REF_KEYS.has(key) ? (ids.get(value) ?? value) : value
  if (Array.isArray(value)) return value.map((v) => resolveRefs(v, ids))
  if (value && typeof value === 'object')
    return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, resolveRefs(v, ids, k)]))
  return value
}

/** A line saying what an event is or came to, for prompts. */
export function describeEvent(e: ItemEvent): string {
  const d = e.data ?? {}
  const what = e.type === 'message' ? String(e.params.text) : JSON.stringify(e.params)
  const came = e.resolved && e.type !== 'message' ? ` → ${e.cancelled ? 'cancelled' : JSON.stringify({ ...d, output: undefined })}` : ''
  return `${new Date(e.createdAt).toISOString()} ${e.createdBy} ${e.type} [${e.id}]: ${what}${came}`
}

export interface EngineOptions {
  db: Db
  secrets?: Secrets
  exec?: Exec
  fetch?: typeof fetch
  now?: () => number
  sources?: Source[]
}

/**
 * The whole system, short of the processes: items, their events, the trace,
 * and the loop that checks the blockers. Every change goes through here, and
 * emits `change`.
 */
export class Engine extends EventEmitter {
  readonly db: Db
  readonly settings: Settings
  readonly exec: Exec
  readonly fetch: typeof fetch
  readonly now: () => number
  readonly actions: Action[] = [...itemActions, ...githubActions, ...slackActions]
  readonly eventTypes: EventType[] = EVENT_TYPES
  readonly presets: Preset[] = PRESETS
  readonly sources: Source[]
  /** Stops the bot running on an item. Set by the pool. */
  onCancel: (itemId: string) => void = () => {}
  private lastPoll = new Map<string, number>()
  private queues = new Map<string, Promise<unknown>>()

  constructor(opts: EngineOptions) {
    super()
    this.db = opts.db
    this.settings = new Settings(opts.db, opts.secrets ?? plainSecrets)
    this.exec = opts.exec ?? execCli
    this.fetch = opts.fetch ?? fetch
    this.now = opts.now ?? Date.now
    this.sources = opts.sources ?? githubSources
    migrate(this.db)
    // A run the app was closed during is gone; its bot blockers wait for a worker again.
    this.db.prepare('UPDATE items SET worker = NULL WHERE worker IS NOT NULL').run()
    this.db.prepare('UPDATE runs SET ended_at = ? WHERE ended_at IS NULL').run(this.now())
  }

  type(name: string): EventType {
    const t = this.eventTypes.find((t) => t.type === name)
    if (!t) throw new Error(`No event type ${name}: one of ${this.eventTypes.map((t) => t.type).join(', ')}.`)
    return t
  }

  /** Runs `fn` after everything already queued for this item: one change to an item at a time. */
  private serial<T>(itemId: string, fn: () => Promise<T>): Promise<T> {
    const run = (this.queues.get(itemId) ?? Promise.resolve()).then(fn, fn)
    const tail = run.catch(() => {})
    this.queues.set(itemId, tail)
    void tail.then(() => this.queues.get(itemId) === tail && this.queues.delete(itemId))
    return run
  }

  /** Settles once nothing is in flight. For the tests. */
  async idle(): Promise<void> {
    while (this.queues.size) await Promise.all(this.queues.values())
  }

  // ---- reads ----

  event(id: string): ItemEvent | null {
    const r = this.db.prepare('SELECT * FROM events WHERE id = ?').get(id) as Row | undefined
    return r ? toEvent(r, this.times(r.item_id as string).get(id)) : null
  }

  events(itemId: string): ItemEvent[] {
    const times = this.times(itemId)
    return (this.db.prepare('SELECT * FROM events WHERE item_id = ? ORDER BY seq').all(itemId) as Row[]).map((r) =>
      toEvent(r, times.get(r.id as string)),
    )
  }

  /** Each event's timeline, read from the trace. */
  private times(itemId: string): Map<string, EventTimes> {
    const rows = this.db
      .prepare(
        `SELECT event_id, kind, MIN(at) AS first, MAX(at) AS last, COUNT(*) AS n FROM trace
         WHERE item_id = ? AND event_id IS NOT NULL GROUP BY event_id, kind`,
      )
      .all(itemId) as Row[]
    const out = new Map<string, EventTimes>()
    for (const r of rows) {
      const t = out.get(r.event_id as string) ?? { ...NO_TIMES }
      const [first, last, n] = [r.first as number, r.last as number, Number(r.n)]
      if (r.kind === 'event_added') t.added = first
      if (r.kind === 'check') Object.assign(t, { firstChecked: first, lastChecked: last, checks: n })
      if (r.kind === 'run_started') Object.assign(t, { firstRunStarted: first, runs: n })
      if (r.kind === 'run_ended') t.firstRunEnded = first
      if (r.kind === 'event_resolved') t.resolved = first
      out.set(r.event_id as string, t)
    }
    return out
  }

  private toItem(r: Row): Item {
    const times = this.times(r.id as string)
    const blockers = (this.db.prepare('SELECT * FROM events WHERE item_id = ? AND resolved = 0 ORDER BY seq').all(r.id) as Row[]).map((e) =>
      toEvent(e, times.get(e.id as string)),
    )
    const last = this.db
      .prepare('SELECT MAX(MAX(created_at), COALESCE(MAX(resolved_at), 0)) AS at FROM events WHERE item_id = ?')
      .get(r.id) as Row
    const closed = Boolean(r.closed)
    return {
      id: r.id as string,
      title: r.title as string,
      source: r.source as string,
      url: (r.url as string) ?? null,
      closed,
      blockers,
      withHuman: !closed && blockers.every((b) => this.waitsOnHuman(b, blockers)),
      worker: (r.worker as number) ?? null,
      since: (last.at as number) ?? (r.created_at as number),
      createdAt: r.created_at as number,
      updatedAt: r.updated_at as number,
    }
  }

  /** A blocker the human resolves, or an after waiting on one: the item is still the human's turn. */
  private waitsOnHuman(b: ItemEvent, blockers: ItemEvent[], depth = 0): boolean {
    if (this.type(b.type).human) return true
    const waited = b.type === 'after' && depth < 20 && blockers.find((x) => x.id === b.params.event)
    return Boolean(waited && this.waitsOnHuman(waited, blockers, depth + 1))
  }

  get(id: string): Item | null {
    const r = this.db.prepare('SELECT * FROM items WHERE id = ?').get(id) as Row | undefined
    return r ? this.toItem(r) : null
  }

  private must(id: string): Item {
    const item = this.get(id)
    if (!item) throw new Error(`No item ${id}.`)
    return item
  }

  overview(workers: Overview['workers'] = []): Overview {
    const rows = this.db
      .prepare('SELECT * FROM items WHERE closed = 0 OR updated_at > ? ORDER BY created_at')
      .all(this.now() - 24 * 60 * MINUTE) as Row[]
    const items = rows.map((r) => this.toItem(r)).sort((a, b) => a.since - b.since)
    return { items, workers }
  }

  detail(id: string): ItemDetail | null {
    const item = this.get(id)
    if (!item) return null
    return {
      item,
      events: this.events(id),
      runs: (this.db.prepare('SELECT * FROM runs WHERE item_id = ? ORDER BY started_at').all(id) as Row[]).map(toRun),
      trace: this.trace(id),
    }
  }

  /**
   * The item that has waited longest for the human, leaving out `skip`. An open
   * item is the human's when nothing but its approvals blocks it.
   */
  next(skip: string[] = []): string | null {
    const rows = this.db.prepare('SELECT * FROM items WHERE closed = 0').all() as Row[]
    return (
      rows
        .map((r) => this.toItem(r))
        .filter((i) => i.withHuman && !skip.includes(i.id))
        .sort((a, b) => a.since - b.since)[0]?.id ?? null
    )
  }

  // ---- the trace ----

  trace(itemId: string): TraceEntry[] {
    return (this.db.prepare('SELECT * FROM trace WHERE item_id = ? ORDER BY id').all(itemId) as Row[]).map(toTrace)
  }

  /** Records one thing that happened. Every change, view, check and call comes through here. */
  mark(
    itemId: string,
    kind: string,
    opts: { eventId?: string | null; runId?: string | null; actor?: string; data?: Record<string, unknown> } = {},
  ): void {
    this.db
      .prepare('INSERT INTO trace (at, item_id, event_id, run_id, actor, kind, data) VALUES (?, ?, ?, ?, ?, ?, ?)')
      .run(this.now(), itemId, opts.eventId ?? null, opts.runId ?? null, opts.actor ?? 'system', kind, opts.data ? JSON.stringify(opts.data) : null)
    this.db.prepare('UPDATE items SET updated_at = ? WHERE id = ?').run(this.now(), itemId)
  }

  viewed(itemId: string): void {
    this.mark(itemId, 'viewed', { actor: 'human' })
  }

  skipped(itemId: string): void {
    this.mark(itemId, 'skipped', { actor: 'human' })
  }

  /**
   * The items touched in the last `days`, each with how long it spent blocked by
   * each kind of event and what came of them. The start of any look back.
   */
  recent(days: number) {
    const since = this.now() - days * 24 * 60 * MINUTE
    const rows = this.db.prepare('SELECT * FROM items WHERE updated_at >= ? ORDER BY created_at').all(since) as Row[]
    return rows.map((r) => {
      const item = this.toItem(r)
      const events = this.events(item.id)
      const blocked: Record<string, number> = {}
      for (const e of events.filter((e) => this.type(e.type).blocking))
        blocked[e.type] = (blocked[e.type] ?? 0) + ((e.resolvedAt ?? this.now()) - e.createdAt) / MINUTE
      const runs = this.db.prepare('SELECT COUNT(*) AS n, SUM(cost_usd) AS cost FROM runs WHERE item_id = ?').get(item.id) as Row
      const trace = this.trace(item.id)
      return {
        id: item.id,
        title: item.title,
        source: item.source,
        closed: item.closed,
        events: events.map((e) => e.type),
        minutesBlockedBy: Object.fromEntries(Object.entries(blocked).map(([k, v]) => [k, Math.round(v)])),
        botRuns: Number(runs.n),
        costUsd: (runs.cost as number) ?? 0,
        humanMessages: events.filter((e) => e.type === 'message' && e.createdBy === 'human').length,
        approvalsChanged: events.filter((e) => ((e.data?.changed as string[] | undefined) ?? []).length).length,
        cancelled: events.filter((e) => e.cancelled).length,
        timedOut: events.filter((e) => e.data?.outcome === 'timed_out').length,
        botFailures: events.filter((e) => e.type === 'bot' && e.data?.error).length,
        handledOutside: events.some((e) => e.type === 'closed' && e.params.how === 'handled_outside'),
        views: trace.filter((t) => t.kind === 'viewed').length,
        skips: trace.filter((t) => t.kind === 'skipped').length,
      }
    })
  }

  transcript(runId: string): string | null {
    const r = this.db.prepare('SELECT prompt, transcript FROM runs WHERE id = ?').get(runId) as Row | undefined
    return r ? `PROMPT:\n${r.prompt}\n\nTRANSCRIPT (stream-json):\n${r.transcript}` : null
  }

  // ---- actions ----

  /** The one way into an action, from MCP, a script or the UI: checked, run, and traced. */
  async callAction(name: string, args: Record<string, unknown>, ctx: ActionContext): Promise<unknown> {
    const action = this.actions.find((a) => a.name === name)
    if (!action) throw new Error(`No action ${name}.`)
    // The rule on unsafe actions (see Action in integrations.ts), checked here
    // too, whatever the caller was shown.
    if (action.unsafe && ctx.author !== 'human')
      throw new Error(`${name} is unsafe, so bots, and scripts bots wrote, may not run it. Ask a human: add an approval event for it.`)
    const parsed = z.object(action.input).safeParse(args ?? {})
    if (!parsed.success) throw new Error(`Bad arguments for ${name}: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
    const started = this.now()
    const traced = (ok: boolean, error?: string) =>
      ctx.itemId &&
      this.mark(ctx.itemId, 'action_called', {
        runId: ctx.runId,
        actor: ctx.actor,
        data: { action: name, args: parsed.data, ok, ...(error ? { error } : {}), ms: this.now() - started },
      })
    let out: unknown
    try {
      out = await action.run(parsed.data, ctx)
    } catch (e) {
      traced(false, (e as Error).message)
      throw e
    }
    traced(true)
    // An unsafe action run straight away (not through an approval) is part of the item's history.
    if (action.unsafe && ctx.itemId && !ctx.approving)
      this.add(ctx.itemId, [{ type: 'action', params: { name, inputs: parsed.data, output: out } }], ctx.actor)
    return out
  }

  // ---- events ----

  /** Checks events and expands them as their types say, without touching the store. */
  prepare(specs: EventSpec[], author: Author): EventSpec[] {
    return specs.flatMap((s) => {
      const type = this.type(s?.type)
      if (author === 'bot' && !type.bots) throw new Error(`A bot may not add a ${s.type} event.`)
      const parsed = z.object(type.params).safeParse(s.params ?? {})
      if (!parsed.success)
        throw new Error(`Bad params for a ${s.type} event: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
      const params = parsed.data as Record<string, unknown>
      if (s.type === 'approval' && !this.actions.find((a) => a.name === params.action)?.unsafe)
        throw new Error(`${params.action} is not an unsafe action, so it needs no approval.`)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e, i) => ({ type: e.type, params: e.params, ref: i === 0 ? (s.ref ?? e.ref) : e.ref }))
    })
  }

  /**
   * Adds events to an item, already prepared. Plain ones resolve at once; blockers
   * wait, and those whose type has a start hook start right after. Answers their ids.
   */
  add(itemId: string, specs: EventSpec[], createdBy: string): string[] {
    const ids = new Map<string, string>()
    const rows = specs.map((s) => {
      const id = randomUUID()
      if (s.ref) ids.set(s.ref, id)
      return { id, spec: s }
    })
    const { seq } = this.db.prepare('SELECT COALESCE(MAX(seq), 0) AS seq FROM events WHERE item_id = ?').get(itemId) as Row
    const at = this.now()
    rows.forEach(({ id, spec }, i) => {
      const type = this.type(spec.type)
      const params = resolveRefs(spec.params, ids) as Record<string, unknown>
      this.db
        .prepare(
          `INSERT INTO events (id, item_id, seq, type, params, created_by, created_at, resolved, resolved_at, resolved_by, started, next_check_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        )
        .run(id, itemId, Number(seq) + i + 1, spec.type, JSON.stringify(params), createdBy, at,
          type.blocking ? 0 : 1, type.blocking ? null : at, type.blocking ? null : createdBy, type.start ? 0 : 1, type.check ? at : null)
      this.mark(itemId, 'event_added', { eventId: id, actor: createdBy, data: { type: spec.type } })
      if (!type.blocking) this.mark(itemId, 'event_resolved', { eventId: id, actor: createdBy })
    })
    if (rows.some(({ spec }) => this.type(spec.type).start)) void this.startPending(itemId)
    // New waits are checked at once; an after may already have what it waits for.
    if (rows.some(({ spec }) => this.type(spec.type).check)) queueMicrotask(() => void this.checkDue())
    this.emit('change')
    return rows.map((r) => r.id)
  }

  /** Runs the start hooks of the item's new blockers. */
  private startPending(itemId: string): Promise<void> {
    return this.serial(itemId, async () => {
      const pending = (this.db.prepare('SELECT * FROM events WHERE item_id = ? AND started = 0 AND resolved = 0').all(itemId) as Row[]).map((r) => toEvent(r))
      for (const event of pending) {
        this.db.prepare('UPDATE events SET started = 1 WHERE id = ?').run(event.id)
        try {
          const out = await this.type(event.type).start!(event.params, { engine: this, item: this.must(itemId), event, author: authorOf(event.createdBy) })
          if (out?.params) this.db.prepare('UPDATE events SET params = ? WHERE id = ?').run(JSON.stringify(out.params), event.id)
          if (out?.resolve) this.resolve(event.id, out.resolve, 'system')
        } catch (e) {
          this.resolve(event.id, { error: (e as Error).message, detail: `Could not start: ${(e as Error).message}` }, 'system')
        }
      }
      this.emit('change')
    })
  }

  /** Resolves a blocker, and wakes whatever was waiting on it. */
  private resolve(eventId: string, data: Record<string, unknown>, by: string, cancelled = false): void {
    const r = this.db.prepare('SELECT item_id FROM events WHERE id = ? AND resolved = 0').get(eventId) as Row | undefined
    if (!r) return
    this.db
      .prepare('UPDATE events SET resolved = 1, resolved_at = ?, resolved_by = ?, cancelled = ?, data = ?, next_check_at = NULL WHERE id = ?')
      .run(this.now(), by, cancelled ? 1 : 0, JSON.stringify(data), eventId)
    this.mark(r.item_id as string, cancelled ? 'event_cancelled' : 'event_resolved', { eventId, actor: by })
    this.wake()
  }

  /** Afters and item waits are checked at once after anything resolves or closes, rather than at their next turn. */
  private wake(): void {
    this.db.prepare("UPDATE events SET next_check_at = ? WHERE resolved = 0 AND type IN ('after', 'items')").run(this.now())
    queueMicrotask(() => void this.checkDue())
  }

  // ---- the human ----

  async create(
    title: string,
    opts: { source?: string; ref?: string; url?: string | null; events?: EventSpec[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const id = randomUUID()
    const at = this.now()
    const actor = opts.actor ?? 'human'
    const events = this.prepare(opts.events ?? [], opts.author ?? 'human')
    const created = transaction(this.db, () => {
      const r = this.db
        .prepare('INSERT OR IGNORE INTO items (id, title, source, source_ref, url, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?)')
        .run(id, title, opts.source ?? 'manual', opts.ref ?? null, opts.url ?? null, at, at) as { changes: number | bigint }
      if (Number(r.changes) === 0) return false
      this.mark(id, 'item_created', { actor, data: { title, source: opts.source ?? 'manual', ref: opts.ref ?? null } })
      this.add(id, events, opts.author === 'bot' ? 'bot' : actor)
      return true
    })
    if (!created) return null
    await this.idle()
    return id
  }

  /** The chat input: `text` through a preset, as the human. */
  async send(itemId: string, presetId: string, text: string): Promise<void> {
    const preset = this.presets.find((p) => p.id === presetId)
    if (!preset) throw new Error(`No preset ${presetId}.`)
    if (this.must(itemId).closed) throw new Error('This item is closed.')
    const events = this.prepare(preset.build(text), 'human')
    if (!events.length) throw new Error('Nothing to send.')
    this.add(itemId, events, 'human')
    await this.idle()
  }

  /** Adds events as the human (or an outside client acting as them). */
  async addEvents(itemId: string, specs: EventSpec[], actor = 'human'): Promise<string[]> {
    if (this.must(itemId).closed) throw new Error('This item is closed.')
    const ids = this.add(itemId, this.prepare(specs, 'human'), actor)
    await this.idle()
    return ids
  }

  /** Runs an approval's action with the human's inputs, which resolves it. */
  async approve(eventId: string, inputs: Record<string, unknown>): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.type !== 'approval' || event.resolved) throw new Error('No approval waiting with that id.')
    const proposed = event.params.inputs as Record<string, unknown>
    const changed = Object.keys({ ...proposed, ...inputs }).filter((k) => String(inputs[k] ?? '') !== String(proposed[k] ?? ''))
    const name = String(event.params.action)
    const output = await this.callAction(name, inputs, { engine: this, author: 'human', actor: 'human', itemId: event.itemId, runId: null, approving: true })
    this.resolve(eventId, { ran: true, action: name, inputs, changed, output, detail: `Ran ${name}${changed.length ? ` (changed ${changed.join(', ')})` : ''}` }, 'human')
    this.emit('change')
    await this.idle()
  }

  /** Ends a blocker without it happening; a bot running for it is stopped. */
  async cancel(eventId: string, actor = 'human'): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.resolved) throw new Error('That event is not waiting.')
    this.resolve(eventId, { cancelled: true }, actor, true)
    if (event.type === 'bot' && this.must(event.itemId).worker !== null) this.onCancel(event.itemId)
    this.emit('change')
    await this.idle()
  }

  async close(itemId: string, opts: { actor?: string; how?: 'closed' | 'handled_outside' } = {}): Promise<void> {
    const actor = opts.actor ?? 'human'
    const item = this.must(itemId)
    if (item.closed) return
    transaction(this.db, () => {
      for (const b of item.blockers) this.resolve(b.id, { cancelled: true, detail: 'The item closed.' }, actor, true)
      this.add(itemId, [{ type: 'closed', params: { how: opts.how ?? 'closed' } }], actor)
      this.db.prepare('UPDATE items SET closed = 1, updated_at = ? WHERE id = ?').run(this.now(), itemId)
      this.mark(itemId, 'item_closed', { actor, data: { how: opts.how ?? 'closed' } })
    })
    if (item.worker !== null) this.onCancel(itemId)
    this.wake()
    this.emit('change')
    await this.idle()
  }

  handledOutside(itemId: string): Promise<void> {
    return this.close(itemId, { how: 'handled_outside' })
  }

  // ---- bots ----

  /**
   * Gives `worker` the item whose bot blocker has waited longest. The run answers
   * every bot blocker the item has, in the item's own Claude session.
   */
  claim(worker: number): { item: Item; events: ItemEvent[]; sessionId: string; resume: boolean } | null {
    const r = this.db
      .prepare(
        `SELECT e.item_id FROM events e JOIN items i ON i.id = e.item_id
         WHERE e.resolved = 0 AND e.type = 'bot' AND i.worker IS NULL AND i.closed = 0 ORDER BY e.created_at LIMIT 1`,
      )
      .get() as Row | undefined
    if (!r) return null
    const itemId = r.item_id as string
    const { session_id } = this.db.prepare('SELECT session_id FROM items WHERE id = ?').get(itemId) as Row
    const sessionId = (session_id as string) ?? randomUUID()
    this.db.prepare('UPDATE items SET worker = ?, session_id = ? WHERE id = ?').run(worker, sessionId, itemId)
    this.emit('change')
    const item = this.must(itemId)
    return { item, events: item.blockers.filter((b) => b.type === 'bot'), sessionId, resume: Boolean(session_id) }
  }

  /**
   * What is new in an item for its bot: everything, the first time; after that,
   * every event added or resolved since the bot was last prompted, less what its
   * own runs did (the session already holds that).
   */
  newSinceLastRun(itemId: string): ItemEvent[] {
    const last = this.db.prepare('SELECT trace_mark FROM runs WHERE item_id = ? ORDER BY started_at DESC, rowid DESC LIMIT 1').get(itemId) as Row | undefined
    const events = this.events(itemId)
    if (!last) return events
    const touched = new Set(
      (
        this.db
          .prepare(
            `SELECT DISTINCT event_id FROM trace WHERE item_id = ? AND id > ? AND kind IN ('event_added', 'event_resolved', 'event_cancelled') AND actor != 'bot'`,
          )
          .all(itemId, last.trace_mark) as Row[]
      ).map((r) => r.event_id as string),
    )
    return events.filter((e) => touched.has(e.id))
  }

  startRun(item: Item, events: ItemEvent[], worker: number, prompt: string): string {
    const id = randomUUID()
    const { mark } = this.db.prepare('SELECT COALESCE(MAX(id), 0) AS mark FROM trace').get() as Row
    this.db
      .prepare('INSERT INTO runs (id, item_id, event_ids, worker, started_at, trace_mark, prompt) VALUES (?, ?, ?, ?, ?, ?, ?)')
      .run(id, item.id, JSON.stringify(events.map((e) => e.id)), worker, this.now(), mark, prompt)
    for (const e of events) this.mark(item.id, 'run_started', { eventId: e.id, runId: id, actor: 'bot', data: { worker } })
    return id
  }

  appendTranscript(runId: string, chunk: string): void {
    this.db.prepare('UPDATE runs SET transcript = transcript || ? WHERE id = ?').run(chunk, runId)
  }

  /** The item of a run still going, or null. */
  activeRun(runId: string): { itemId: string; eventIds: string[] } | null {
    const r = this.db.prepare('SELECT item_id, event_ids FROM runs WHERE id = ? AND ended_at IS NULL').get(runId) as Row | undefined
    return r ? { itemId: r.item_id as string, eventIds: json(r.event_ids) ?? [] } : null
  }

  /**
   * A run is over. Its output answers its bot blockers: a message to the human,
   * events to add, or closing the item. What it said on the way is kept in their
   * data. If anything is wrong, a message says so.
   */
  async endRun(
    runId: string,
    end: { exitCode: number | null; structured?: unknown; output?: string; costUsd?: number; turns?: number; error?: string },
  ): Promise<void> {
    const run = this.db.prepare('SELECT * FROM runs WHERE id = ?').get(runId) as Row | undefined
    if (!run) return
    const itemId = run.item_id as string
    const eventIds: string[] = json(run.event_ids) ?? []
    this.db
      .prepare('UPDATE runs SET ended_at = ?, cost_usd = ?, turns = ?, result = ? WHERE id = ?')
      .run(this.now(), end.costUsd ?? null, end.turns ?? null, end.output ?? end.error ?? null, runId)
    for (const e of eventIds)
      this.mark(itemId, 'run_ended', { eventId: e, runId, actor: 'bot', data: { exitCode: end.exitCode, costUsd: end.costUsd ?? null, turns: end.turns ?? null, error: end.error ?? null } })
    this.db.prepare('UPDATE items SET worker = NULL WHERE id = ? AND worker = ?').run(itemId, run.worker)
    const open = eventIds.filter((id) => !this.event(id)?.resolved)
    if (!open.length) {
      this.emit('change')
      return // every blocker it answered was cancelled while it ran
    }

    let out: BotOutput | null = null
    let error = end.error ?? null
    let events: EventSpec[] = []
    if (!error) {
      try {
        out = z
          .object({
            message: z.string().optional(),
            events: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })).optional(),
            close: z.boolean().optional(),
          })
          .parse(end.structured ?? {}) as BotOutput
        events = this.prepare(out.events ?? [], 'bot')
      } catch (e) {
        error = `The bot's output was not usable: ${(e as Error).message}`
      }
    }
    const base = { runId, output: end.output ?? '', ...(end.costUsd !== undefined ? { costUsd: end.costUsd } : {}) }
    transaction(this.db, () => {
      if (error || !out) {
        for (const id of open) this.resolve(id, { ...base, error, detail: error }, 'bot')
        this.add(itemId, [{ type: 'message', params: { text: error ?? 'The bot failed.' } }], 'system')
        return
      }
      if (out.message) this.add(itemId, [{ type: 'message', params: { text: out.message } }], 'bot')
      for (const id of open) this.resolve(id, { ...base, message: out.message ?? null }, 'bot')
      if (events.length) this.add(itemId, events, 'bot')
    })
    if (out?.close) await this.close(itemId, { actor: 'bot' })
    this.emit('change')
    await this.idle()
  }

  // ---- the loop ----

  /** One turn of the central loop: every blocker check that is due, then every source that is. */
  async tick(): Promise<void> {
    await this.checkDue()
    await this.pollSources(false)
  }

  async checkDue(): Promise<void> {
    const due = this.db
      .prepare('SELECT id, item_id FROM events WHERE resolved = 0 AND started = 1 AND next_check_at IS NOT NULL AND next_check_at <= ?')
      .all(this.now()) as Row[]
    await Promise.all(due.map((r) => this.checkOne(r.item_id as string, r.id as string)))
  }

  private checkOne(itemId: string, eventId: string): Promise<void> {
    return this.serial(itemId, async () => {
      const row = this.db.prepare('SELECT * FROM events WHERE id = ? AND resolved = 0').get(eventId) as Row | undefined
      if (!row || (row.next_check_at as number) > this.now()) return
      const event = toEvent(row)
      const type = this.type(event.type)
      let data: Record<string, unknown> | null = null
      let error: string | null = null
      try {
        data = await type.check!(event.params, { engine: this, item: this.must(itemId), event, author: authorOf(event.createdBy) })
      } catch (e) {
        error = (e as Error).message
      }
      const timeout = Number(event.params.timeout_minutes ?? 0)
      if (!data && timeout && this.now() >= event.createdAt + timeout * MINUTE)
        data = { outcome: 'timed_out', detail: `Nothing after ${timeout} minutes.` }
      this.mark(itemId, 'check', {
        eventId,
        data: data ? { result: 'resolved', outcome: data.outcome ?? null } : error ? { result: 'error', error } : { result: 'pending' },
      })
      if (data) return this.resolve(eventId, data, 'system')
      const step = Math.min((row.check_step as number) + 1, BACKOFF_MINUTES.length - 1)
      this.db
        .prepare('UPDATE events SET check_step = ?, next_check_at = ?, last_error = ? WHERE id = ?')
        .run(step, this.now() + BACKOFF_MINUTES[step] * MINUTE, error, eventId)
      this.emit('change')
    })
  }

  async pollSources(force: boolean): Promise<void> {
    for (const source of this.sources) {
      const last = this.lastPoll.get(source.id) ?? 0
      if (!force && this.now() - last < source.everyMinutes * MINUTE) continue
      this.lastPoll.set(source.id, this.now())
      try {
        for (const found of await source.poll(this))
          await this.create(found.title, { source: source.id, ref: found.ref, url: found.url, events: found.events, actor: source.id })
      } catch (e) {
        this.emit('sourceError', `${source.id}: ${(e as Error).message}`)
      }
    }
  }
}
