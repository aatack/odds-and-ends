import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { z } from 'zod'
import type { BotRun, EventDetail, EventSpec, EventTimes, Item, ItemDetail, ItemEvent, Overview, TraceEntry } from '../shared/types'
import { itemActions } from './actions'
import { migrate, transaction, type Db } from './db'
import { EVENT_TYPES } from './eventTypes'
import { githubActions, githubSources } from './github'
import { execCli, type Action, type ActionContext, type Author, type EventType, type Exec, type Source } from './integrations'
import { PRESETS, type Preset } from './presets'
import { plainSecrets, Settings, type Secrets } from './settings'
import { slackActions } from './slack'

/** How long a blocker sleeps between checks, by default: at once, then a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000
const DEFAULT_SCHEDULE = [0, ...BACKOFF_MINUTES.map((m) => m * 60)]

/** What a bot's run returns, as its structured output. */
export interface BotOutput {
  /** To the human: what it did, or one direct question. */
  message?: string
  /** Blockers to add in its item: more bots, waits, approvals, afters, child items. */
  events?: EventSpec[]
  close?: boolean
}

type Row = Record<string, unknown>
const json = <T>(s: unknown): T | null => (typeof s === 'string' && s ? (JSON.parse(s) as T) : null)

const NO_TIMES: EventTimes = {
  added: null, firstChecked: null, lastChecked: null, checks: 0, firstRunStarted: null, firstRunEnded: null, runs: 0, resolved: null,
}

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

export interface EngineOptions {
  db: Db
  secrets?: Secrets
  exec?: Exec
  fetch?: typeof fetch
  now?: () => number
  sources?: Source[]
}

/**
 * The whole system, short of the processes: one tree of events (items among
 * them), the trace, and the loop that checks the blockers. Every change goes
 * through here, and emits `change`.
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
  /** The items a bot is running on, and its worker. In memory only: a crash leaves nothing behind. */
  private running = new Map<string, number>()
  private lastPoll = new Map<string, number>()
  /** The events being checked right now: a check never starts while the last is still going. */
  private checking = new Set<string>()
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
    // A run the app was closed during is over; its bot blockers wait for a worker again.
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

  /** A row as the shared event shape, its text filled in by its type. */
  private toEvent(r: Row, times: EventTimes = NO_TIMES): ItemEvent {
    const type = this.type(r.type as string)
    const params: Record<string, unknown> = json(r.params) ?? {}
    const data: Record<string, unknown> | null = json(r.data)
    const resolved = Boolean(r.resolved)
    const cancelled = Boolean(r.cancelled)
    const summary = type.describe(params)
    return {
      id: r.id as string,
      parentId: (r.parent_id as string) ?? null,
      type: type.type,
      params,
      createdBy: r.created_by as string,
      createdAt: r.created_at as number,
      resolved,
      resolvedAt: (r.resolved_at as number) ?? null,
      resolvedBy: (r.resolved_by as string) ?? null,
      cancelled,
      data,
      actor: (type.blocking ? (r.resolved_by as string) : null) ?? (r.created_by as string),
      summary,
      message: !resolved ? null : cancelled ? `Cancelled: ${summary}` : type.said ? type.said(params, data ?? {}) : summary,
      nextCheckAt: (r.next_check_at as number) ?? null,
      lastError: (r.last_error as string) ?? null,
      times,
    }
  }

  private row(id: string): Row | undefined {
    return this.db.prepare('SELECT * FROM events WHERE id = ?').get(id) as Row | undefined
  }

  event(id: string): ItemEvent | null {
    const r = this.row(id)
    return r ? this.toEvent(r, this.times([id]).get(id)) : null
  }

  /** The events in an item, oldest first; only the open ones (its blockers) with `open`. */
  children(itemId: string, open = false): ItemEvent[] {
    const rows = this.db
      .prepare(`SELECT * FROM events WHERE parent_id = ? ${open ? 'AND resolved = 0' : ''} ORDER BY seq`)
      .all(itemId) as Row[]
    const times = this.times(rows.map((r) => r.id as string))
    return rows.map((r) => this.toEvent(r, times.get(r.id as string)))
  }

  /** Events' timelines, read from the trace. */
  private times(ids: string[]): Map<string, EventTimes> {
    const out = new Map<string, EventTimes>()
    if (!ids.length) return out
    const rows = this.db
      .prepare(
        `SELECT event_id, kind, MIN(at) AS first, MAX(at) AS last, COUNT(*) AS n FROM trace
         WHERE event_id IN (SELECT value FROM json_each(?)) GROUP BY event_id, kind`,
      )
      .all(JSON.stringify(ids)) as Row[]
    for (const r of rows) {
      const t = out.get(r.event_id as string) ?? { ...NO_TIMES }
      const [first, last, n] = [r.first as number, r.last as number, Number(r.n)]
      if (r.kind === 'event_added') t.added = first
      if (r.kind === 'check') Object.assign(t, { firstChecked: first, lastChecked: last, checks: n })
      if (r.kind === 'run_started') Object.assign(t, { firstRunStarted: first, runs: n })
      if (r.kind === 'run_ended') t.firstRunEnded = first
      if (r.kind === 'event_resolved' || r.kind === 'event_cancelled') t.resolved = first
      out.set(r.event_id as string, t)
    }
    return out
  }

  /** An item event as the lists and the item view read it. */
  private toItem(r: Row): Item {
    const id = r.id as string
    const params: Record<string, unknown> = json(r.params) ?? {}
    const blockers = this.children(id, true)
    const last = this.db
      .prepare('SELECT MAX(MAX(created_at), COALESCE(MAX(resolved_at), 0)) AS at FROM events WHERE parent_id = ?')
      .get(id) as Row
    const closed = Boolean(r.resolved)
    const marked = this.db
      .prepare("SELECT params FROM events WHERE parent_id = ? AND type = 'priority' ORDER BY seq DESC LIMIT 1")
      .get(id) as Row | undefined
    return {
      id,
      title: String(params.title),
      parentId: (r.parent_id as string) ?? null,
      closed,
      how: (json<Record<string, unknown>>(r.data)?.how as string) ?? null,
      blockers,
      withHuman: !closed && blockers.every((b) => this.waitsOnHuman(b, blockers)),
      priority: Boolean(marked && json<Record<string, unknown>>(marked.params)?.on),
      worker: this.running.get(id) ?? null,
      sessionId: (params.sessionId as string) ?? null,
      since: Math.max((last.at as number) ?? 0, r.created_at as number),
      createdAt: r.created_at as number,
      closedAt: (r.resolved_at as number) ?? null,
    }
  }

  /** A blocker the human resolves, or an after waiting on one: the item is still the human's turn. */
  private waitsOnHuman(b: ItemEvent, blockers: ItemEvent[], depth = 0): boolean {
    if (this.type(b.type).human) return true
    const waited = b.type === 'after' && depth < 20 && blockers.find((x) => x.id === b.params.event)
    return Boolean(waited && this.waitsOnHuman(waited, blockers, depth + 1))
  }

  /** The item event with this id, or null. */
  get(id: string): Item | null {
    const r = this.db.prepare("SELECT * FROM events WHERE id = ? AND type = 'item'").get(id) as Row | undefined
    return r ? this.toItem(r) : null
  }

  private must(id: string): Item {
    const item = this.get(id)
    if (!item) throw new Error(`No item ${id}.`)
    return item
  }

  /** Every open item, at any depth, and those closed in the last day. */
  overview(workers: Overview['workers'] = []): Overview {
    const rows = this.db
      .prepare("SELECT * FROM events WHERE type = 'item' AND (resolved = 0 OR resolved_at > ?) ORDER BY created_at")
      .all(this.now() - 24 * 60 * MINUTE) as Row[]
    return { items: rows.map((r) => this.toItem(r)).sort((a, b) => a.since - b.since), workers }
  }

  detail(id: string): ItemDetail | null {
    const item = this.get(id)
    if (!item) return null
    const parent = item.parentId ? this.get(item.parentId) : null
    return {
      item,
      parent: parent && { id: parent.id, title: parent.title },
      events: this.children(id),
      runs: (this.db.prepare('SELECT * FROM runs WHERE item_id = ? ORDER BY started_at').all(id) as Row[]).map(toRun),
      trace: this.trace(id),
    }
  }

  /** Any event, item or not, with what its views need. */
  eventDetail(id: string): EventDetail | null {
    const event = this.event(id)
    if (!event) return null
    const parent = event.parentId ? this.get(event.parentId) : null
    const runs = (
      this.db
        .prepare('SELECT * FROM runs WHERE item_id = ? OR ? IN (SELECT value FROM json_each(event_ids)) ORDER BY started_at')
        .all(id, id) as Row[]
    ).map(toRun)
    return {
      event,
      parent: parent && { id: parent.id, title: parent.title },
      runs,
      trace: (this.db.prepare('SELECT * FROM trace WHERE event_id = ? ORDER BY id').all(id) as Row[]).map(toTrace),
      item: event.type === 'item' ? this.detail(id) : null,
    }
  }

  /**
   * The item that has waited longest for the human, leaving out `skip`. An open
   * item is the human's when nothing but its approvals blocks it, so a child
   * item comes before the item it blocks.
   */
  /** What needs the human next: their priority items, the most recently active first; then the rest, the longest waiting first. */
  next(skip: string[] = []): string | null {
    const rows = this.db.prepare("SELECT * FROM events WHERE type = 'item' AND resolved = 0").all() as Row[]
    const open = rows.map((r) => this.toItem(r)).filter((i) => i.withHuman && !skip.includes(i.id))
    const first = open.filter((i) => i.priority).sort((a, b) => b.since - a.since)[0] ?? open.sort((a, b) => a.since - b.since)[0]
    return first?.id ?? null
  }

  /**
   * Where an item stands now, in a few lines, from a cheap model (Haiku, with no
   * tools and no session kept): what is current rather than its history. Kept
   * in the trace; not added to the item.
   */
  async summarise(itemId: string, actor = 'human'): Promise<string> {
    const item = this.must(itemId)
    const lines = this.children(itemId).map(
      (e) => `- ${e.createdBy} ${e.type}${e.resolved ? '' : ' (still open)'}: ${e.type === 'message' ? String(e.params.text ?? '') : (e.message ?? e.summary)}`,
    )
    const prompt = [`Task: ${item.title} (${item.closed ? 'resolved' : item.withHuman ? "the human's turn" : 'waiting on others'})`, 'Its history, oldest first:', ...lines].join('\n')
    const started = this.now()
    const out = await this.exec('claude', [
      '-p', prompt, '--model', 'haiku', '--tools', '', '--strict-mcp-config', '--no-session-persistence',
      '--system-prompt',
      'Summarise where this task stands now for the person who owns it: what it is, what has been decided or done that still matters, ' +
        'what is open or waiting and on whom, and what they need to do next, if anything. The current state, not the history: no ' +
        'narration of who said what when. A few short markdown bullets, plain words, nothing else.',
    ])
    const summary = out.trim()
    this.mark(itemId, 'summarised', { actor, data: { ms: this.now() - started, summary } })
    return summary
  }

  /** Marks an item as a priority, or not, with a priority event in it. */
  async setPriority(itemId: string, on: boolean, actor = 'human'): Promise<void> {
    if (this.must(itemId).priority === on) return
    await this.addEvents(itemId, [{ type: 'priority', params: { on } }], actor)
  }

  // ---- the trace ----

  trace(itemId: string): TraceEntry[] {
    return (this.db.prepare('SELECT * FROM trace WHERE item_id = ? ORDER BY id').all(itemId) as Row[]).map(toTrace)
  }

  /** Records one thing that happened in an item. Every change, view, check and call comes through here. */
  mark(
    itemId: string,
    kind: string,
    opts: { eventId?: string | null; runId?: string | null; actor?: string; data?: Record<string, unknown> } = {},
  ): void {
    this.db
      .prepare('INSERT INTO trace (at, item_id, event_id, run_id, actor, kind, data) VALUES (?, ?, ?, ?, ?, ?, ?)')
      .run(this.now(), itemId, opts.eventId ?? null, opts.runId ?? null, opts.actor ?? 'system', kind, opts.data ? JSON.stringify(opts.data) : null)
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
    const ids = (
      this.db
        .prepare(
          `SELECT id FROM events WHERE type = 'item' AND (created_at >= ? OR resolved_at >= ?
             OR id IN (SELECT item_id FROM trace WHERE at >= ?)) ORDER BY created_at`,
        )
        .all(since, since, since) as Row[]
    ).map((r) => r.id as string)
    return ids.map((id) => {
      const item = this.must(id)
      const events = this.children(id)
      const blocked: Record<string, number> = {}
      for (const e of events.filter((e) => this.type(e.type).blocking))
        blocked[e.type] = (blocked[e.type] ?? 0) + ((e.resolvedAt ?? this.now()) - e.createdAt) / MINUTE
      const runs = this.db.prepare('SELECT COUNT(*) AS n, SUM(cost_usd) AS cost FROM runs WHERE item_id = ?').get(id) as Row
      const trace = this.trace(id)
      return {
        id,
        title: item.title,
        parentId: item.parentId,
        closed: item.closed,
        how: item.how,
        events: events.map((e) => e.type),
        minutesBlockedBy: Object.fromEntries(Object.entries(blocked).map(([k, v]) => [k, Math.round(v)])),
        botRuns: Number(runs.n),
        costUsd: (runs.cost as number) ?? 0,
        humanMessages: events.filter((e) => e.type === 'message' && e.createdBy === 'human').length,
        approvalsChanged: events.filter((e) => ((e.data?.changed as string[] | undefined) ?? []).length).length,
        cancelled: events.filter((e) => e.cancelled).length,
        timedOut: events.filter((e) => e.data?.outcome === 'timed_out').length,
        botFailures: events.filter((e) => e.type === 'bot' && e.data?.error).length,
        handledOutside: item.how === 'handled_outside',
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
      // Prepared now, so a bad one fails the whole list; added in the item as it starts.
      if (s.type === 'item' && params.events) params.events = this.prepare(params.events as EventSpec[], author)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e, i) => ({ type: e.type, params: e.params, ref: i === 0 ? (s.ref ?? e.ref) : e.ref }))
    })
  }

  /**
   * Adds events, already prepared, in an item (or, for items, in none). Plain ones
   * resolve at once; blockers wait, and those whose type has a start hook start
   * right after. An item whose key is already taken is left out. Answers the ids added.
   */
  add(parentId: string | null, specs: EventSpec[], createdBy: string, runId: string | null = null): string[] {
    // A detached item sits in no item, wherever it is added.
    const detached = parentId ? specs.filter((s) => s.type === 'item' && s.params.detached) : []
    if (detached.length)
      return [...this.add(parentId, specs.filter((s) => !detached.includes(s)), createdBy, runId), ...this.add(null, detached, createdBy, runId)]
    const keyed = (s: EventSpec) =>
      s.type === 'item' && s.params.key &&
      this.db.prepare("SELECT 1 FROM events WHERE type = 'item' AND json_extract(params, '$.key') = ?").get(s.params.key)
    specs = specs.filter((s) => !keyed(s))
    if (!parentId && specs.some((s) => s.type !== 'item')) throw new Error('Only an item can sit in no item.')
    const ids = new Map<string, string>()
    // A bot already queued in the item (open, and no run has it yet) will be told
    // everything new when it runs, so another with nothing more to ask is left out:
    // its ref stands for the queued one. One that is running doesn't count.
    const queued = parentId ? this.queuedBot(parentId) : null
    if (queued)
      specs = specs.filter((s) => {
        const same = s.type === 'bot' && (!s.params.instructions || s.params.instructions === queued.params.instructions)
        if (same && s.ref) ids.set(s.ref, queued.id)
        return !same
      })
    const rows = specs.map((s) => {
      const id = randomUUID()
      if (s.ref) ids.set(s.ref, id)
      return { id, spec: s }
    })
    const { seq } = this.db.prepare('SELECT COALESCE(MAX(seq), 0) AS seq FROM events WHERE parent_id IS ?').get(parentId) as Row
    const at = this.now()
    rows.forEach(({ id, spec }, i) => {
      const type = this.type(spec.type)
      const params = resolveRefs(spec.params, ids) as Record<string, unknown>
      this.db
        .prepare(
          `INSERT INTO events (id, parent_id, seq, type, params, created_by, created_at, resolved, resolved_at, resolved_by, started, next_check_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        )
        .run(id, parentId, Number(seq) + i + 1, spec.type, JSON.stringify(params), createdBy, at,
          type.blocking ? 0 : 1, type.blocking ? null : at, type.blocking ? null : createdBy, type.start ? 0 : 1,
          type.check ? at + this.scheduleOf(type, params)[0] * 1000 : null)
      const where = parentId ?? id
      this.mark(where, 'event_added', { eventId: id, runId, actor: createdBy, data: { type: spec.type } })
      if (!type.blocking) this.mark(where, 'event_resolved', { eventId: id, runId, actor: createdBy })
    })
    for (const { id, spec } of rows) if (this.type(spec.type).start) void this.startPending(parentId ?? id)
    // New waits are checked at once; an after may already have what it waits for.
    if (rows.some(({ spec }) => this.type(spec.type).check)) queueMicrotask(() => void this.checkDue())
    this.emit('change')
    return rows.map((r) => r.id)
  }

  /** Runs the start hooks of the new blockers in an item (and of the item itself). */
  private startPending(itemId: string): Promise<void> {
    return this.serial(itemId, async () => {
      const pending = (
        this.db.prepare('SELECT * FROM events WHERE (parent_id = ? OR id = ?) AND started = 0 AND resolved = 0').all(itemId, itemId) as Row[]
      ).map((r) => this.toEvent(r))
      for (const event of pending) {
        this.db.prepare('UPDATE events SET started = 1 WHERE id = ?').run(event.id)
        try {
          const ctx = { engine: this, item: this.must(event.parentId ?? event.id), event, author: authorOf(event.createdBy), memo: null, setMemo: () => {} }
          const out = await this.type(event.type).start!(event.params, ctx)
          if (out?.params) this.db.prepare('UPDATE events SET params = ? WHERE id = ?').run(JSON.stringify(out.params), event.id)
          if (out?.resolve) this.resolve(event.id, out.resolve, 'system')
        } catch (e) {
          this.resolve(event.id, { outcome: 'error', error: (e as Error).message, detail: `Could not start: ${(e as Error).message}` }, 'system')
        }
      }
      this.emit('change')
    })
  }

  /** Resolves a blocker, and wakes whatever was waiting on it. */
  private resolve(eventId: string, data: Record<string, unknown>, by: string, cancelled = false, runId: string | null = null): void {
    const r = this.db.prepare('SELECT parent_id FROM events WHERE id = ? AND resolved = 0').get(eventId) as Row | undefined
    if (!r) return
    this.db
      .prepare('UPDATE events SET resolved = 1, resolved_at = ?, resolved_by = ?, cancelled = ?, data = ?, next_check_at = NULL WHERE id = ?')
      .run(this.now(), by, cancelled ? 1 : 0, JSON.stringify(data), eventId)
    this.mark((r.parent_id as string) ?? eventId, cancelled ? 'event_cancelled' : 'event_resolved', { eventId, runId, actor: by })
    this.wake()
  }

  /** Afters are checked at once after anything resolves, rather than at their next turn. */
  private wake(): void {
    this.db.prepare("UPDATE events SET next_check_at = ? WHERE resolved = 0 AND type = 'after'").run(this.now())
    queueMicrotask(() => void this.checkDue())
  }

  /** An event's check schedule, in seconds: its type's, never under a second apart after the first, or the default. */
  private scheduleOf(type: EventType, params: Record<string, unknown>): number[] {
    const s = type.schedule?.(params) ?? DEFAULT_SCHEDULE
    const delays = s.length ? s : DEFAULT_SCHEDULE
    return delays.map((d, i) => (i === 0 ? Math.max(0, d) : Math.max(1, d)))
  }

  /** Writes one param of an event: for the app's own bookkeeping, such as an item's session. */
  private setParam(eventId: string, key: string, value: unknown): void {
    const row = this.row(eventId)!
    this.db.prepare('UPDATE events SET params = ? WHERE id = ?').run(JSON.stringify({ ...json<Record<string, unknown>>(row.params), [key]: value }), eventId)
  }

  // ---- the human ----

  /** A new item, in `parentId` or in none, with events to start it off. */
  async create(
    title: string,
    opts: { parentId?: string | null; key?: string; events?: EventSpec[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const author = opts.author ?? 'human'
    const [spec] = this.prepare([{ type: 'item', params: { title, ...(opts.key ? { key: opts.key } : {}), ...(opts.events ? { events: opts.events } : {}) } }], author)
    const [id] = this.add(opts.parentId ?? null, [spec], author === 'bot' ? 'bot' : (opts.actor ?? 'human'))
    await this.idle()
    return id ?? null
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

  /** Adds events in an item (or items in none): from the human, or from an agent through MCP (author 'bot', so its rules apply). */
  async addEvents(parentId: string | null, specs: EventSpec[], actor = 'human', author: Author = 'human', runId: string | null = null): Promise<string[]> {
    if (parentId && this.must(parentId).closed) throw new Error('This item is closed.')
    const ids = this.add(parentId, this.prepare(specs, author), actor, runId)
    await this.idle()
    return ids
  }

  /**
   * Attaches an event (any, items included) to an item, to gather it there: an
   * attachment event in the item points at it. The event stays where it was.
   */
  async attach(eventId: string, targetId: string, actor = 'human', author: Author = 'human', runId: string | null = null): Promise<string> {
    const e = this.event(eventId)
    if (!e) throw new Error(`No event ${eventId}.`)
    if (this.event(targetId)?.type !== 'item') throw new Error(`No item ${targetId}.`)
    if (eventId === targetId) throw new Error('An item can not be attached to itself.')
    const params = { event: e.id, title: e.summary, item: e.type === 'item' ? e.id : e.parentId }
    const [id] = await this.addEvents(targetId, [{ type: 'attachment', params }], actor, author, runId)
    return id
  }

  /** Runs an approval's action with the human's inputs, which resolves it. */
  async approve(eventId: string, inputs: Record<string, unknown>): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.type !== 'approval' || event.resolved) throw new Error('No approval waiting with that id.')
    const proposed = event.params.inputs as Record<string, unknown>
    const changed = Object.keys({ ...proposed, ...inputs }).filter((k) => String(inputs[k] ?? '') !== String(proposed[k] ?? ''))
    const name = String(event.params.action)
    const output = await this.callAction(name, inputs, { engine: this, author: 'human', actor: 'human', itemId: event.parentId, runId: null, approving: true })
    this.resolve(eventId, { ran: true, inputs, changed, output }, 'human')
    this.emit('change')
    await this.idle()
  }

  /** Ends a blocker without it happening; a bot running for it is stopped, and an item is closed as cancelled. */
  async cancel(eventId: string, actor = 'human'): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.resolved) throw new Error('That event is not waiting.')
    if (event.type === 'item') return this.close(eventId, { actor, how: 'cancelled' })
    this.resolve(eventId, { cancelled: true }, actor, true)
    if (event.type === 'bot' && event.parentId && this.running.has(event.parentId)) this.onCancel(event.parentId)
    this.emit('change')
    await this.idle()
  }

  /** Closes an item: its open children are cancelled (child items closed, all the way down), then it resolves. */
  async close(itemId: string, opts: { actor?: string; how?: 'closed' | 'handled_outside' | 'cancelled'; runId?: string } = {}): Promise<void> {
    const actor = opts.actor ?? 'human'
    const runId = opts.runId ?? null
    const shut = (id: string, how: string) => {
      for (const b of this.children(id, true))
        if (b.type === 'item') shut(b.id, 'cancelled')
        else this.resolve(b.id, { cancelled: true, detail: 'Its item closed.' }, actor, true, runId)
      this.resolve(id, { how }, actor, false, runId)
      if (this.running.has(id)) this.onCancel(id)
    }
    const item = this.must(itemId)
    if (item.closed) return
    transaction(this.db, () => shut(itemId, opts.how ?? 'closed'))
    this.emit('change')
    await this.idle()
  }

  handledOutside(itemId: string): Promise<void> {
    return this.close(itemId, { how: 'handled_outside' })
  }

  // ---- bots ----

  /**
   * Gives `worker` the item whose bot blocker has waited longest. The run answers
   * every bot blocker in the item, in the item's own Claude session: resumed if it
   * has one; otherwise a new one, forked from the nearest item above with one, so
   * it knows how the item came about.
   */
  claim(worker: number): { item: Item; events: ItemEvent[]; sessionId: string; resume: boolean; forkFrom: string | null } | null {
    const busy = [...this.running.keys()]
    const r = this.db
      .prepare(
        `SELECT e.parent_id FROM events e JOIN events i ON i.id = e.parent_id
         WHERE e.resolved = 0 AND e.type = 'bot' AND i.resolved = 0
           AND e.parent_id NOT IN (SELECT value FROM json_each(?)) ORDER BY e.created_at LIMIT 1`,
      )
      .get(JSON.stringify(busy)) as Row | undefined
    if (!r) return null
    const itemId = r.parent_id as string
    this.running.set(itemId, worker)
    let item = this.must(itemId)
    const resume = item.sessionId !== null
    let forkFrom: string | null = null
    if (!resume) {
      for (let up = item.parentId ? this.get(item.parentId) : null; up && !forkFrom; up = up.parentId ? this.get(up.parentId) : null)
        forkFrom = up.sessionId
      // The session's id is the item's from now on, whether the run goes well or not.
      this.setParam(itemId, 'sessionId', randomUUID())
      item = this.must(itemId)
    }
    this.emit('change')
    return { item, events: item.blockers.filter((b) => b.type === 'bot'), sessionId: item.sessionId!, resume, forkFrom }
  }

  /** An open bot blocker in the item that no run has taken up yet, or null. */
  queuedBot(itemId: string): ItemEvent | null {
    const taken = new Set(
      (this.db.prepare('SELECT event_ids FROM runs WHERE item_id = ? AND ended_at IS NULL').all(itemId) as Row[]).flatMap((r) => json<string[]>(r.event_ids) ?? []),
    )
    return this.children(itemId).find((e) => e.type === 'bot' && !e.resolved && !taken.has(e.id)) ?? null
  }

  /**
   * What is new in an item for its bot: everything, the first time; after that,
   * every event added or resolved since the bot was last prompted, less what this
   * item's own runs did (the session already holds that). What other bots did,
   * a child item's included, is new.
   */
  newSinceLastRun(itemId: string): ItemEvent[] {
    const last = this.db.prepare('SELECT trace_mark FROM runs WHERE item_id = ? ORDER BY started_at DESC, rowid DESC LIMIT 1').get(itemId) as Row | undefined
    const events = this.children(itemId)
    if (!last) return events
    const touched = new Set(
      (
        this.db
          .prepare(
            `SELECT DISTINCT event_id FROM trace WHERE item_id = ? AND id > ? AND kind IN ('event_added', 'event_resolved', 'event_cancelled')
             AND (run_id IS NULL OR run_id NOT IN (SELECT id FROM runs WHERE item_id = ?))`,
          )
          .all(itemId, last.trace_mark, itemId) as Row[]
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
   * events to add in the item, or closing it. What it said on the way is kept in
   * their data. If anything is wrong, the bot events say so.
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
    if (this.running.get(itemId) === (run.worker as number)) this.running.delete(itemId)
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
        for (const id of open) this.resolve(id, { ...base, error: error ?? 'The bot failed.' }, 'bot', false, runId)
        return
      }
      if (out.message) this.add(itemId, [{ type: 'message', params: { text: out.message } }], 'bot', runId)
      for (const id of open) this.resolve(id, { ...base, message: out.message ?? null }, 'bot', false, runId)
      if (events.length) this.add(itemId, events, 'bot', runId)
    })
    if (out?.close) await this.close(itemId, { actor: 'bot', runId })
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
      .prepare('SELECT id, parent_id FROM events WHERE resolved = 0 AND started = 1 AND next_check_at IS NOT NULL AND next_check_at <= ?')
      .all(this.now()) as Row[]
    await Promise.all(due.filter((r) => !this.checking.has(r.id as string)).map((r) => this.checkOne(r.parent_id as string, r.id as string)))
  }

  private checkOne(itemId: string, eventId: string): Promise<void> {
    this.checking.add(eventId)
    return this.serial(itemId, async () => {
      const row = this.db.prepare('SELECT * FROM events WHERE id = ? AND resolved = 0').get(eventId) as Row | undefined
      if (!row || (row.next_check_at as number) > this.now()) return
      const event = this.toEvent(row, this.times([eventId]).get(eventId))
      const type = this.type(event.type)
      let data: Record<string, unknown> | null = null
      let error: string | null = null
      try {
        data = await type.check!(event.params, {
          engine: this,
          item: this.must(itemId),
          event,
          author: authorOf(event.createdBy),
          memo: json(row.memo),
          setMemo: (value) => this.db.prepare('UPDATE events SET memo = ? WHERE id = ?').run(JSON.stringify(value ?? null), eventId),
        })
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
      const schedule = this.scheduleOf(type, event.params)
      const step = Math.min((row.check_step as number) + 1, schedule.length - 1)
      this.db
        .prepare('UPDATE events SET check_step = ?, next_check_at = ?, last_error = ? WHERE id = ?')
        .run(step, this.now() + schedule[step] * 1000, error, eventId)
      this.emit('change')
    }).finally(() => this.checking.delete(eventId))
  }

  async pollSources(force: boolean): Promise<void> {
    for (const source of this.sources) {
      const last = this.lastPoll.get(source.id) ?? 0
      if (!force && this.now() - last < source.everyMinutes * MINUTE) continue
      this.lastPoll.set(source.id, this.now())
      try {
        this.add(null, this.prepare(await source.poll(this), 'human'), source.id)
        await this.idle()
      } catch (e) {
        this.emit('sourceError', `${source.id}: ${(e as Error).message}`)
      }
    }
  }
}
