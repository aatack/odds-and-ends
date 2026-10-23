import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { z } from 'zod'
import type { BotRun, Handoff, HandoffSpec, HandoffTimes, Item, ItemDetail, ItemEvent, Overview, TraceEntry } from '../shared/types'
import { itemActions } from './actions'
import { migrate, transaction, type Db } from './db'
import { githubActions, githubSources } from './github'
import { HANDOFF_TYPES } from './handoffs'
import { execCli, type Action, type ActionContext, type Author, type Exec, type HandoffType, type Source } from './integrations'
import { plainSecrets, Settings, type Secrets } from './settings'
import { slackActions } from './slack'

/** How long a wait sleeps between checks: a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000

/** What a bot's run returns, as its structured output. */
export interface BotOutput {
  next: HandoffSpec[]
  close?: boolean
}

type Row = Record<string, unknown>
const json = <T>(s: unknown): T | null => (typeof s === 'string' && s ? (JSON.parse(s) as T) : null)

const NO_TIMES: HandoffTimes = {
  set: null, started: null, firstViewed: null, lastViewed: null, views: 0, skips: 0, firstRunStarted: null, firstRunEnded: null,
  runs: 0, firstChecked: null, lastChecked: null, checks: 0, resolved: null, cancelled: null,
}

const toTrace = (r: Row): TraceEntry => ({
  id: r.id as number,
  at: r.at as number,
  itemId: r.item_id as string,
  handoffId: (r.handoff_id as string) ?? null,
  runId: (r.run_id as string) ?? null,
  actor: r.actor as string,
  kind: r.kind as string,
  data: json(r.data),
})

const toHandoff = (r: Row, times: HandoffTimes = NO_TIMES): Handoff => ({
  id: r.id as string,
  itemId: r.item_id as string,
  status: r.status as Handoff['status'],
  type: r.type as Handoff['type'],
  params: json(r.params) ?? {},
  createdBy: r.created_by as string,
  createdAt: r.created_at as number,
  startedAt: (r.started_at as number) ?? null,
  resolvedAt: (r.resolved_at as number) ?? null,
  resolution: json(r.resolution),
  resolvedBy: (r.resolved_by as string) ?? null,
  nextCheckAt: (r.next_check_at as number) ?? null,
  lastError: (r.last_error as string) ?? null,
  times,
})

const toEvent = (r: Row): ItemEvent => ({
  id: r.id as number,
  itemId: r.item_id as string,
  at: r.at as number,
  actor: r.actor as string,
  kind: r.kind as string,
  text: r.text as string,
  data: json(r.data),
})

const toRun = (r: Row): BotRun => ({
  id: r.id as string,
  itemId: r.item_id as string,
  handoffId: r.handoff_id as string,
  worker: r.worker as number,
  startedAt: r.started_at as number,
  endedAt: (r.ended_at as number) ?? null,
  costUsd: (r.cost_usd as number) ?? null,
  turns: (r.turns as number) ?? null,
  result: (r.result as string) ?? null,
})

/** An open item is with the human when its handoff is a human one, or it has none. */
export const withHuman = (i: Item): boolean => !i.closed && (!i.current || i.current.type === 'human')

/** A human set it, or the system did on their behalf; anything else is a bot's. */
const authorOf = (createdBy: string): Author => (createdBy === 'bot' || createdBy === 'script:bot' ? 'bot' : 'human')

/** A line saying how a handoff resolved, for the history and the default handoff. */
export function describeResolution(h: Handoff): string {
  const r = h.resolution ?? {}
  const what = r.detail ?? r.decision ?? r.outcome ?? r.error
  const line = what ? `: ${String(what).trim()}` : ''
  return `The ${h.type} handoff resolved${line}${/[.?!]$/.test(line) ? '' : '.'}`
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
 * The whole system, short of the processes: items, their queues of handoffs,
 * the history, and the loop that checks the waits. Every change goes through
 * here, and emits `change`.
 */
export class Engine extends EventEmitter {
  readonly db: Db
  readonly settings: Settings
  readonly exec: Exec
  readonly fetch: typeof fetch
  readonly now: () => number
  readonly actions: Action[] = [...itemActions, ...githubActions, ...slackActions]
  readonly handoffTypes: HandoffType[] = HANDOFF_TYPES
  readonly sources: Source[]
  /** Stops the run on an item, because the human took it back. Set by the pool. */
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
    // A run the app was closed during is gone; its handoff goes back to a worker.
    this.db.prepare('UPDATE items SET worker = NULL WHERE worker IS NOT NULL').run()
    this.db.prepare('UPDATE runs SET ended_at = ? WHERE ended_at IS NULL').run(this.now())
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

  handoffs(itemId: string): Handoff[] {
    const times = this.times(itemId)
    return (this.db.prepare('SELECT * FROM handoffs WHERE item_id = ? ORDER BY seq').all(itemId) as Row[]).map((r) =>
      toHandoff(r, times.get(r.id as string)),
    )
  }

  /** Each handoff's timeline, read from the trace. */
  private times(itemId: string): Map<string, HandoffTimes> {
    const rows = this.db
      .prepare(
        `SELECT handoff_id, kind, MIN(at) AS first, MAX(at) AS last, COUNT(*) AS n FROM trace
         WHERE item_id = ? AND handoff_id IS NOT NULL GROUP BY handoff_id, kind`,
      )
      .all(itemId) as Row[]
    const out = new Map<string, HandoffTimes>()
    for (const r of rows) {
      const t = out.get(r.handoff_id as string) ?? { ...NO_TIMES }
      const first = r.first as number
      const last = r.last as number
      const n = Number(r.n)
      switch (r.kind) {
        case 'handoff_set': t.set = first; break
        case 'handoff_started': t.started = first; break
        case 'handoff_viewed': Object.assign(t, { firstViewed: first, lastViewed: last, views: n }); break
        case 'handoff_skipped': t.skips = n; break
        case 'run_started': Object.assign(t, { firstRunStarted: first, runs: n }); break
        case 'run_ended': t.firstRunEnded = first; break
        case 'check': Object.assign(t, { firstChecked: first, lastChecked: last, checks: n }); break
        case 'handoff_resolved': t.resolved = first; break
        case 'handoff_cancelled': t.cancelled = first; break
      }
      out.set(r.handoff_id as string, t)
    }
    return out
  }

  /** Everything that happened to an item, in order. */
  trace(itemId: string): TraceEntry[] {
    return (this.db.prepare('SELECT * FROM trace WHERE item_id = ? ORDER BY id').all(itemId) as Row[]).map(toTrace)
  }

  /** Records one thing that happened. Every change, view and check comes through here. */
  private mark(
    itemId: string,
    kind: string,
    opts: { handoffId?: string | null; runId?: string | null; actor?: string; data?: Record<string, unknown> } = {},
  ): void {
    this.db
      .prepare('INSERT INTO trace (at, item_id, handoff_id, run_id, actor, kind, data) VALUES (?, ?, ?, ?, ?, ?, ?)')
      .run(this.now(), itemId, opts.handoffId ?? null, opts.runId ?? null, opts.actor ?? 'system', kind, opts.data ? JSON.stringify(opts.data) : null)
  }

  /** The human has the item on screen. */
  viewed(itemId: string): void {
    const item = this.must(itemId)
    this.mark(itemId, 'handoff_viewed', { handoffId: item.current?.id, actor: 'human' })
  }

  /** The human passed over the item for now. */
  skipped(itemId: string): void {
    const item = this.must(itemId)
    this.mark(itemId, 'handoff_skipped', { handoffId: item.current?.id, actor: 'human' })
  }

  private active(itemId: string): Handoff | null {
    const r = this.db.prepare("SELECT * FROM handoffs WHERE item_id = ? AND status = 'active'").get(itemId) as Row | undefined
    return r ? toHandoff(r) : null
  }

  private toItem(r: Row): Item {
    const queued = this.db.prepare("SELECT COUNT(*) AS n FROM handoffs WHERE item_id = ? AND status = 'queued'").get(r.id) as Row
    const last = this.db.prepare('SELECT MAX(resolved_at) AS at FROM handoffs WHERE item_id = ?').get(r.id) as Row
    const current = this.active(r.id as string)
    return {
      id: r.id as string,
      title: r.title as string,
      source: r.source as string,
      url: (r.url as string) ?? null,
      closed: Boolean(r.closed),
      current,
      queued: Number(queued.n),
      worker: (r.worker as number) ?? null,
      since: current?.startedAt ?? (last.at as number) ?? (r.created_at as number),
      createdAt: r.created_at as number,
      updatedAt: r.updated_at as number,
    }
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
    const items = rows.map((r) => this.toItem(r))
    items.sort((a, b) => a.since - b.since)
    return { items, workers }
  }

  events(id: string): ItemEvent[] {
    return (this.db.prepare('SELECT * FROM events WHERE item_id = ? ORDER BY id').all(id) as Row[]).map(toEvent)
  }

  detail(id: string): ItemDetail | null {
    const item = this.get(id)
    if (!item) return null
    return {
      item,
      handoffs: this.handoffs(id),
      events: this.events(id),
      trace: this.trace(id),
      runs: (this.db.prepare('SELECT * FROM runs WHERE item_id = ? ORDER BY started_at').all(id) as Row[]).map(toRun),
    }
  }

  /**
   * The item that has waited longest for the human, leaving out `skip`. An open
   * item waits for the human when its handoff is a human one, or when it has no
   * handoff at all: nothing else is doing anything with it.
   */
  next(skip: string[] = []): string | null {
    const rows = this.db.prepare('SELECT * FROM items WHERE closed = 0').all() as Row[]
    return (
      rows
        .map((r) => this.toItem(r))
        .filter((i) => withHuman(i) && !skip.includes(i.id))
        .sort((a, b) => a.since - b.since)[0]?.id ?? null
    )
  }

  // ---- history ----

  record(itemId: string, actor: string, kind: string, text: string, data?: Record<string, unknown>): void {
    this.db
      .prepare('INSERT INTO events (item_id, at, actor, kind, text, data) VALUES (?, ?, ?, ?, ?, ?)')
      .run(itemId, this.now(), actor, kind, text, data ? JSON.stringify(data) : null)
    this.db.prepare('UPDATE items SET updated_at = ? WHERE id = ?').run(this.now(), itemId)
  }

  /**
   * The items touched in the last `days`, each with how long it spent on each
   * kind of handoff and what happened in them. The start of any look back.
   */
  recent(days: number) {
    const since = this.now() - days * 24 * 60 * MINUTE
    const rows = this.db.prepare('SELECT * FROM items WHERE updated_at >= ? ORDER BY created_at').all(since) as Row[]
    return rows.map((r) => {
      const item = this.toItem(r)
      const handoffs = this.handoffs(item.id).filter((h) => h.startedAt !== null)
      const minutes: Record<string, number> = {}
      for (const h of handoffs) minutes[h.type] = (minutes[h.type] ?? 0) + ((h.resolvedAt ?? this.now()) - h.startedAt!) / MINUTE
      const events = this.events(item.id)
      const runs = this.db.prepare('SELECT COUNT(*) AS n, SUM(cost_usd) AS cost FROM runs WHERE item_id = ?').get(item.id) as Row
      return {
        id: item.id,
        title: item.title,
        source: item.source,
        closed: item.closed,
        handoffs: handoffs.map((h) => h.type),
        minutesOn: Object.fromEntries(Object.entries(minutes).map(([k, v]) => [k, Math.round(v)])),
        botRuns: Number(runs.n),
        costUsd: (runs.cost as number) ?? 0,
        humanDecisions: handoffs.filter((h) => h.type === 'human' && h.resolvedBy === 'human').length,
        approvalsChanged: handoffs.filter((h) => ((h.resolution?.action as { changed?: string[] })?.changed ?? []).length).length,
        timedOut: handoffs.filter((h) => h.resolution?.outcome === 'timed_out').length,
        botFailures: handoffs.filter((h) => h.type === 'bot' && h.resolution?.error).length,
        // How long the human took: from a question appearing to first seeing it, and from seeing it to answering.
        humanMinutesToView: handoffs
          .filter((h) => h.type === 'human' && h.times.started && h.times.firstViewed)
          .map((h) => Math.round((h.times.firstViewed! - h.times.started!) / MINUTE)),
        humanMinutesToAnswer: handoffs
          .filter((h) => h.type === 'human' && h.times.firstViewed && h.times.resolved)
          .map((h) => Math.round((h.times.resolved! - h.times.firstViewed!) / MINUTE)),
        // How long bot handoffs waited for a free worker.
        botMinutesQueued: handoffs
          .filter((h) => h.type === 'bot' && h.times.started && h.times.firstRunStarted)
          .map((h) => Math.round((h.times.firstRunStarted! - h.times.started!) / MINUTE)),
        handledOutside: events.filter((e) => e.kind === 'handled_outside').map((e) => e.text),
      }
    })
  }

  transcript(runId: string): string | null {
    const r = this.db.prepare('SELECT prompt, transcript FROM runs WHERE id = ?').get(runId) as Row | undefined
    return r ? `PROMPT:\n${r.prompt}\n\nTRANSCRIPT (stream-json):\n${r.transcript}` : null
  }

  // ---- actions ----

  /** The one way into an action, from MCP, a script or the UI: checked, then run. */
  async callAction(name: string, args: Record<string, unknown>, ctx: ActionContext): Promise<unknown> {
    const action = this.actions.find((a) => a.name === name)
    if (!action) throw new Error(`No action ${name}.`)
    // The rule on unsafe actions (see Action in integrations.ts), checked here
    // too, whatever the caller was shown.
    if (action.unsafe && ctx.author !== 'human')
      throw new Error(
        `${name} is unsafe, so bots, and scripts bots wrote, may not run it. Ask a human: a human handoff with approval { action: "${name}", inputs }.`,
      )
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
    if (action.unsafe && ctx.itemId && !ctx.approving)
      this.record(ctx.itemId, ctx.actor, 'action_run', name, { action: name, inputs: parsed.data, output: out as Record<string, unknown> })
    return out
  }

  // ---- handoffs ----

  /** Checks handoffs and expands them as their type says, without touching the store. */
  private prepare(specs: HandoffSpec[], author: Author): HandoffSpec[] {
    return specs.flatMap((s) => {
      const type = this.handoffTypes.find((t) => t.type === s?.type)
      if (!type) throw new Error(`No handoff type ${s?.type}: one of ${this.handoffTypes.map((t) => t.type).join(', ')}.`)
      const parsed = z.object(type.params).safeParse(s.params ?? {})
      if (!parsed.success)
        throw new Error(`Bad params for a ${s.type} handoff: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
      const params = parsed.data as Record<string, unknown>
      if (s.type === 'human' && params.approval) {
        const { action } = params.approval as { action: string }
        const known = this.actions.find((a) => a.name === action)
        if (!known?.unsafe) throw new Error(`${action} is not an unsafe action, so it needs no approval: a bot runs it itself.`)
      }
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e) => ({ type: e.type, params: e.params }))
    })
  }

  /** Puts handoffs at the front of an item's queue, behind whatever is active. */
  private enqueue(itemId: string, specs: HandoffSpec[], createdBy: string): void {
    if (!specs.length) return
    const bounds = this.db
      .prepare(
        `SELECT MAX(CASE WHEN status != 'queued' THEN seq END) AS lo, MIN(CASE WHEN status = 'queued' THEN seq END) AS hi
         FROM handoffs WHERE item_id = ?`,
      )
      .get(itemId) as Row
    const lo = (bounds.lo as number) ?? 0
    const hi = (bounds.hi as number) ?? lo + specs.length + 1
    const step = (hi - lo) / (specs.length + 1)
    specs.forEach((s, i) => {
      const id = randomUUID()
      this.db
        .prepare('INSERT INTO handoffs (id, item_id, seq, status, type, params, created_by, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)')
        .run(id, itemId, lo + step * (i + 1), 'queued', s.type, JSON.stringify(s.params), createdBy, this.now())
      this.mark(itemId, 'handoff_set', { handoffId: id, actor: createdBy, data: { type: s.type } })
    })
  }

  private resolve(handoffId: string, resolution: Record<string, unknown>, by: string): void {
    this.db
      .prepare("UPDATE handoffs SET status = 'resolved', resolution = ?, resolved_by = ?, resolved_at = ? WHERE id = ?")
      .run(JSON.stringify(resolution), by, this.now(), handoffId)
    const { item_id } = this.db.prepare('SELECT item_id FROM handoffs WHERE id = ?').get(handoffId) as Row
    this.mark(item_id as string, 'handoff_resolved', { handoffId, actor: by })
  }

  private cancelQueue(itemId: string, actor: string): void {
    const queued = this.db.prepare("SELECT id FROM handoffs WHERE item_id = ? AND status = 'queued'").all(itemId) as Row[]
    this.db.prepare("UPDATE handoffs SET status = 'cancelled' WHERE item_id = ? AND status = 'queued'").run(itemId)
    for (const { id } of queued) this.mark(itemId, 'handoff_cancelled', { handoffId: id as string, actor })
  }

  /**
   * Starts handoffs until one has to wait: the next queued one, or a human one
   * saying what just resolved when there is none. Scripts resolve as they start,
   * so a chain of them runs through here in one go.
   */
  private advance(itemId: string): Promise<void> {
    return this.serial(itemId, async () => {
      for (let guard = 0; guard < 50; guard++) {
        const item = this.must(itemId)
        if (item.closed || item.current) break
        let row = this.db
          .prepare("SELECT * FROM handoffs WHERE item_id = ? AND status = 'queued' ORDER BY seq LIMIT 1")
          .get(itemId) as Row | undefined
        if (!row) break // nothing queued: the item is with the human
        const handoff = toHandoff(row)
        const type = this.handoffTypes.find((t) => t.type === handoff.type)!
        this.db
          .prepare("UPDATE handoffs SET status = 'active', started_at = ?, next_check_at = ? WHERE id = ?")
          .run(this.now(), type.check ? this.now() : null, handoff.id)
        this.mark(itemId, 'handoff_started', { handoffId: handoff.id })
        const previous = this.handoffs(itemId).filter((h) => h.status === 'resolved').at(-1) ?? null
        const ctx = { engine: this, item, handoff, previous, author: authorOf(handoff.createdBy) }
        try {
          if (type.start) {
            const extra = await type.start(handoff.params, ctx)
            if (extra) this.db.prepare('UPDATE handoffs SET params = ? WHERE id = ?').run(JSON.stringify({ ...handoff.params, ...extra }), handoff.id)
          }
          if (type.run) {
            const { resolution, next } = await type.run(handoff.params, ctx)
            const prepared = this.prepare(next, ctx.author)
            transaction(this.db, () => {
              this.resolve(handoff.id, resolution, 'system')
              this.enqueue(itemId, prepared, handoff.createdBy === 'bot' ? 'script:bot' : 'script')
            })
          }
        } catch (e) {
          const error = (e as Error).message
          transaction(this.db, () => {
            this.resolve(handoff.id, { error }, 'system')
            this.enqueue(itemId, [{ type: 'human', params: { needed: `The ${handoff.type} handoff failed: ${error} What next?` } }], 'system')
          })
        }
      }
      this.emit('change')
    })
  }

  // ---- changes ----

  async create(
    title: string,
    opts: { source?: string; ref?: string; url?: string | null; next?: HandoffSpec[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const id = randomUUID()
    const at = this.now()
    const actor = opts.actor ?? 'human'
    // With no handoffs, the item is with the human.
    const next = this.prepare(opts.next ?? [], opts.author ?? 'human')
    const created = transaction(this.db, () => {
      const r = this.db
        .prepare(
          `INSERT OR IGNORE INTO items (id, title, source, source_ref, url, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?)`,
        )
        .run(id, title, opts.source ?? 'manual', opts.ref ?? null, opts.url ?? null, at, at) as { changes: number | bigint }
      if (Number(r.changes) === 0) return false
      this.record(id, actor, 'created', title, { source: opts.source ?? 'manual', url: opts.url ?? null })
      this.mark(id, 'item_created', { actor, data: { title, source: opts.source ?? 'manual', ref: opts.ref ?? null } })
      this.enqueue(id, next, opts.author === 'bot' ? 'bot' : actor)
      return true
    })
    if (!created) return null
    await this.advance(id)
    return id
  }

  /** A human resolves the current handoff, whatever it is, and puts `next` in front of the queue. */
  async handOff(
    itemId: string,
    opts: { next: HandoffSpec[]; replaceQueue?: boolean; actor?: string },
  ): Promise<void> {
    const actor = opts.actor ?? 'human'
    const next = this.prepare(opts.next, 'human')
    await this.serial(itemId, async () => {
      const item = this.must(itemId)
      if (item.closed) throw new Error('This item is closed.')
      transaction(this.db, () => {
        if (item.current)
          this.resolve(item.current.id, item.current.type === 'human' ? { decision: 'handed_off' } : { outcome: 'taken_over' }, 'human')
        if (opts.replaceQueue) this.cancelQueue(itemId, actor)
        this.enqueue(itemId, next, actor)
      })
      if (item.current?.type === 'bot' && item.worker !== null) this.onCancel(itemId)
    })
    await this.advance(itemId)
  }

  /** Runs the action the current human handoff asks approval for, with the human's inputs. */
  async approve(itemId: string, inputs: Record<string, unknown>): Promise<void> {
    const item = this.must(itemId)
    const approval = item.current?.type === 'human' ? (item.current.params.approval as { action: string; inputs: Record<string, unknown> } | undefined) : undefined
    if (!item.current || !approval) throw new Error('This item has no action waiting for approval.')
    const changed = Object.keys({ ...approval.inputs, ...inputs }).filter(
      (k) => String(inputs[k] ?? '') !== String(approval.inputs[k] ?? ''),
    )
    let output: unknown
    try {
      output = await this.callAction(approval.action, inputs, { engine: this, author: 'human', actor: 'human', itemId, runId: null, approving: true })
    } catch (e) {
      this.record(itemId, 'human', 'action_failed', (e as Error).message, { action: approval.action, inputs })
      this.emit('change')
      throw e
    }
    this.resolve(item.current.id, { decision: 'approved', action: { name: approval.action, proposed: approval.inputs, inputs, changed, output } }, 'human')
    await this.advance(itemId)
  }

  async close(itemId: string, opts: { actor?: string; kind?: 'closed' | 'handled_outside' } = {}): Promise<void> {
    const actor = opts.actor ?? 'human'
    const kind = opts.kind ?? 'closed'
    await this.serial(itemId, async () => {
      const item = this.must(itemId)
      transaction(this.db, () => {
        if (kind === 'handled_outside') this.record(itemId, actor, 'handled_outside', 'Handled outside the tool.')
        if (item.current)
          this.resolve(item.current.id, item.current.type === 'human' ? { decision: kind } : { outcome: 'taken_over', decision: kind }, actor)
        this.cancelQueue(itemId, actor)
        this.db.prepare('UPDATE items SET closed = 1, updated_at = ? WHERE id = ?').run(this.now(), itemId)
        this.mark(itemId, 'item_closed', { actor, data: { kind } })
      })
      if (item.current?.type === 'bot' && item.worker !== null) this.onCancel(itemId)
    })
    // Items waiting on this one are checked at once rather than at their next turn.
    this.db.prepare("UPDATE handoffs SET next_check_at = ? WHERE status = 'active' AND type = 'items'").run(this.now())
    this.emit('change')
    void this.checkDue()
  }

  handledOutside(itemId: string): Promise<void> {
    return this.close(itemId, { kind: 'handled_outside' })
  }

  /** Brings an item back to the human, stopping any run on it. */
  async takeOver(id: string): Promise<void> {
    // The current handoff ends and nothing starts: the item is with the human until they send it on
    // (or Continue with what was queued).
    await this.serial(id, async () => {
      const item = this.must(id)
      if (!item.current || item.closed) return
      this.resolve(item.current.id, { outcome: 'taken_over' }, 'human')
      if (item.current.type === 'bot' && item.worker !== null) this.onCancel(id)
    })
    this.emit('change')
  }

  // ---- bots ----

  /** Gives `worker` the bot handoff that has waited longest, or null. */
  claim(worker: number): { item: Item; handoff: Handoff } | null {
    const r = this.db
      .prepare(
        `SELECT h.item_id FROM handoffs h JOIN items i ON i.id = h.item_id
         WHERE h.status = 'active' AND h.type = 'bot' AND i.worker IS NULL ORDER BY h.started_at LIMIT 1`,
      )
      .get() as Row | undefined
    if (!r) return null
    this.db.prepare('UPDATE items SET worker = ? WHERE id = ?').run(worker, r.item_id)
    this.emit('change')
    const item = this.must(r.item_id as string)
    return { item, handoff: item.current! }
  }

  startRun(item: Item, handoff: Handoff, worker: number, prompt: string): string {
    const id = randomUUID()
    this.db
      .prepare('INSERT INTO runs (id, item_id, handoff_id, worker, started_at, prompt) VALUES (?, ?, ?, ?, ?, ?)')
      .run(id, item.id, handoff.id, worker, this.now(), prompt)
    this.mark(item.id, 'run_started', { handoffId: handoff.id, runId: id, actor: 'bot', data: { worker } })
    return id
  }

  appendTranscript(runId: string, chunk: string): void {
    this.db.prepare('UPDATE runs SET transcript = transcript || ? WHERE id = ?').run(chunk, runId)
  }

  /** The item and handoff of a run still going, or null. */
  activeRun(runId: string): { itemId: string; handoffId: string } | null {
    const r = this.db.prepare('SELECT item_id, handoff_id FROM runs WHERE id = ? AND ended_at IS NULL').get(runId) as Row | undefined
    return r ? { itemId: r.item_id as string, handoffId: r.handoff_id as string } : null
  }

  /**
   * A run is over. If its handoff is still the item's, the run's output resolves
   * it: the handoffs it returned queued in front, and what
   * it said on the way kept in the resolution. Anything wrong goes to the human.
   */
  async endRun(
    runId: string,
    end: { exitCode: number | null; structured?: unknown; output?: string; costUsd?: number; turns?: number; error?: string },
  ): Promise<void> {
    const run = this.db.prepare('SELECT * FROM runs WHERE id = ?').get(runId) as Row | undefined
    if (!run) return
    const itemId = run.item_id as string
    this.db
      .prepare('UPDATE runs SET ended_at = ?, cost_usd = ?, turns = ?, result = ? WHERE id = ?')
      .run(this.now(), end.costUsd ?? null, end.turns ?? null, end.output ?? end.error ?? null, runId)
    this.mark(itemId, 'run_ended', {
      handoffId: run.handoff_id as string,
      runId,
      actor: 'bot',
      data: { exitCode: end.exitCode, costUsd: end.costUsd ?? null, turns: end.turns ?? null, error: end.error ?? null },
    })
    this.db.prepare('UPDATE items SET worker = NULL WHERE id = ? AND worker = ?').run(itemId, run.worker)
    const item = this.must(itemId)
    if (item.current?.id !== run.handoff_id) {
      this.emit('change')
      return // the human took it back while it ran
    }

    let out: BotOutput | null = null
    let error = end.error ?? null
    let next: HandoffSpec[] = []
    if (!error) {
      try {
        out = z
          .object({ next: z.array(z.object({ type: z.string(), params: z.record(z.unknown()) })), close: z.boolean().optional() })
          .parse(end.structured) as BotOutput
        if (!out.close && !out.next.length) throw new Error('It returned no handoffs and did not close the item.')
        next = this.prepare(out.next, 'bot')
      } catch (e) {
        error = `The bot's output was not usable: ${(e as Error).message}`
      }
    }
    const base = { output: end.output ?? '', runId, ...(end.costUsd !== undefined ? { costUsd: end.costUsd } : {}) }
    await this.serial(itemId, async () => {
      transaction(this.db, () => {
        if (error || !out) {
          this.resolve(item.current!.id, { ...base, error }, 'bot')
          this.enqueue(itemId, [{ type: 'human', params: { needed: `${error} What next?` } }], 'system')
          return
        }
        this.resolve(item.current!.id, { ...base, ...(out.close ? { closed: true } : {}) }, 'bot')
        if (out.close) {
          this.cancelQueue(itemId, 'bot')
          this.db.prepare('UPDATE items SET closed = 1, updated_at = ? WHERE id = ?').run(this.now(), itemId)
          this.mark(itemId, 'item_closed', { actor: 'bot', runId, data: { kind: 'closed' } })
        } else this.enqueue(itemId, next, 'bot')
      })
    })
    if (out?.close) {
      this.db.prepare("UPDATE handoffs SET next_check_at = ? WHERE status = 'active' AND type = 'items'").run(this.now())
      void this.checkDue()
    }
    await this.advance(itemId)
  }

  // ---- the loop ----

  /** One turn of the central loop: every wait that is due, then every source that is. */
  async tick(): Promise<void> {
    await this.checkDue()
    await this.pollSources(false)
  }

  async checkDue(): Promise<void> {
    const due = this.db
      .prepare("SELECT item_id FROM handoffs WHERE status = 'active' AND next_check_at IS NOT NULL AND next_check_at <= ?")
      .all(this.now()) as Row[]
    await Promise.all(due.map(({ item_id }) => this.checkOne(item_id as string)))
  }

  private async checkOne(itemId: string): Promise<void> {
    let resolved = false
    await this.serial(itemId, async () => {
      const item = this.must(itemId)
      const h = item.current
      if (!h || h.nextCheckAt === null || h.nextCheckAt > this.now()) return
      const type = this.handoffTypes.find((t) => t.type === h.type)!
      const previous = this.handoffs(itemId).filter((x) => x.status === 'resolved').at(-1) ?? null
      let resolution: Record<string, unknown> | null = null
      let error: string | null = null
      try {
        resolution = await type.check!(h.params, { engine: this, item, handoff: h, previous, author: authorOf(h.createdBy) })
      } catch (e) {
        error = (e as Error).message
      }
      this.mark(itemId, 'check', {
        handoffId: h.id,
        data: resolution ? { result: 'resolved', outcome: resolution.outcome ?? null } : error ? { result: 'error', error } : { result: 'pending' },
      })
      const timeout = Number(h.params.timeout_minutes ?? 0)
      if (!resolution && timeout && this.now() >= h.startedAt! + timeout * MINUTE)
        resolution = { outcome: 'timed_out', detail: `Nothing after ${timeout} minutes.` }
      if (resolution) {
        this.resolve(h.id, resolution, 'system')
        resolved = true
        return
      }
      const step = Math.min(this.stepOf(h.id) + 1, BACKOFF_MINUTES.length - 1)
      this.db
        .prepare('UPDATE handoffs SET check_step = ?, next_check_at = ?, last_error = ? WHERE id = ?')
        .run(step, this.now() + BACKOFF_MINUTES[step] * MINUTE, error, h.id)
      if (error && error !== h.lastError) this.record(itemId, 'system', 'check_failed', error)
    })
    if (resolved) await this.advance(itemId)
    this.emit('change')
  }

  private stepOf(handoffId: string): number {
    // The first check comes at once; the backoff starts from there.
    const r = this.db.prepare('SELECT check_step, next_check_at, started_at FROM handoffs WHERE id = ?').get(handoffId) as Row
    return r.next_check_at === r.started_at ? -1 : (r.check_step as number)
  }

  async pollSources(force: boolean): Promise<void> {
    for (const source of this.sources) {
      const last = this.lastPoll.get(source.id) ?? 0
      if (!force && this.now() - last < source.everyMinutes * MINUTE) continue
      this.lastPoll.set(source.id, this.now())
      try {
        for (const found of await source.poll(this))
          await this.create(found.title, { source: source.id, ref: found.ref, url: found.url, next: found.next, actor: source.id })
      } catch (e) {
        this.emit('sourceError', `${source.id}: ${(e as Error).message}`)
      }
    }
  }
}
