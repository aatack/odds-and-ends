import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { z } from 'zod'
import type { AgentRun, Blocker, Item, ItemDetail, ItemEvent, Overview, Request, Wait } from '../shared/types'
import { migrate, transaction, type Db } from './db'
import { githubActions, githubReads, githubSources, githubWaits } from './github'
import { execCli, type Action, type Exec, type ReadTool, type Source, type WaitKind } from './integrations'

/** How long a wait sleeps between checks: a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000

const timeWait: WaitKind = {
  id: 'time',
  description: 'A moment in time to pass.',
  input: { until: z.string().describe('an ISO 8601 date and time') },
  describe: (p) => `until ${p.until}`,
  check: async (p) => (Date.now() >= Date.parse(String(p.until)) ? `It is past ${p.until}.` : null),
}

export interface Registry {
  reads: ReadTool[]
  actions: Action[]
  waits: WaitKind[]
  sources: Source[]
}

export const defaultRegistry: Registry = {
  reads: githubReads,
  actions: githubActions,
  waits: [timeWait, ...githubWaits],
  sources: githubSources,
}

/** How an agent's run ended, as recorded by the tool that ended it. */
export type AgentOutcome =
  | { kind: 'ask'; state: string; question: string }
  | { kind: 'approval'; state: string; action: string; inputs: Record<string, string>; reason: string }
  | { kind: 'wait'; state: string; wait: string; params: Record<string, unknown>; timeoutMinutes?: number }
  | { kind: 'items'; state: string; itemIds: string[] }
  | { kind: 'close'; state: string }

type Row = Record<string, unknown>

const json = <T>(s: unknown): T | null => (typeof s === 'string' && s ? (JSON.parse(s) as T) : null)

const toItem = (r: Row): Item => ({
  id: r.id as string,
  title: r.title as string,
  source: r.source as string,
  url: (r.url as string) ?? null,
  state: r.state as string,
  blocker: r.blocker as Blocker,
  request: json<Request>(r.request),
  instruction: (r.instruction as string) ?? null,
  worker: (r.worker as number) ?? null,
  createdAt: r.created_at as number,
  updatedAt: r.updated_at as number,
  blockedAt: r.blocked_at as number,
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

const toRun = (r: Row): AgentRun => ({
  id: r.id as string,
  itemId: r.item_id as string,
  worker: r.worker as number,
  startedAt: r.started_at as number,
  endedAt: (r.ended_at as number) ?? null,
  outcome: (r.outcome as string) ?? null,
  costUsd: (r.cost_usd as number) ?? null,
  turns: (r.turns as number) ?? null,
  result: (r.result as string) ?? null,
})

/**
 * The whole system, short of the processes: items and their blockers, the
 * history, the waits and the loop that checks them. Every change goes through
 * here, records an event, and emits `change`.
 */
export class Engine extends EventEmitter {
  /** The runs the pool is to stop, because the operator took their item back. */
  onCancel: (itemId: string) => void = () => {}
  private lastPoll = new Map<string, number>()

  constructor(
    readonly db: Db,
    readonly registry: Registry = defaultRegistry,
    readonly exec: Exec = execCli,
    readonly now: () => number = Date.now,
  ) {
    super()
    migrate(db)
    // A run the app was closed during is gone; its item goes back in the queue.
    db.prepare('UPDATE items SET worker = NULL WHERE worker IS NOT NULL').run()
    db.prepare("UPDATE runs SET ended_at = ?, outcome = 'interrupted' WHERE ended_at IS NULL").run(this.now())
  }

  // ---- reads ----

  get(id: string): Item | null {
    const r = this.db.prepare('SELECT * FROM items WHERE id = ?').get(id) as Row | undefined
    return r ? toItem(r) : null
  }

  private must(id: string): Item {
    const item = this.get(id)
    if (!item) throw new Error(`No item ${id}.`)
    return item
  }

  overview(workers: Overview['workers'] = []): Overview {
    const rows = this.db
      .prepare(
        `SELECT * FROM items WHERE blocker != 'closed' OR updated_at > ? ORDER BY blocked_at`,
      )
      .all(this.now() - 24 * 60 * MINUTE) as Row[]
    return { items: rows.map(toItem), workers }
  }

  wait(id: string): Wait | null {
    const r = this.db.prepare('SELECT * FROM waits WHERE item_id = ?').get(id) as Row | undefined
    if (!r) return null
    return {
      kind: r.kind as string,
      description: r.description as string,
      params: json(r.params) ?? {},
      step: r.step as number,
      nextCheckAt: r.next_check_at as number,
      deadline: (r.deadline as number) ?? null,
      lastError: (r.last_error as string) ?? null,
    }
  }

  detail(id: string): ItemDetail | null {
    const item = this.get(id)
    if (!item) return null
    const dependsOn = (
      this.db
        .prepare('SELECT i.id, i.title, i.blocker FROM deps d JOIN items i ON i.id = d.depends_on WHERE d.item_id = ?')
        .all(id) as Row[]
    ).map((r) => ({ id: r.id as string, title: r.title as string, blocker: r.blocker as Blocker }))
    return {
      item,
      wait: this.wait(id),
      dependsOn,
      events: this.events(id),
      runs: (this.db.prepare('SELECT * FROM runs WHERE item_id = ? ORDER BY started_at').all(id) as Row[]).map(toRun),
    }
  }

  events(id: string): ItemEvent[] {
    return (this.db.prepare('SELECT * FROM events WHERE item_id = ? ORDER BY id').all(id) as Row[]).map(toEvent)
  }

  /** The operator-blocked item that has waited longest, leaving out `skip`. */
  next(skip: string[] = []): string | null {
    const rows = this.db
      .prepare("SELECT id FROM items WHERE blocker = 'operator' ORDER BY blocked_at, created_at")
      .all() as Row[]
    return (rows.map((r) => r.id as string).find((id) => !skip.includes(id))) ?? null
  }

  // ---- history ----

  record(itemId: string, actor: string, kind: string, text: string, data?: Record<string, unknown>): void {
    this.db
      .prepare('INSERT INTO events (item_id, at, actor, kind, text, data) VALUES (?, ?, ?, ?, ?, ?)')
      .run(itemId, this.now(), actor, kind, text, data ? JSON.stringify(data) : null)
  }

  /**
   * The items touched in the last `days`, each with how long it spent on each
   * blocker, how many agent runs it took and what they cost, and what the
   * operator did outside the tool. The start of any look back at the work.
   */
  recent(days: number) {
    const since = this.now() - days * 24 * 60 * MINUTE
    const items = (this.db.prepare('SELECT * FROM items WHERE updated_at >= ? ORDER BY created_at').all(since) as Row[]).map(toItem)
    return items.map((item) => {
      const events = this.events(item.id)
      const time: Record<string, number> = {}
      let at = item.createdAt
      let on = events.find((e) => e.kind === 'created')?.data?.blocker as string | undefined
      for (const e of events.filter((e) => e.kind === 'blocker')) {
        if (on) time[on] = (time[on] ?? 0) + (e.at - at)
        at = e.at
        on = e.data?.to as string
      }
      if (on && on !== 'closed') time[on] = (time[on] ?? 0) + (this.now() - at)
      const runs = this.db
        .prepare('SELECT COUNT(*) AS n, SUM(cost_usd) AS cost FROM runs WHERE item_id = ?')
        .get(item.id) as Row
      return {
        id: item.id,
        title: item.title,
        source: item.source,
        blocker: item.blocker,
        state: item.state,
        minutesOn: Object.fromEntries(Object.entries(time).map(([k, ms]) => [k, Math.round(ms / MINUTE)])),
        agentRuns: runs.n as number,
        costUsd: (runs.cost as number) ?? 0,
        operatorDecisions: events.filter((e) => e.actor === 'operator').length,
        handledOutside: events.filter((e) => e.kind === 'handled_outside').map((e) => e.text),
        approvalsChanged: events.filter((e) => e.kind === 'action_run' && (e.data?.changed as string[] | undefined)?.length).length,
      }
    })
  }

  transcript(runId: string): string | null {
    const r = this.db.prepare('SELECT prompt, transcript FROM runs WHERE id = ?').get(runId) as Row | undefined
    return r ? `PROMPT:\n${r.prompt}\n\nTRANSCRIPT (stream-json):\n${r.transcript}` : null
  }

  // ---- changes ----

  private changed(): void {
    this.emit('change')
  }

  /** The one way an item's blocker changes, so the history has every move. */
  private setBlocker(id: string, to: Blocker, actor: string, reason: string, request: Request | null = null): void {
    const item = this.must(id)
    const at = this.now()
    this.db
      .prepare('UPDATE items SET blocker = ?, request = ?, blocked_at = ?, updated_at = ? WHERE id = ?')
      .run(to, request ? JSON.stringify(request) : null, at, at, id)
    if (to !== 'external') this.db.prepare('DELETE FROM waits WHERE item_id = ?').run(id)
    if (to !== 'items') this.db.prepare('DELETE FROM deps WHERE item_id = ?').run(id)
    this.record(id, actor, 'blocker', reason, { from: item.blocker, to, minutesOnPrevious: Math.round((at - item.blockedAt) / MINUTE) })
    if (to === 'closed') this.checkDeps()
  }

  private setStateText(id: string, state: string, actor: string): void {
    this.db.prepare('UPDATE items SET state = ?, updated_at = ? WHERE id = ?').run(state, this.now(), id)
    this.record(id, actor, 'state', state)
  }

  create(
    title: string,
    opts: { source?: string; ref?: string; url?: string | null; state?: string; instruction?: string; actor?: string } = {},
  ): string | null {
    const id = randomUUID()
    const at = this.now()
    const actor = opts.actor ?? 'operator'
    const blocker: Blocker = opts.instruction ? 'agent' : 'operator'
    const request: Request | null = opts.instruction ? null : { question: 'New item. What should happen with it?' }
    const created = transaction(this.db, () => {
      const r = this.db
        .prepare(
          `INSERT OR IGNORE INTO items (id, title, source, source_ref, url, state, blocker, request, instruction, created_at, updated_at, blocked_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        )
        .run(id, title, opts.source ?? 'manual', opts.ref ?? null, opts.url ?? null, opts.state ?? '', blocker,
          request ? JSON.stringify(request) : null, opts.instruction ?? null, at, at, at) as { changes: number | bigint }
      if (Number(r.changes) === 0) return false
      this.record(id, actor, 'created', title, { source: opts.source ?? 'manual', blocker, url: opts.url ?? null })
      if (opts.instruction) this.record(id, actor, 'instruction', opts.instruction)
      return true
    })
    if (!created) return null
    this.changed()
    return id
  }

  dispatch(id: string, instruction: string, actor = 'operator'): void {
    transaction(this.db, () => {
      this.db.prepare('UPDATE items SET instruction = ? WHERE id = ?').run(instruction, id)
      this.record(id, actor, 'instruction', instruction)
      this.setBlocker(id, 'agent', actor, 'Dispatched to an agent.')
    })
    this.changed()
  }

  /** Runs the action the agent asked for, with the operator's inputs, and hands the item back. */
  async execute(id: string, inputs: Record<string, string>): Promise<void> {
    const item = this.must(id)
    const approval = item.request?.approval
    if (item.blocker !== 'operator' || !approval) throw new Error('This item has no action waiting for approval.')
    const action = this.registry.actions.find((a) => a.id === approval.action)
    if (!action) throw new Error(`No action ${approval.action}.`)
    const changed = action.fields.map((f) => f.name).filter((n) => (inputs[n] ?? '') !== (approval.inputs[n] ?? ''))
    let output: string
    try {
      output = await action.run(inputs, this.exec)
    } catch (e) {
      this.record(id, 'operator', 'action_failed', (e as Error).message, { action: action.id, inputs })
      this.changed()
      throw e
    }
    transaction(this.db, () => {
      this.record(id, 'operator', 'action_run', `${action.label}: ${output.trim() || 'done'}`, {
        action: action.id,
        proposed: approval.inputs,
        executed: inputs,
        changed,
      })
      this.setBlocker(id, 'agent', 'operator', `Ran ${action.label}; back to an agent.`)
    })
    this.changed()
  }

  close(id: string, note: string, actor = 'operator'): void {
    transaction(this.db, () => {
      if (note) this.record(id, actor, 'note', note)
      this.setBlocker(id, 'closed', actor, 'Closed.')
    })
    this.changed()
  }

  handledOutside(id: string, note: string): void {
    transaction(this.db, () => {
      this.record(id, 'operator', 'handled_outside', note || '(no note)')
      this.setBlocker(id, 'closed', 'operator', 'Closed: handled outside the tool.')
    })
    this.changed()
  }

  setState(id: string, state: string, actor = 'operator'): void {
    this.setStateText(id, state, actor)
    this.changed()
  }

  note(id: string, text: string, actor: string): void {
    this.record(id, actor, 'note', text)
    this.changed()
  }

  /** Brings an item back to the operator, stopping any run on it. */
  takeOver(id: string): void {
    const item = this.must(id)
    if (item.blocker === 'operator') return
    this.setBlocker(id, 'operator', 'operator', 'Taken over by the operator.', { question: 'You took this item over.' })
    if (item.worker !== null) this.onCancel(id)
    this.changed()
  }

  // ---- agents ----

  /** Gives `worker` the agent-blocked item that has waited longest, or null. */
  claim(worker: number): Item | null {
    const r = this.db
      .prepare("SELECT * FROM items WHERE blocker = 'agent' AND worker IS NULL ORDER BY blocked_at LIMIT 1")
      .get() as Row | undefined
    if (!r) return null
    this.db.prepare('UPDATE items SET worker = ? WHERE id = ?').run(worker, r.id)
    this.changed()
    return toItem({ ...r, worker })
  }

  startRun(itemId: string, worker: number, prompt: string): string {
    const id = randomUUID()
    this.db
      .prepare('INSERT INTO runs (id, item_id, worker, started_at, prompt) VALUES (?, ?, ?, ?, ?)')
      .run(id, itemId, worker, this.now(), prompt)
    this.record(itemId, 'system', 'run_started', `Worker ${worker} started.`, { runId: id })
    return id
  }

  appendTranscript(runId: string, chunk: string): void {
    this.db.prepare('UPDATE runs SET transcript = transcript || ? WHERE id = ?').run(chunk, runId)
  }

  activeRun(runId: string): { itemId: string; worker: number; outcome: string | null } | null {
    const r = this.db
      .prepare('SELECT item_id, worker, outcome FROM runs WHERE id = ? AND ended_at IS NULL')
      .get(runId) as Row | undefined
    return r ? { itemId: r.item_id as string, worker: r.worker as number, outcome: (r.outcome as string) ?? null } : null
  }

  /** The tool an agent ends its run with: what blocks the item now, and its state. */
  async agentBlock(runId: string, outcome: AgentOutcome): Promise<string> {
    const run = this.activeRun(runId)
    if (!run) throw new Error('This run has ended.')
    if (run.outcome) throw new Error(`This run already recorded its outcome (${run.outcome}). Stop now.`)
    const item = this.must(run.itemId)
    if (item.blocker !== 'agent') throw new Error('The operator took this item back. Stop now.')
    const actor = 'agent'

    // Anything that has to reach outside is done before the transaction.
    let wait: { kind: WaitKind; params: Record<string, unknown> } | null = null
    if (outcome.kind === 'wait') {
      const kind = this.registry.waits.find((w) => w.id === outcome.wait)
      if (!kind) throw new Error(`No wait kind ${outcome.wait}. Known: ${this.registry.waits.map((w) => w.id).join(', ')}.`)
      const parsed = z.object(kind.input).safeParse(outcome.params)
      if (!parsed.success) throw new Error(`Bad params for ${kind.id}: ${parsed.error.message}`)
      wait = { kind, params: kind.start ? await kind.start(parsed.data, this.exec) : parsed.data }
    }
    let approval: Request['approval']
    if (outcome.kind === 'approval') {
      const action = this.registry.actions.find((a) => a.id === outcome.action)
      if (!action) throw new Error(`No action ${outcome.action}. Known: ${this.registry.actions.map((a) => a.id).join(', ')}.`)
      approval = { action: action.id, label: action.label, fields: action.fields, inputs: outcome.inputs }
    }
    if (outcome.kind === 'items') {
      const missing = outcome.itemIds.filter((i) => !this.get(i))
      if (missing.length) throw new Error(`No items ${missing.join(', ')}.`)
    }

    transaction(this.db, () => {
      this.setStateText(item.id, outcome.state, actor)
      this.db.prepare('UPDATE runs SET outcome = ? WHERE id = ?').run(outcome.kind, runId)
      switch (outcome.kind) {
        case 'ask':
          this.setBlocker(item.id, 'operator', actor, outcome.question, { question: outcome.question })
          break
        case 'approval':
          this.setBlocker(item.id, 'operator', actor, `Asks to run ${approval!.label}: ${outcome.reason}`, {
            question: outcome.reason,
            approval,
          })
          break
        case 'wait': {
          const description = wait!.kind.describe(wait!.params)
          this.setBlocker(item.id, 'external', actor, `Waiting for ${description}.`)
          this.db
            .prepare(
              'INSERT OR REPLACE INTO waits (item_id, kind, description, params, step, next_check_at, deadline) VALUES (?, ?, ?, ?, 0, ?, ?)',
            )
            .run(item.id, wait!.kind.id, description, JSON.stringify(wait!.params), this.now() + BACKOFF_MINUTES[0] * MINUTE,
              outcome.timeoutMinutes ? this.now() + outcome.timeoutMinutes * MINUTE : null)
          break
        }
        case 'items':
          this.setBlocker(item.id, 'items', actor, `Waiting for ${outcome.itemIds.length} other item(s).`)
          for (const dep of outcome.itemIds)
            this.db.prepare('INSERT OR IGNORE INTO deps (item_id, depends_on) VALUES (?, ?)').run(item.id, dep)
          break
        case 'close':
          this.setBlocker(item.id, 'closed', actor, 'Closed by the agent.')
          break
      }
    })
    this.changed()
    if (outcome.kind === 'items') this.checkDeps()
    return 'Recorded. Your work on this item is done: stop now.'
  }

  endRun(runId: string, end: { exitCode: number | null; costUsd?: number; turns?: number; result?: string; error?: string }): void {
    const run = this.db.prepare('SELECT * FROM runs WHERE id = ?').get(runId) as Row | undefined
    if (!run) return
    const itemId = run.item_id as string
    transaction(this.db, () => {
      this.db
        .prepare('UPDATE runs SET ended_at = ?, outcome = COALESCE(outcome, ?), cost_usd = ?, turns = ?, result = ? WHERE id = ?')
        .run(this.now(), end.error ? 'failed' : 'no_outcome', end.costUsd ?? null, end.turns ?? null, end.result ?? end.error ?? null, runId)
      this.record(itemId, 'system', 'run_ended', end.error ?? `Worker ${run.worker} finished (exit ${end.exitCode}).`, {
        runId,
        exitCode: end.exitCode,
        costUsd: end.costUsd ?? null,
        turns: end.turns ?? null,
      })
      this.db.prepare('UPDATE items SET worker = NULL WHERE id = ? AND worker = ?').run(itemId, run.worker)
      const item = this.must(itemId)
      if (!run.outcome && item.blocker === 'agent') {
        const question = end.error
          ? `The agent failed: ${end.error}`
          : `The agent stopped without saying what blocks the item.${end.result ? `\n\nIts last words: ${end.result}` : ''}`
        this.setBlocker(itemId, 'operator', 'system', question, { question })
      }
    })
    this.changed()
  }

  // ---- the loop ----

  /**
   * One turn of the central loop: check the waits that are due, release items
   * whose dependencies have closed, and poll the sources that are due.
   */
  async tick(): Promise<void> {
    await this.checkWaits()
    this.checkDeps()
    await this.pollSources(false)
  }

  async checkWaits(): Promise<void> {
    const due = this.db.prepare('SELECT item_id FROM waits WHERE next_check_at <= ? OR deadline <= ?').all(this.now(), this.now()) as Row[]
    for (const { item_id } of due) await this.checkWait(item_id as string)
  }

  private async checkWait(itemId: string): Promise<void> {
    const wait = this.wait(itemId)
    if (!wait) return
    const kind = this.registry.waits.find((w) => w.id === wait.kind)
    let result: string | null = null
    let error: string | null = null
    try {
      if (!kind) throw new Error(`No wait kind ${wait.kind}.`)
      result = await kind.check(wait.params, this.exec)
    } catch (e) {
      error = (e as Error).message
    }
    if (!this.wait(itemId)) return // the operator moved it while we were checking
    if (result !== null) {
      this.record(itemId, 'system', 'wait_resolved', result, { wait: wait.kind, params: wait.params })
      this.setBlocker(itemId, 'agent', 'system', `Done waiting for ${wait.description}.`)
    } else if (wait.deadline !== null && this.now() >= wait.deadline) {
      const question = `Gave up waiting for ${wait.description}.`
      this.setBlocker(itemId, 'operator', 'system', question, { question })
    } else {
      const step = Math.min(wait.step + 1, BACKOFF_MINUTES.length - 1)
      this.db
        .prepare('UPDATE waits SET step = ?, next_check_at = ?, last_error = ? WHERE item_id = ?')
        .run(step, this.now() + BACKOFF_MINUTES[step] * MINUTE, error, itemId)
      if (error && error !== wait.lastError) this.record(itemId, 'system', 'wait_error', error)
    }
    this.changed()
  }

  checkDeps(): void {
    const ready = this.db
      .prepare(
        `SELECT DISTINCT d.item_id FROM deps d JOIN items i ON i.id = d.item_id WHERE i.blocker = 'items'
         AND NOT EXISTS (SELECT 1 FROM deps d2 JOIN items j ON j.id = d2.depends_on WHERE d2.item_id = d.item_id AND j.blocker != 'closed')`,
      )
      .all() as Row[]
    for (const { item_id } of ready) {
      this.setBlocker(item_id as string, 'agent', 'system', 'The items it waited for have closed.')
    }
    if (ready.length) this.changed()
  }

  async pollSources(force: boolean): Promise<void> {
    for (const source of this.registry.sources) {
      const last = this.lastPoll.get(source.id) ?? 0
      if (!force && this.now() - last < source.everyMinutes * MINUTE) continue
      this.lastPoll.set(source.id, this.now())
      try {
        for (const found of await source.poll(this.exec))
          this.create(found.title, { source: source.id, ref: found.ref, url: found.url, state: found.state, actor: source.id })
      } catch (e) {
        this.emit('sourceError', `${source.id}: ${(e as Error).message}`)
      }
    }
  }
}
