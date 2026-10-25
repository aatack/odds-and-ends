// The store. The engine reads and writes it synchronously, in memory; every
// change is also queued as egdb ops and written, in order, to an egdb server
// (`EgdbSink`), which is where it lasts. On start the whole store is read back
// from egdb (`loadStore`). The tests use it with no sink, in memory only.
//
// In egdb every row is an entity:
//   - an event (an item included) is its own id, its `type` the event type, and
//     its fields as values; it is the `child` link (sorted by seq) of the item it
//     sits in, or of `@root` when it sits in none;
//   - a trace row is `trace:<n>` (type `wf.trace`), a run `run:<id>` (`wf.run`),
//     a setting `setting:<key>` (`wf.setting`), a custom action
//     `custom-action:<id>` (`wf.custom_action`; deleting one archives it), and
//     a finished run's transcript `transcript:<runId>` (`wf.transcript`), which
//     is read only when asked for.
// egdb keeps every value an entity ever had, so "never change a row" holds
// there by construction: a change is a new event on top of the old.
//
// What only the running app needs (when a blocker is next checked, its backoff
// step, its last error) is held in memory and not written: after a restart
// every wait is checked at once.

import type { Op, Value } from 'egdb-client'

export interface EventRow {
  id: string
  parent_id: string | null
  seq: number
  type: string
  params: Record<string, unknown>
  created_by: string
  created_at: number
  resolved: boolean
  resolved_at: number | null
  resolved_by: string | null
  cancelled: boolean
  /** Its type's start hook has run (or it has none). */
  started: boolean
  data: Record<string, unknown> | null
  /** What a script handed on to its next run. */
  memo: unknown
  // In memory only.
  check_step: number
  next_check_at: number | null
  last_error: string | null
}

export interface TraceRow {
  id: number
  at: number
  item_id: string
  event_id: string | null
  run_id: string | null
  actor: string
  kind: string
  data: Record<string, unknown> | null
}

export interface RunRow {
  id: string
  /** The bot that ran, or null for a run from before bots were their own items. */
  agent_id: string | null
  /** The items it was woken for. */
  item_ids: string[]
  /** The events it answered (older runs: the item's bot blockers). */
  event_ids: string[]
  started_at: number
  /** The last trace row as the bot was prompted: the next run is told everything after it. */
  trace_mark: number
  ended_at: number | null
  cost_usd: number | null
  turns: number | null
  result: string | null
  prompt: string
}

export interface CustomActionRow {
  id: string
  type: string
  name: string
  description: string
  notes: string
  source: string
  icon: string
  important: boolean
  created_at: number
  updated_at: number
}

/** Where the store's changes go to last. */
export interface Sink {
  write(ops: Op[]): void
  /** Settles once everything written so far has landed. */
  flush(): Promise<void>
  /** A run's transcript, which is kept out of memory once the run is over. */
  transcript(runId: string): Promise<string | null>
}

const EVENT_KEYS = ['parent', 'seq', 'params', 'created_by', 'created_at', 'resolved', 'resolved_at', 'resolved_by', 'cancelled', 'started', 'data', 'memo'] as const
const RUN_KEYS = ['agent', 'items', 'events', 'started_at', 'trace_mark', 'ended_at', 'cost_usd', 'turns', 'result', 'prompt'] as const
export const ROOT = '@root'
/** Characters in each part of a transcript, as egdb keeps it. */
export const TRANSCRIPT_PART = 256 * 1024

/** One value as egdb takes it: a JSON-ish value as is, anything else (undefined) as null, which clears the key. */
const v = (x: unknown): Value => (x === undefined ? null : (x as Value))
const set = (entity: string, key: string, value: unknown): Op => ({ set: { entity, key, value: v(value) } })

export class Store {
  readonly events = new Map<string, EventRow>()
  /** Each item's events by seq; the key null holds those that sit in none. */
  private kids = new Map<string | null, EventRow[]>()
  private byType = new Map<string, Set<string>>()
  readonly trace: TraceRow[] = []
  private traceByItem = new Map<string, TraceRow[]>()
  private traceByEvent = new Map<string, TraceRow[]>()
  readonly runs = new Map<string, RunRow>()
  /** Transcripts of the runs still going (and, with no sink, of every run). */
  private transcripts = new Map<string, string>()
  readonly settings = new Map<string, unknown>()
  readonly customActions = new Map<string, CustomActionRow>()
  /** `assigned_to` links: each item's assignees, with when each was assigned; and the other way, each assignee's items. */
  private assigneesOf = new Map<string, Map<string, number>>()
  private itemsOf = new Map<string, Set<string>>()
  /** Ops held back while a `batch` runs, so that they land as one write. */
  private held: Op[] | null = null

  constructor(private sink: Sink | null = null) {}

  private out(ops: Op[]): void {
    if (!ops.length) return
    if (this.held) this.held.push(...ops)
    else this.sink?.write(ops)
  }

  /** Runs `fn`, writing what it changed to egdb as one batch. */
  batch<T>(fn: () => T): T {
    if (this.held) return fn()
    this.held = []
    try {
      return fn()
    } finally {
      const ops = this.held
      this.held = null
      this.out(ops)
    }
  }

  flush(): Promise<void> {
    return this.sink?.flush() ?? Promise.resolve()
  }

  // ---- events ----

  private index(row: EventRow): void {
    this.events.set(row.id, row)
    const kids = this.kids.get(row.parent_id) ?? []
    kids.push(row)
    this.kids.set(row.parent_id, kids)
    const typed = this.byType.get(row.type) ?? new Set()
    typed.add(row.id)
    this.byType.set(row.type, typed)
  }

  insertEvent(row: EventRow): void {
    this.index(row)
    const ops: Op[] = [set(row.id, 'type', row.type)]
    for (const k of EVENT_KEYS) {
      const value = eventValue(row, k)
      if (value !== null && value !== undefined) ops.push(set(row.id, k, value))
    }
    ops.push({ link: { source: row.parent_id ?? ROOT, destination: row.id, link_type: 'child', sort: { explicit: row.seq } } })
    this.out(ops)
  }

  /** Changes an event. Only the fields that last are written. */
  updateEvent(id: string, patch: Partial<EventRow>): void {
    const row = this.events.get(id)
    if (!row) throw new Error(`No event ${id}.`)
    Object.assign(row, patch)
    this.out(EVENT_KEYS.filter((k) => fieldOf(k) in patch).map((k) => set(id, k, eventValue(row, k))))
  }

  /** The events in an item (or, for null, those in none), by seq. */
  children(parentId: string | null): EventRow[] {
    return this.kids.get(parentId) ?? []
  }

  ofType(type: string): EventRow[] {
    return [...(this.byType.get(type) ?? [])].map((id) => this.events.get(id)!)
  }

  nextSeq(parentId: string | null): number {
    return (this.kids.get(parentId)?.at(-1)?.seq ?? 0) + 1
  }

  // ---- the trace ----

  /** Adds a trace row: the next id, or (importing) its own, which must come after the last. */
  appendTrace(row: Omit<TraceRow, 'id'> & { id?: number }): TraceRow {
    const id = row.id ?? (this.trace.at(-1)?.id ?? 0) + 1
    if (id <= this.lastTraceId()) throw new Error(`Trace row ${id} is not after the last.`)
    const full = { ...row, id }
    this.indexTrace(full)
    const e = `trace:${full.id}`
    this.out([
      set(e, 'type', 'wf.trace'), set(e, 'n', full.id), set(e, 'at', full.at), set(e, 'item', full.item_id), set(e, 'event', full.event_id),
      set(e, 'run', full.run_id), set(e, 'actor', full.actor), set(e, 'kind', full.kind), set(e, 'data', full.data),
    ].filter((op) => 'set' in op && op.set.value !== null))
    return full
  }

  private indexTrace(row: TraceRow): void {
    this.trace.push(row)
    const add = (m: Map<string, TraceRow[]>, k: string | null) => {
      if (!k) return
      const list = m.get(k) ?? []
      list.push(row)
      m.set(k, list)
    }
    add(this.traceByItem, row.item_id)
    add(this.traceByEvent, row.event_id)
  }

  traceOfItem(itemId: string): TraceRow[] {
    return this.traceByItem.get(itemId) ?? []
  }

  traceOfEvent(eventId: string): TraceRow[] {
    return this.traceByEvent.get(eventId) ?? []
  }

  lastTraceId(): number {
    return this.trace.at(-1)?.id ?? 0
  }

  /** The trace rows after `mark`, oldest first. */
  traceAfter(mark: number): TraceRow[] {
    // Ids rise by one, so the rows after `mark` are a tail of the list.
    let lo = 0
    let hi = this.trace.length
    while (lo < hi) {
      const mid = (lo + hi) >> 1
      if (this.trace[mid].id <= mark) lo = mid + 1
      else hi = mid
    }
    return this.trace.slice(lo)
  }

  // ---- runs ----

  insertRun(row: RunRow, transcript = ''): void {
    this.runs.set(row.id, row)
    this.transcripts.set(row.id, transcript)
    const e = `run:${row.id}`
    this.out([set(e, 'type', 'wf.run'), ...RUN_KEYS.map((k) => set(e, k, runValue(row, k))).filter((op) => 'set' in op && op.set.value !== null)])
  }

  updateRun(id: string, patch: Partial<RunRow>): void {
    const row = this.runs.get(id)
    if (!row) throw new Error(`No run ${id}.`)
    Object.assign(row, patch)
    const e = `run:${id}`
    this.out(RUN_KEYS.filter((k) => runField(k) in patch).map((k) => set(e, k, runValue(row, k))))
  }

  appendTranscript(runId: string, chunk: string): void {
    this.transcripts.set(runId, (this.transcripts.get(runId) ?? '') + chunk)
  }

  /** Writes a finished run's transcript, once, and lets it go from memory. */
  endTranscript(runId: string): void {
    const text = this.transcripts.get(runId) ?? ''
    // An entity of its own, so that loading the runs never reads the transcripts; in parts, since egdb takes a
    // request of a few megabytes at most.
    const e = `transcript:${runId}`
    const parts = Math.max(1, Math.ceil(text.length / TRANSCRIPT_PART))
    this.out([
      set(e, 'type', 'wf.transcript'), set(e, 'parts', parts),
      ...Array.from({ length: parts }, (_, i) => set(e, `part:${i}`, text.slice(i * TRANSCRIPT_PART, (i + 1) * TRANSCRIPT_PART))),
    ])
    if (this.sink) this.transcripts.delete(runId)
  }

  async transcript(runId: string): Promise<string | null> {
    const held = this.transcripts.get(runId)
    if (held !== undefined) return held
    return this.sink ? this.sink.transcript(runId) : null
  }

  // ---- assignment links ----

  /** Who an item is assigned to, the earliest assigned first. */
  assignees(itemId: string): string[] {
    return [...(this.assigneesOf.get(itemId) ?? new Map<string, number>())].sort((a, b) => a[1] - b[1]).map(([to]) => to)
  }

  /** The items assigned to someone: their `assigned_to` links, followed backwards. */
  assignedTo(assignee: string): string[] {
    return [...(this.itemsOf.get(assignee) ?? [])]
  }

  private link(itemId: string, to: string, at: number | null): void {
    const mine = this.assigneesOf.get(itemId) ?? new Map<string, number>()
    const theirs = this.itemsOf.get(to) ?? new Set<string>()
    if (at === null) {
      mine.delete(to)
      theirs.delete(itemId)
    } else {
      mine.set(to, at)
      theirs.add(itemId)
    }
    this.assigneesOf.set(itemId, mine)
    this.itemsOf.set(to, theirs)
  }

  /** Assigns an item (an `assigned_to` link from it, sorted by when), or takes it off someone (the link removed). */
  setAssigned(itemId: string, to: string, on: boolean, at: number): void {
    this.link(itemId, to, on ? at : null)
    this.out([{ link: { source: itemId, destination: to, link_type: 'assigned_to', sort: on ? { explicit: at } : 'removed' } }])
  }

  // ---- settings and custom actions ----

  setSetting(key: string, value: unknown): void {
    this.settings.set(key, value)
    const e = `setting:${key}`
    this.out([set(e, 'type', 'wf.setting'), set(e, 'key', key), set(e, 'value', JSON.stringify(value))])
  }

  putCustomAction(row: CustomActionRow): void {
    this.customActions.set(row.id, row)
    const e = `custom-action:${row.id}`
    // `type` is egdb's own key, so the event type the action is for is `for_type`.
    const { type, ...rest } = row
    this.out([set(e, 'type', 'wf.custom_action'), set(e, 'for_type', type), ...Object.entries(rest).map(([k, value]) => set(e, k, value))])
  }

  deleteCustomAction(id: string): void {
    if (!this.customActions.delete(id)) return
    // A null type archives the entity in egdb: it is kept, and read no more.
    this.out([set(`custom-action:${id}`, 'type', null)])
  }

  // ---- loading ----

  /** Puts back rows read from egdb, without writing them again. */
  restore(rows: {
    events: EventRow[]
    trace: TraceRow[]
    runs: RunRow[]
    settings: [string, unknown][]
    customActions: CustomActionRow[]
    assigned?: { item: string; to: string; at: number }[]
  }): void {
    for (const a of rows.assigned ?? []) this.link(a.item, a.to, a.at)
    for (const r of [...rows.events].sort((a, b) => a.seq - b.seq)) this.index(r)
    for (const r of [...rows.trace].sort((a, b) => a.id - b.id)) this.indexTrace(r)
    for (const r of rows.runs) this.runs.set(r.id, r)
    for (const [k, value] of rows.settings) this.settings.set(k, value)
    for (const r of rows.customActions) this.customActions.set(r.id, r)
  }
}

const fieldOf = (k: (typeof EVENT_KEYS)[number]): keyof EventRow => (k === 'parent' ? 'parent_id' : k)
const eventValue = (row: EventRow, k: (typeof EVENT_KEYS)[number]): unknown => row[fieldOf(k)]
const runField = (k: (typeof RUN_KEYS)[number]): keyof RunRow =>
  k === 'agent' ? 'agent_id' : k === 'items' ? 'item_ids' : k === 'events' ? 'event_ids' : k
const runValue = (row: RunRow, k: (typeof RUN_KEYS)[number]): unknown => row[runField(k)]

// ---- reading egdb back ----

type Values = Record<string, unknown>
const num = (x: unknown): number | null => (typeof x === 'number' ? x : null)
const str = (x: unknown): string | null => (typeof x === 'string' ? x : null)
const obj = <T>(x: unknown): T | null => (x && typeof x === 'object' && !Array.isArray(x) ? (x as T) : null)

export function eventFromValues(id: string, type: string, x: Values): EventRow {
  return {
    id,
    parent_id: str(x.parent),
    seq: num(x.seq) ?? 0,
    type,
    params: obj(x.params) ?? {},
    created_by: str(x.created_by) ?? 'system',
    created_at: num(x.created_at) ?? 0,
    resolved: x.resolved === true,
    resolved_at: num(x.resolved_at),
    resolved_by: str(x.resolved_by),
    cancelled: x.cancelled === true,
    started: x.started === true,
    data: obj(x.data),
    memo: x.memo ?? null,
    check_step: 0,
    // Waits are checked at once after a restart.
    next_check_at: null,
    last_error: null,
  }
}

export function traceFromValues(x: Values): TraceRow {
  return {
    id: num(x.n) ?? 0,
    at: num(x.at) ?? 0,
    item_id: str(x.item) ?? '',
    event_id: str(x.event),
    run_id: str(x.run),
    actor: str(x.actor) ?? 'system',
    kind: str(x.kind) ?? '',
    data: obj(x.data),
  }
}

export function runFromValues(id: string, x: Values): RunRow {
  return {
    id,
    agent_id: str(x.agent),
    item_ids: Array.isArray(x.items) ? (x.items as string[]) : [],
    event_ids: Array.isArray(x.events) ? (x.events as string[]) : [],
    started_at: num(x.started_at) ?? 0,
    trace_mark: num(x.trace_mark) ?? 0,
    ended_at: num(x.ended_at),
    cost_usd: num(x.cost_usd),
    turns: num(x.turns),
    result: str(x.result),
    prompt: str(x.prompt) ?? '',
  }
}

export function customActionFromValues(x: Values): CustomActionRow {
  return {
    id: str(x.id) ?? '',
    type: str(x.for_type) ?? '',
    name: str(x.name) ?? '',
    description: str(x.description) ?? '',
    notes: str(x.notes) ?? '',
    source: str(x.source) ?? '',
    icon: str(x.icon) ?? 'zap',
    important: x.important === true,
    created_at: num(x.created_at) ?? 0,
    updated_at: num(x.updated_at) ?? 0,
  }
}
