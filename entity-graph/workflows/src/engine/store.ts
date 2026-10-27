// The store. The engine reads and writes it synchronously, in memory; every
// change is also queued as egdb ops and written, in order, to an egdb server
// (`EgdbSink`), which is where it lasts. On start the whole store is read back
// from egdb (`loadStore`). The tests use it with no sink, in memory only.
//
// In egdb every item is an entity, and nothing more is kept than it needs:
//   - its `type`, and its params as values of their own (a note is `type` and
//     `text`); a blocker, `resolved` (false, then true), and once resolved
//     `cancelled` (if so) and `data`; a script, the `state` it hands on. When
//     it resolved, and by whom, is egdb's history of `resolved`;
//   - a `contains` link to it from the item it sits in (or from `@root`),
//     sorted by its place there; who wrote that link, and when, is who made the
//     item, and when (egdb keeps both with every write);
//   - an `assigned_to` link from a task to each person or bot who has it.
// Every write says who made it (its author) and when, and egdb keeps every value
// and link an entity ever had: that is the history. There is no trace or run
// table of the app's own.
//
// Settings and the people's custom actions are entities too: `setting:<key>`
// and `custom-action:<id>`.
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
  /** When it resolved, and by whom: known for what resolved since the app started (egdb keeps the rest). */
  resolved_at: number | null
  resolved_by: string | null
  cancelled: boolean
  data: Record<string, unknown> | null
  /** What a script handed on to its next run. */
  memo: unknown
  /** Who reacted to it (a thumbs-up): people's usernames, or bots' actors. */
  reactions: string[]
  // In memory only.
  check_step: number
  next_check_at: number | null
  last_error: string | null
  /** How many times a script has run since the app started. */
  runs: number
  /** When it was added and resolved, as moments (`Store.moment`); 0 for what was loaded. Order, where times tie. */
  added_n: number
  resolved_n: number
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

/** One link, as the debug view shows it. */
export interface LinkView {
  type: string
  direction: 'outbound' | 'inbound'
  other: string
  sort: number | null
  at: number | null
  by: string | null
}

/** An item as egdb holds it: its values and links, and who made it and when. */
export interface Inspection {
  id: string
  values: Record<string, unknown>
  /** When each value was set, and by whom (egdb's event for it); empty with no egdb. */
  set: Record<string, { at: number; by: string }>
  links: LinkView[]
  createdAt: number | null
  createdBy: string | null
}

/** Where the store's changes go to last: ops, as written by `author` at `at`. */
export interface Sink {
  write(ops: Op[], author: string, at: number): void
  /** Settles once everything written so far has landed. */
  flush(): Promise<void>
  /** An entity as egdb holds it. */
  inspect(id: string): Promise<Inspection>
}

export const ROOT = '@root'
/** The values an event keeps beside its params. */
// (`resolved_at` and `resolved_by` are from a build that kept them: they are read, and never written.)
export const STATE_KEYS = new Set(['type', 'resolved', 'resolved_at', 'resolved_by', 'cancelled', 'data', 'state', 'reactions'])

/** One value as egdb takes it: a JSON-ish value as is, anything else (undefined) as null, which clears the key. */
const v = (x: unknown): Value => (x === undefined ? null : (x as Value))
const set = (entity: string, key: string, value: unknown): Op => ({ set: { entity, key, value: v(value) } })

/** One `assigned_to` link, as memory holds it. */
export interface Assigned {
  at: number
  by: string
  /** The moment it was written; 0 for one that was loaded. */
  n: number
}

interface Held {
  ops: Op[]
  author: string
  at: number
}

export class Store {
  readonly events = new Map<string, EventRow>()
  /** Each item's events by seq; the key null holds those that sit in none. */
  private kids = new Map<string | null, EventRow[]>()
  private byType = new Map<string, Set<string>>()
  readonly settings = new Map<string, unknown>()
  readonly customActions = new Map<string, CustomActionRow>()
  /** `assigned_to` links: each task's assignees, with when (and at which moment) and by whom; and each assignee's tasks. */
  private assigneesOf = new Map<string, Map<string, Assigned>>()
  private itemsOf = new Map<string, Set<string>>()
  /** Writes held back while a `batch` runs, so that they land together. */
  private held: Held[] | null = null
  /** The last moment handed out. */
  private moments = 0

  constructor(
    private sink: Sink | null = null,
    /** What the app's own writes (settings) are written as. */
    private now: () => number = Date.now,
  ) {}

  private out(ops: Op[], author: string, at: number): void {
    if (!ops.length) return
    if (this.held) this.held.push({ ops, author, at })
    else this.sink?.write(ops, author, at)
  }

  /** Runs `fn`, writing what it changed to egdb together. */
  batch<T>(fn: () => T): T {
    if (this.held) return fn()
    this.held = []
    try {
      return fn()
    } finally {
      const held = this.held
      this.held = null
      for (const h of held) this.sink?.write(h.ops, h.author, h.at)
    }
  }

  flush(): Promise<void> {
    return this.sink?.flush() ?? Promise.resolve()
  }

  /**
   * A new moment: a number that only rises, given to each change as memory makes
   * it, so that what came first is known even within one millisecond. Not
   * written: what was loaded is moment 0, and is ordered by time.
   */
  moment(): number {
    return ++this.moments
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

  /** Adds an event: its type and params as values, and the `contains` link to it, as its maker wrote them. */
  insertEvent(row: EventRow): void {
    row.added_n = this.moment()
    if (row.resolved) row.resolved_n = row.added_n
    this.index(row)
    const ops: Op[] = [set(row.id, 'type', row.type)]
    for (const [k, value] of Object.entries(row.params)) if (value !== undefined && value !== null) ops.push(set(row.id, k, value))
    // A blocker says it is open; a plain event says nothing, being resolved as it is made.
    if (!row.resolved) ops.push(set(row.id, 'resolved', false))
    ops.push({ link: { source: row.parent_id ?? ROOT, destination: row.id, link_type: 'contains', sort: { explicit: row.seq } } })
    this.out(ops, row.created_by, row.created_at)
  }

  /** Changes an event's params: the values that changed are written (a param gone is cleared), as `by` at `at`. */
  setParams(id: string, params: Record<string, unknown>, by: string, at: number): void {
    const row = this.events.get(id)
    if (!row) throw new Error(`No event ${id}.`)
    const keys = new Set([...Object.keys(row.params), ...Object.keys(params)])
    const ops = [...keys]
      .filter((k) => JSON.stringify(row.params[k] ?? null) !== JSON.stringify(params[k] ?? null))
      .map((k) => set(id, k, params[k] ?? null))
    row.params = params
    this.out(ops, by, at)
  }

  /** Resolves a blocker: `resolved` turns true (egdb keeps when, and by whom), with whether it was cancelled, and what it resolved with. */
  resolveEvent(id: string, data: Record<string, unknown>, by: string, at: number, cancelled: boolean): void {
    const row = this.events.get(id)
    if (!row) throw new Error(`No event ${id}.`)
    Object.assign(row, { resolved: true, resolved_at: at, resolved_by: by, cancelled, data, next_check_at: null, resolved_n: this.moment() })
    this.out([set(id, 'resolved', true), ...(cancelled ? [set(id, 'cancelled', true)] : []), set(id, 'data', data)], by, at)
  }

  /** Keeps what a script hands on to its next run. */
  setState(id: string, state: unknown, at: number): void {
    const row = this.events.get(id)
    if (!row) throw new Error(`No event ${id}.`)
    row.memo = state ?? null
    this.out([set(id, 'state', state ?? null)], 'system', at)
  }

  /** Who reacted to an event, as `by` changed it at `at`. */
  setReactions(id: string, reactions: string[], by: string, at: number): void {
    const row = this.events.get(id)
    if (!row) throw new Error(`No event ${id}.`)
    row.reactions = reactions
    this.out([set(id, 'reactions', reactions)], by, at)
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

  // ---- assignment links ----

  /** Who a task is assigned to, the earliest assigned first. */
  assignees(itemId: string): string[] {
    return [...(this.assigneesOf.get(itemId) ?? new Map<string, Assigned>())].sort((a, b) => a[1].at - b[1].at || a[1].n - b[1].n).map(([to]) => to)
  }

  /** When, and by whom, someone was given a task, while they have it. */
  assignment(itemId: string, to: string): Assigned | null {
    return this.assigneesOf.get(itemId)?.get(to) ?? null
  }

  /** Every `assigned_to` link there is, as memory holds it. */
  allAssignments(): { item: string; to: string; given: Assigned }[] {
    return [...this.assigneesOf].flatMap(([item, m]) => [...m].map(([to, given]) => ({ item, to, given })))
  }

  /** The tasks assigned to someone: their `assigned_to` links, followed backwards. */
  assignedTo(assignee: string): string[] {
    return [...(this.itemsOf.get(assignee) ?? [])]
  }

  private link(itemId: string, to: string, given: Assigned | null): void {
    const mine = this.assigneesOf.get(itemId) ?? new Map<string, Assigned>()
    const theirs = this.itemsOf.get(to) ?? new Set<string>()
    if (given === null) {
      mine.delete(to)
      theirs.delete(itemId)
    } else {
      mine.set(to, given)
      theirs.add(itemId)
    }
    this.assigneesOf.set(itemId, mine)
    this.itemsOf.set(to, theirs)
  }

  /** Assigns a task (an `assigned_to` link from it, sorted by when), or takes it off someone (the link removed). */
  setAssigned(itemId: string, to: string, on: boolean, by: string, at: number): void {
    this.link(itemId, to, on ? { at, by, n: this.moment() } : null)
    this.out([{ link: { source: itemId, destination: to, link_type: 'assigned_to', sort: on ? { explicit: at } : 'removed' } }], by, at)
  }

  // ---- settings and custom actions ----

  setSetting(key: string, value: unknown): void {
    this.settings.set(key, value)
    const e = `setting:${key}`
    this.out([set(e, 'type', 'wf.setting'), set(e, 'key', key), set(e, 'value', JSON.stringify(value))], 'system', this.now())
  }

  putCustomAction(row: CustomActionRow): void {
    this.customActions.set(row.id, row)
    const e = `custom-action:${row.id}`
    // `type` is egdb's own key, so the event type the action is for is `for_type`.
    const { type, ...rest } = row
    this.out([set(e, 'type', 'wf.custom_action'), set(e, 'for_type', type), ...Object.entries(rest).map(([k, value]) => set(e, k, value))], 'system', this.now())
  }

  deleteCustomAction(id: string): void {
    if (!this.customActions.delete(id)) return
    // A null type archives the entity in egdb: it is kept, and read no more.
    this.out([set(`custom-action:${id}`, 'type', null)], 'system', this.now())
  }

  // ---- inspecting ----

  /** An item as egdb holds it; with no egdb (the tests), as memory has it. */
  async inspect(id: string): Promise<Inspection> {
    if (this.sink) return this.sink.inspect(id)
    const row = this.events.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    const values: Record<string, unknown> = { type: row.type, ...row.params }
    // A blocker is one that was not resolved as it was added.
    if (!row.resolved || row.resolved_n !== row.added_n) Object.assign(values, { resolved: row.resolved }, row.cancelled ? { cancelled: true } : {}, row.resolved ? { data: row.data } : {})
    const links: LinkView[] = [
      { type: 'contains', direction: 'inbound', other: row.parent_id ?? ROOT, sort: row.seq, at: row.created_at, by: row.created_by },
      ...this.children(id).map((k) => ({ type: 'contains', direction: 'outbound' as const, other: k.id, sort: k.seq, at: k.created_at, by: k.created_by })),
      ...this.assignees(id).map((to) => ({ type: 'assigned_to', direction: 'outbound' as const, other: to, sort: null, at: this.assignment(id, to)!.at, by: this.assignment(id, to)!.by })),
    ]
    return { id, values, set: {}, links, createdAt: row.created_at, createdBy: row.created_by }
  }

  // ---- loading ----

  /** Puts back what was read from egdb, without writing it again. */
  restore(rows: {
    events: EventRow[]
    settings: [string, unknown][]
    customActions: CustomActionRow[]
    assigned?: { item: string; to: string; at: number; by: string }[]
  }): void {
    for (const a of rows.assigned ?? []) this.link(a.item, a.to, { at: a.at, by: a.by, n: 0 })
    for (const r of [...rows.events].sort((a, b) => a.seq - b.seq)) this.index(r)
    for (const [k, value] of rows.settings) this.settings.set(k, value)
    for (const r of rows.customActions) this.customActions.set(r.id, r)
  }
}

// ---- reading egdb back ----

type Values = Record<string, unknown>
const num = (x: unknown): number | null => (typeof x === 'number' ? x : null)
const str = (x: unknown): string | null => (typeof x === 'string' ? x : null)
const obj = <T>(x: unknown): T | null => (x && typeof x === 'object' && !Array.isArray(x) ? (x as T) : null)

/**
 * An event from its values and the `contains` link to it (where it sits, its
 * place, and who made it when). A plain event (`blocking` false) is resolved as
 * it was made; a blocker is resolved once its `resolved` is true.
 */
export function eventFromValues(
  id: string,
  x: Values,
  made: { parent: string | null; seq: number; at: number; by: string },
  blocking: boolean,
  /** The event that set `resolved` true: when it resolved, and by whom. */
  resolvedBy?: { at: number; by: string },
): EventRow {
  const params = Object.fromEntries(Object.entries(x).filter(([k]) => !STATE_KEYS.has(k)))
  const resolved = x.resolved === true || num(x.resolved_at) !== null
  return {
    id,
    parent_id: made.parent,
    seq: made.seq,
    type: String(x.type),
    params,
    created_by: made.by,
    created_at: made.at,
    resolved: !blocking || resolved,
    resolved_at: blocking ? (resolvedBy?.at ?? num(x.resolved_at)) : made.at,
    resolved_by: blocking ? (resolvedBy?.by ?? str(x.resolved_by)) : made.by,
    cancelled: x.cancelled === true,
    data: obj(x.data),
    memo: x.state ?? null,
    reactions: Array.isArray(x.reactions) ? x.reactions.filter((r): r is string => typeof r === 'string') : [],
    check_step: 0,
    // Waits are checked at once after a restart.
    next_check_at: null,
    last_error: null,
    runs: 0,
    added_n: 0,
    resolved_n: 0,
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
