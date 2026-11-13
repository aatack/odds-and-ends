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
//   - a `comments` or `notes` link to it from the topic it sits in (none for an
//     item in no item), sorted by its place there; who set its `type`, and when,
//     is who made the item, and when (egdb keeps both with every write);
//   - an `assignees` link from an item to each person, bot or script who has it,
//     and an `owns` link to it from its owner, each sorted by when it was made.
// An older build's `contains` and `assigned_to` links are read once, as the
// loader turns them into these (egdb.ts).
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
import type { LogEntry, LogQuery } from '../shared/types'

/** The links a topic holds what is in it by: its chat, and its tree. */
export type Via = 'comments' | 'notes'

export interface ItemRow {
  id: string
  parent_id: string | null
  /** The link it sits in its parent by; null for an item in none. */
  via: Via | null
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
  // In memory only.
  next_check_at: number | null
  last_error: string | null
  /** How many times a script has run since the app started. */
  runs: number
  /** When it was added and resolved, as moments (`Store.moment`); 0 for what was loaded. Order, where times tie. */
  added_n: number
  resolved_n: number
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
  /** The edit log: every value and link item the query keeps, the newest first. */
  log(query: LogQuery): Promise<LogEntry[]>
}

export const ROOT = '@root'
/** The values an item keeps beside its params. */
// (`resolved_at` and `resolved_by` are from a build that kept them: they are read, and never written.)
export const STATE_KEYS = new Set(['type', 'resolved', 'resolved_at', 'resolved_by', 'cancelled', 'data', 'state', 'reactions'])

/** One value as egdb takes it: a JSON-ish value as is, anything else (undefined) as null, which clears the key. */
const v = (x: unknown): Value => (x === undefined ? null : (x as Value))
const set = (entity: string, key: string, value: unknown): Op => ({ set: { entity, key, value: v(value) } })

/** One `assignees` (or `owns`) link, as memory holds it. */
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

/** An item pinned to another. */
export interface Pin {
  id: string
  at: number
  by: string
}

/** Who owns an item: an `owns` link to it. */
export interface Owned {
  owner: string
  at: number
  by: string
}

export class Store {
  readonly items = new Map<string, ItemRow>()
  /** Each item's items by seq; the key null holds those that sit in none. */
  private kids = new Map<string | null, ItemRow[]>()
  private byType = new Map<string, Set<string>>()
  readonly settings = new Map<string, unknown>()
  /** `assignees` links: each item's assignees, with when (and at which moment) and by whom; and each assignee's items. */
  private assigneesOf = new Map<string, Map<string, Assigned>>()
  private itemsOf = new Map<string, Set<string>>()
  /** Pins: `pins` links from an item to what is pinned to it (artefacts it needs: subtasks, PRs, files), each with when and by whom, the first pinned first. */
  private pinsOf = new Map<string, Pin[]>()
  /** The other way: the items each item is pinned to. */
  private pinnedTo = new Map<string, string[]>()
  /** `owns` links: each item's owner; and what each owner owns. */
  private ownerOf = new Map<string, Owned>()
  private ownedBy = new Map<string, Set<string>>()
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

  // ---- items ----

  private index(row: ItemRow): void {
    this.items.set(row.id, row)
    const kids = this.kids.get(row.parent_id) ?? []
    kids.push(row)
    this.kids.set(row.parent_id, kids)
    const typed = this.byType.get(row.type) ?? new Set()
    typed.add(row.id)
    this.byType.set(row.type, typed)
  }

  /** Adds an item: its type and params as values, and the link to it from where it sits (if anywhere), as its maker wrote them. */
  insertItem(row: ItemRow): void {
    row.added_n = this.moment()
    if (row.resolved) row.resolved_n = row.added_n
    this.index(row)
    const ops: Op[] = [set(row.id, 'type', row.type)]
    for (const [k, value] of Object.entries(row.params)) if (value !== undefined && value !== null) ops.push(set(row.id, k, value))
    // A blocker says it is open; a plain item says nothing, being resolved as it is made; a topic's `open` says how it stands.
    if (!row.resolved && row.type !== 'topic') ops.push(set(row.id, 'resolved', false))
    if (row.parent_id) ops.push({ link: { source: row.parent_id, destination: row.id, link_type: row.via ?? 'comments', sort: { explicit: row.seq } } })
    this.out(ops, row.created_by, row.created_at)
  }

  /**
   * Moves an item into another (null: into none), at its end, by `via` (its comments or its notes): the link it sits by
   * is removed (egdb keeps that it was) and one from where it goes is added. Who made it, and when, stay.
   */
  move(id: string, to: string | null, via: Via, by: string, at: number): void {
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    if (row.parent_id === to && (to === null || row.via === via)) return
    if (to && !this.items.has(to)) throw new Error(`No item ${to}.`)
    for (let p: string | null = to; p; p = this.items.get(p)?.parent_id ?? null) if (p === id) throw new Error("An item can't go inside itself.")
    const from = row.parent_id
    const fromVia = row.via
    this.kids.set(from, this.children(from).filter((r) => r.id !== id))
    Object.assign(row, { parent_id: to, via: to ? via : null, seq: this.nextSeq(to) })
    this.kids.set(to, [...this.children(to), row])
    this.out(
      [
        ...(from ? [{ link: { source: from, destination: id, link_type: fromVia ?? 'comments', sort: 'removed' as const } }] : []),
        ...(to ? [{ link: { source: to, destination: id, link_type: via, sort: { explicit: row.seq } } }] : []),
      ],
      by,
      at,
    )
  }

  /** Changes an item's params: the values that changed are written (a param gone is cleared), as `by` at `at`. */
  setParams(id: string, params: Record<string, unknown>, by: string, at: number): void {
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    const keys = new Set([...Object.keys(row.params), ...Object.keys(params)])
    const ops = [...keys]
      .filter((k) => JSON.stringify(row.params[k] ?? null) !== JSON.stringify(params[k] ?? null))
      .map((k) => set(id, k, params[k] ?? null))
    row.params = params
    this.out(ops, by, at)
  }

  /** Resolves a blocker: `resolved` turns true (egdb keeps when, and by whom), with whether it was cancelled, and what it resolved with. */
  resolveItem(id: string, data: Record<string, unknown>, by: string, at: number, cancelled: boolean): void {
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    Object.assign(row, { resolved: true, resolved_at: at, resolved_by: by, cancelled, data, next_check_at: null, resolved_n: this.moment() })
    this.out([set(id, 'resolved', true), ...(cancelled ? [set(id, 'cancelled', true)] : []), set(id, 'data', data)], by, at)
  }

  /** Opens a resolved event again: `resolved` turns false (egdb keeps that it was), and what it resolved with is cleared. */
  reopenItem(id: string, by: string, at: number): void {
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    const wasCancelled = row.cancelled
    Object.assign(row, { resolved: false, resolved_at: null, resolved_by: null, cancelled: false, data: null, resolved_n: 0 })
    // An older build's `resolved_at` would read as resolved still: it is cleared too.
    this.out([set(id, 'resolved', false), set(id, 'resolved_at', null), ...(wasCancelled ? [set(id, 'cancelled', null)] : []), set(id, 'data', null)], by, at)
  }

  /**
   * An item's status: open (true), closed (false) or none (null), as its `open` value. A topic closed reads as resolved:
   * when, and by whom, are kept (egdb keeps them too, as the event for `open`).
   */
  setOpen(id: string, open: boolean | null, by: string, at: number): void {
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    const { open: _o, ...rest } = row.params
    row.params = open === null ? rest : { ...rest, open }
    const closed = open === false
    // A topic closed reads as resolved; anything else keeps what `resolved` says of it (a plain item is resolved as it is added).
    if (row.type === 'topic' && closed !== row.resolved) Object.assign(row, { resolved: closed, resolved_at: closed ? at : null, resolved_by: closed ? by : null, resolved_n: closed ? this.moment() : 0 })
    this.out([set(id, 'open', open)], by, at)
  }

  /** Keeps what a script hands on to its next run. */
  setState(id: string, state: unknown, at: number): void {
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    row.memo = state ?? null
    this.out([set(id, 'state', state ?? null)], 'system', at)
  }

  private indexPin(from: string, pin: Pin): void {
    this.pinsOf.set(from, [...this.pins(from), pin])
    this.pinnedTo.set(pin.id, [...(this.pinnedTo.get(pin.id) ?? []), from])
  }

  /** Pins an item to another: a `pins` link from `from` to it. Pinning one twice, or to itself, does nothing. */
  pin(from: string, id: string, by: string, at: number): void {
    if (!this.items.has(id)) throw new Error(`No item ${id}.`)
    if (id === from || this.pins(from).some((p) => p.id === id)) return
    this.indexPin(from, { id, at, by })
    this.out([{ link: { source: from, destination: id, link_type: 'pins', sort: { explicit: at } } }], by, at)
  }

  /** Unpins it: the link that pinned it is removed (egdb keeps that it was). */
  unpin(from: string, id: string, by: string, at: number): void {
    const had = this.pins(from).find((p) => p.id === id)
    if (!had) return
    this.pinsOf.set(from, this.pins(from).filter((p) => p.id !== id))
    this.pinnedTo.set(id, (this.pinnedTo.get(id) ?? []).filter((f) => f !== from))
    this.out([{ link: { source: from, destination: id, link_type: 'pins', sort: 'removed' } }], by, at)
  }

  /** What is pinned to an item, the first pinned first. */
  pins(from: string): Pin[] {
    return this.pinsOf.get(from) ?? []
  }

  /** The items an item is pinned to, the first first. */
  pinnedIn(id: string): string[] {
    return this.pinnedTo.get(id) ?? []
  }

  /** The items in an item (or, for null, those in none), by seq: all of them, or only those it holds by `via`. */
  children(parentId: string | null, via?: Via): ItemRow[] {
    const all = this.kids.get(parentId) ?? []
    return via ? all.filter((r) => (r.via ?? 'comments') === via) : all
  }

  ofType(type: string): ItemRow[] {
    return [...(this.byType.get(type) ?? [])].map((id) => this.items.get(id)!)
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

  /** Every `assignees` link there is, as memory holds it. */
  allAssignments(): { item: string; to: string; given: Assigned }[] {
    return [...this.assigneesOf].flatMap(([item, m]) => [...m].map(([to, given]) => ({ item, to, given })))
  }

  /** The items assigned to someone: their `assignees` links, followed backwards. */
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

  /** Assigns an item (an `assignees` link from it, sorted by when), or takes it off someone (the link removed). */
  setAssigned(itemId: string, to: string, on: boolean, by: string, at: number): void {
    this.link(itemId, to, on ? { at, by, n: this.moment() } : null)
    this.out([{ link: { source: itemId, destination: to, link_type: 'assignees', sort: on ? { explicit: at } : 'removed' } }], by, at)
  }

  // ---- ownership ----

  /** Who owns an item (its `owns` link), or null. */
  owner(itemId: string): Owned | null {
    return this.ownerOf.get(itemId) ?? null
  }

  /** What someone owns: their `owns` links. */
  owned(owner: string): string[] {
    return [...(this.ownedBy.get(owner) ?? [])]
  }

  private indexOwner(itemId: string, o: Owned | null): void {
    const had = this.ownerOf.get(itemId)
    if (had) this.ownedBy.get(had.owner)?.delete(itemId)
    if (!o) return void this.ownerOf.delete(itemId)
    this.ownerOf.set(itemId, o)
    this.ownedBy.set(o.owner, (this.ownedBy.get(o.owner) ?? new Set()).add(itemId))
  }

  /** Gives an item to an owner (an `owns` link to it, sorted by when), taking it from whoever owned it; null: nobody. */
  setOwner(itemId: string, owner: string | null, by: string, at: number): void {
    const had = this.owner(itemId)
    if ((had?.owner ?? null) === owner) return
    this.indexOwner(itemId, owner ? { owner, at, by } : null)
    this.out(
      [
        ...(had ? [{ link: { source: had.owner, destination: itemId, link_type: 'owns', sort: 'removed' as const } }] : []),
        ...(owner ? [{ link: { source: owner, destination: itemId, link_type: 'owns', sort: { explicit: at } } }] : []),
      ],
      by,
      at,
    )
  }

  // ---- settings and custom actions ----

  setSetting(key: string, value: unknown, by = 'system'): void {
    this.settings.set(key, value)
    const e = `setting:${key}`
    this.out([set(e, 'type', 'wf.setting'), set(e, 'key', key), set(e, 'value', JSON.stringify(value))], by, this.now())
  }

  // ---- inspecting ----

  /** An item as egdb holds it; with no egdb (the tests), as memory has it. */
  async inspect(id: string): Promise<Inspection> {
    if (this.sink) return this.sink.inspect(id)
    const row = this.items.get(id)
    if (!row) throw new Error(`No item ${id}.`)
    const values: Record<string, unknown> = { type: row.type, ...row.params }
    // A blocker is one that was not resolved as it was added.
    if (row.type !== 'topic' && (!row.resolved || row.resolved_n !== row.added_n)) Object.assign(values, { resolved: row.resolved }, row.cancelled ? { cancelled: true } : {}, row.resolved ? { data: row.data } : {})
    const owner = this.owner(id)
    const links: LinkView[] = [
      ...(row.parent_id ? [{ type: row.via ?? 'comments', direction: 'inbound' as const, other: row.parent_id, sort: row.seq, at: row.created_at, by: row.created_by }] : []),
      ...this.children(id).map((k) => ({ type: k.via ?? 'comments', direction: 'outbound' as const, other: k.id, sort: k.seq, at: k.created_at, by: k.created_by })),
      ...this.pins(id).map((p) => ({ type: 'pins', direction: 'outbound' as const, other: p.id, sort: p.at, at: p.at, by: p.by })),
      ...this.pinnedIn(id).map((from) => {
        const p = this.pins(from).find((q) => q.id === id)!
        return { type: 'pins', direction: 'inbound' as const, other: from, sort: p.at, at: p.at, by: p.by }
      }),
      ...this.assignees(id).map((to) => ({ type: 'assignees', direction: 'outbound' as const, other: to, sort: null, at: this.assignment(id, to)!.at, by: this.assignment(id, to)!.by })),
      ...(owner ? [{ type: 'owns', direction: 'inbound' as const, other: owner.owner, sort: owner.at, at: owner.at, by: owner.by }] : []),
      ...this.owned(id).map((o) => ({ type: 'owns', direction: 'outbound' as const, other: o, sort: null, at: this.owner(o)!.at, by: this.owner(o)!.by })),
    ]
    return { id, values, set: {}, links, createdAt: row.created_at, createdBy: row.created_by }
  }

  /** The edit log, as egdb keeps it; with no egdb (the tests), empty: memory keeps no history. */
  log(query: LogQuery): Promise<LogEntry[]> {
    return this.sink?.log(query) ?? Promise.resolve([])
  }

  // ---- loading ----

  /** Puts back what was read from egdb, without writing it again. */
  restore(saved: {
    items: ItemRow[]
    settings: [string, unknown][]
    assigned?: { item: string; to: string; at: number; by: string }[]
    pinned?: { from: string; id: string; at: number; by: string }[]
    owners?: { item: string; owner: string; at: number; by: string }[]
  }): void {
    for (const p of [...(saved.pinned ?? [])].sort((a, b) => a.at - b.at)) if (!this.pins(p.from).some((q) => q.id === p.id)) this.indexPin(p.from, { id: p.id, at: p.at, by: p.by })
    for (const o of [...(saved.owners ?? [])].sort((a, b) => a.at - b.at)) this.indexOwner(o.item, { owner: o.owner, at: o.at, by: o.by })
    for (const a of saved.assigned ?? []) this.link(a.item, a.to, { at: a.at, by: a.by, n: 0 })
    for (const r of [...saved.items].sort((a, b) => a.seq - b.seq)) this.index(r)
    for (const [k, value] of saved.settings) this.settings.set(k, value)
  }
}

// ---- reading egdb back ----

type Values = Record<string, unknown>
const num = (x: unknown): number | null => (typeof x === 'number' ? x : null)
const str = (x: unknown): string | null => (typeof x === 'string' ? x : null)
const obj = <T>(x: unknown): T | null => (x && typeof x === 'object' && !Array.isArray(x) ? (x as T) : null)

/**
 * An older build's `task` and `note` as the topic they are now, read and never rewritten (so the older build still reads
 * them): a task's `title` is its `text`, and it is open till its `resolved` turned true (unless an `open` was set since);
 * a note keeps its checkbox (`open`) and shows its notes. Anything else is as it was.
 */
export function legacyValues(x: Values): Values {
  if (x.type === 'task') {
    const { title, resolved, cancelled: _c, data: _d, ...rest } = x
    return { ...rest, type: 'topic', text: rest.text ?? title, open: typeof rest.open === 'boolean' ? rest.open : !(resolved === true || num(rest.resolved_at) !== null) }
  }
  if (x.type === 'note') return { ...x, type: 'topic', display: x.display ?? 'notes' }
  return x
}

/**
 * An item from its values (as `legacyValues` reads them), where it sits (its parent, the link it sits by and its place),
 * and who made it when. A plain item (`blocking` false) is resolved as it was made; a blocker is resolved once its
 * `resolved` is true; a topic is resolved (closed) while its `open` is false.
 */
export function itemFromValues(
  id: string,
  x: Values,
  made: { parent: string | null; via: Via | null; seq: number; at: number; by: string },
  blocking: boolean,
  /** The write that resolved (or closed) it: when, and by whom. */
  resolvedBy?: { at: number; by: string },
): ItemRow {
  const params = Object.fromEntries(Object.entries(x).filter(([k]) => !STATE_KEYS.has(k)))
  const topic = x.type === 'topic'
  const resolved = topic ? x.open === false : x.resolved === true || num(x.resolved_at) !== null
  const held = topic ? resolved : blocking
  return {
    id,
    parent_id: made.parent,
    via: made.parent ? made.via : null,
    seq: made.seq,
    type: String(x.type),
    params,
    created_by: made.by,
    created_at: made.at,
    resolved: topic ? resolved : !blocking || resolved,
    resolved_at: held ? (resolvedBy?.at ?? num(x.resolved_at)) : topic ? null : made.at,
    resolved_by: held ? (resolvedBy?.by ?? str(x.resolved_by)) : topic ? null : made.by,
    cancelled: !topic && x.cancelled === true,
    data: topic ? null : obj(x.data),
    memo: x.state ?? null,
    // Waits are checked at once after a restart.
    next_check_at: null,
    last_error: null,
    runs: 0,
    added_n: 0,
    resolved_n: 0,
  }
}

