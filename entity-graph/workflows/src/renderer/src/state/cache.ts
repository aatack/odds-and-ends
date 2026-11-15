// What the app shows, kept here rather than asked for each time it is drawn: the overview and each item's detail,
// as the backend last said them, with the changes made here and not yet answered laid over them. A view draws
// whatever is here at once (an item not fetched yet, from what the overview says of it, its chat Loading…), and the
// backend is asked in the background: after a change it says, an item opened, and every change made here. On the
// phone (a browser) it is also kept in local storage, so the app opens onto what it last showed, and the open items
// in the person's lists are fetched ahead, so opening one is instant.
//
// Changes made here go in the outbox (`withOptimism`, on api()'s calls): drawn at once, the call answered at once,
// and sent to the backend in order behind. One the backend refuses is taken off, and says why (`onError`). One that
// can't reach it (offline, or the backend restarting) stays drawn and is sent again until it lands, kept across
// reloads on the phone; what it adds carries an id chosen here, so sending it twice adds it once.

import type { Item, ItemInfo, ItemInfoDetail, Overview, WorkflowsAPI } from '../../../shared/types'
import { isOffline } from '../transport'

export interface Shown {
  overview: Overview | null
  /** Each item's detail: undefined not fetched yet, null not there. */
  details: Record<string, ItemInfoDetail | null | undefined>
  /** Changes made here the backend has not taken yet. */
  unsent: number
  /** The last try to send them could not reach the backend. */
  offline: boolean
}

type Patch = (s: Shown) => Shown
/** A change made here, as sent: a call of WorkflowsAPI's, by name. */
export interface Write {
  method: string
  args: unknown[]
}

const EMPTY_OVERVIEW: Overview = { me: { id: '', name: '' }, items: [], bots: [], users: [], recentAssignees: [], scripts: [] }
/** Where it is kept, in a browser; and how many details are kept with it (the latest shown). */
const KEEP_KEY = 'workflows.cache'
const OUTBOX_KEY = 'workflows.outbox'
const KEEP_DETAILS = 40
/** How many of the person's items are fetched ahead, and how many at once. */
const AHEAD = 40
const AHEAD_AT_ONCE = 3
/** Waits between tries to send, once the backend can't be reached. */
const RETRY_MS = [1000, 2000, 5000, 10_000]

export class Cache {
  /** What the backend last said. */
  private server: Omit<Shown, 'unsent' | 'offline'> = { overview: null, details: {} }
  /** The order of each detail as fetched (the fetch's number): an older fetch landing later is dropped. */
  private fetchedAt = new Map<string, number>()
  /** Details fetched before the latest change: shown, and fetched afresh when opened. */
  private stale = new Set<string>()
  /** Changes made here: sent in order; each drawn until a refresh begun after it landed has landed. */
  private outbox: { write: Write; patch: Patch; settled: number | null }[] = []
  private sending = false
  private offline = false
  private tries = 0
  private retryTimer: ReturnType<typeof setTimeout> | null = null
  private shown: Shown = { ...this.server, unsent: 0, offline: false }
  private listeners = new Set<() => void>()
  private errorListeners = new Set<(message: string) => void>()
  /** Every fetch is numbered; `landed` is the number of the last full refresh to land. */
  private seq = 0
  private landed = 0
  private running: Promise<void> | null = null
  private again = false
  private current: string | null = null
  private user: string | null = null
  private ahead = new Set<string>()
  private keepTimer: ReturnType<typeof setTimeout> | null = null
  /** How a write is drawn: set by `withOptimism`. */
  patchFor: (w: Write) => Patch = () => (s) => s

  constructor(
    private api: () => WorkflowsAPI,
    /** Kept across reloads (the phone): local storage. */
    private keep: Storage | null,
  ) {}

  // ---- reading ----

  subscribe = (fn: () => void): (() => void) => {
    this.listeners.add(fn)
    return () => this.listeners.delete(fn)
  }
  get = (): Shown => this.shown
  overview = (): Overview => this.shown.overview ?? EMPTY_OVERVIEW
  /** What went wrong with a change made here, once the backend refused it. */
  onError = (fn: (message: string) => void): (() => void) => {
    this.errorListeners.add(fn)
    return () => this.errorListeners.delete(fn)
  }

  private emit(): void {
    let s: Shown = { ...this.server, unsent: this.outbox.filter((o) => o.settled === null).length, offline: this.offline }
    for (const o of this.outbox) s = o.patch(s)
    this.shown = s
    for (const fn of this.listeners) fn()
    this.scheduleKeep()
  }

  // ---- who, and what is on screen ----

  /** The person signed in: what was kept for them is shown until the backend answers, and what they had not sent is sent. */
  setUser(user: string | null): void {
    if (user === this.user) return
    this.user = user
    this.server = { overview: null, details: {} }
    this.fetchedAt.clear()
    this.stale.clear()
    this.outbox = []
    if (user && this.keep) {
      try {
        const kept = JSON.parse(this.keep.getItem(KEEP_KEY) ?? 'null') as ({ user: string } & Shown) | null
        if (kept?.user === user) {
          this.server = { overview: kept.overview, details: kept.details }
          for (const id of Object.keys(kept.details)) this.stale.add(id)
        }
        const unsent = JSON.parse(this.keep.getItem(OUTBOX_KEY) ?? 'null') as { user: string; writes: Write[] } | null
        if (unsent?.user === user) this.outbox = unsent.writes.map((write) => ({ write, patch: this.patchFor(write), settled: null }))
      } catch {
        // Nothing usable kept.
      }
    }
    this.emit()
    void this.send()
  }

  /** The item on screen: shown from here at once, and fetched if it is not here or may have changed. */
  show(id: string | null): void {
    this.current = id
    if (id && (this.server.details[id] === undefined || this.stale.has(id))) void this.fetchDetail(id)
  }

  // ---- asking the backend ----

  /** The overview and the item on screen, afresh; asked again once more if something changes meanwhile. */
  refresh = (): Promise<void> => {
    // A refresh is a sign the backend is there: what waits to be sent goes now.
    if (this.offline) this.retryNow()
    if (this.running) {
      this.again = true
      return this.running
    }
    this.running = this.run().finally(() => (this.running = null))
    return this.running
  }

  private async run(): Promise<void> {
    try {
      do {
        this.again = false
        const n = ++this.seq
        const id = this.current
        const [overview, detail] = await Promise.all([this.api().overview(), id ? this.api().itemInfoDetail(id) : Promise.resolve(undefined)])
        this.server = { ...this.server, overview }
        // Everything else may have changed: shown as it is, and fetched afresh when opened.
        for (const k of Object.keys(this.server.details)) this.stale.add(k)
        if (id && detail !== undefined) this.landDetail(id, detail, n)
        this.landed = n
        this.outbox = this.outbox.filter((o) => o.settled === null || o.settled >= this.landed)
        this.emit()
        this.fetchAhead()
      } while (this.again)
    } catch {
      // Offline, or the backend is restarting: what is here stays shown, and the next change asks again.
    }
  }

  private landDetail(id: string, detail: ItemInfoDetail | null, n: number): void {
    if ((this.fetchedAt.get(id) ?? 0) > n) return
    this.fetchedAt.set(id, n)
    this.stale.delete(id)
    this.server = { ...this.server, details: { ...this.server.details, [id]: detail } }
  }

  private async fetchDetail(id: string): Promise<void> {
    const n = ++this.seq
    try {
      const detail = await this.api().itemInfoDetail(id)
      this.landDetail(id, detail, n)
      this.emit()
    } catch {
      // Shown as it was; asked again with the next change.
    }
  }

  /** On the phone: the open items in the person's lists (theirs first), not fetched yet, so opening one is instant. */
  private fetchAhead(): void {
    if (!this.keep) return
    const o = this.server.overview
    if (!o) return
    const me = o.me.id
    const mine = (i: Item) => (i.withYou ? 0 : i.assignees.includes(me) ? 1 : i.owner === me ? 2 : 3)
    const todo = o.items
      .filter((i) => !i.closed && mine(i) < 3 && this.server.details[i.id] === undefined && !this.ahead.has(i.id))
      .sort((a, b) => mine(a) - mine(b) || b.since - a.since)
      .slice(0, AHEAD)
    const next = async (): Promise<void> => {
      const item = todo.shift()
      if (!item) return
      this.ahead.add(item.id)
      await this.fetchDetail(item.id)
      this.ahead.delete(item.id)
      return next()
    }
    for (let i = 0; i < AHEAD_AT_ONCE; i++) void next()
  }

  private scheduleKeep(): void {
    if (!this.keep || !this.user || this.keepTimer) return
    this.keepTimer = setTimeout(() => {
      this.keepTimer = null
      if (!this.keep || !this.user) return
      try {
        // What is not sent yet first: it is what matters.
        this.keep.setItem(OUTBOX_KEY, JSON.stringify({ user: this.user, writes: this.outbox.filter((o) => o.settled === null).map((o) => o.write) }))
        if (!this.server.overview) return
        // The details shown lately: the item on screen, then the person's own.
        const ids = [...new Set([this.current, ...this.server.overview.items.filter((i) => i.withYou).map((i) => i.id), ...Object.keys(this.server.details)])].filter(
          (id): id is string => !!id && !!this.server.details[id],
        )
        const details = Object.fromEntries(ids.slice(0, KEEP_DETAILS).map((id) => [id, this.server.details[id]]))
        this.keep.setItem(KEEP_KEY, JSON.stringify({ user: this.user, overview: this.server.overview, details }))
      } catch {
        // Full: it is only a head start.
      }
    }, 500)
  }

  // ---- changes made here ----

  /** Draws the write at once, and sends it after any before it. */
  write(write: Write): void {
    this.outbox.push({ write, patch: this.patchFor(write), settled: null })
    this.emit()
    void this.send()
  }

  /** Sends what waits, in order, one at a time. */
  private async send(): Promise<void> {
    if (this.sending || !this.user) return
    this.sending = true
    let any = false
    try {
      for (let o = this.outbox.find((x) => x.settled === null); o; o = this.outbox.find((x) => x.settled === null)) {
        const call = (this.api() as unknown as Record<string, (...a: unknown[]) => Promise<unknown>>)[o.write.method]
        try {
          await call(...o.write.args)
          any = true
          o.settled = this.seq
          this.offline = false
          this.tries = 0
        } catch (e) {
          if (isOffline(e)) {
            // Kept, drawn, and tried again in a while (or as soon as the backend is heard from).
            this.offline = true
            this.emit()
            this.retryTimer ??= setTimeout(() => this.retryNow(), RETRY_MS[Math.min(this.tries++, RETRY_MS.length - 1)])
            return
          }
          this.outbox = this.outbox.filter((x) => x !== o)
          const message = (e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, '')
          for (const fn of this.errorListeners) fn(describe(o.write, message))
        }
        this.emit()
      }
    } finally {
      this.sending = false
    }
    if (any) void this.refresh()
  }

  private retryNow(): void {
    if (this.retryTimer) clearTimeout(this.retryTimer)
    this.retryTimer = null
    void this.send()
  }

  /** The person signed in, as the overview says (the kept one, before the backend answers). */
  me(): string {
    return this.server.overview?.me.id || this.user || ''
  }

  /** An item not fetched yet, as far as the overview says it: drawn while its detail is fetched. */
  placeholder(id: string): ItemInfoDetail | null {
    const item = this.shown.overview?.items.find((i) => i.id === id)
    if (!item) return null
    const info: ItemInfo = {
      ...infoBase(id, item.parentId, item.createdAt, ''),
      type: item.type,
      owner: item.owner,
      params: { text: item.title, ...(item.open !== null ? { open: item.open } : {}), ...(item.display ? { display: item.display } : {}) },
      resolved: item.closed,
      closed: item.closed,
      summary: item.title,
    }
    return { info, parent: null, pins: [], pinnedIn: [], typeScripts: [], item: { item, parent: null, children: [] }, loading: true }
  }
}

/** What a refused change says: which it was (a message, its text) and why. */
function describe(w: Write, why: string): string {
  if (w.method === 'send' && typeof w.args[2] === 'string' && w.args[2]) return `Not sent (“${w.args[2]}”): ${why}`
  return why
}

// ---- patches ----

const infoBase = (id: string, parentId: string | null, at: number, me: string): ItemInfo => ({
  id,
  parentId,
  via: parentId ? 'comments' : null,
  owner: null,
  type: 'topic',
  params: {},
  createdBy: me,
  createdAt: at,
  resolved: true,
  closed: false,
  resolvedAt: at,
  resolvedBy: me,
  cancelled: false,
  data: null,
  actor: me,
  summary: '',
  message: null,
  nextCheckAt: null,
  lastError: null,
  runs: 0,
  pins: [],
  pinnedIn: [],
})

/** Changes an item wherever it is shown: in the overview's list and in its own detail. */
export const patchItem =
  (id: string, item: (i: Item) => Item, info: (i: ItemInfo) => ItemInfo = (i) => i): Patch =>
  (s) => ({
    ...s,
    overview: s.overview && { ...s.overview, items: s.overview.items.map((i) => (i.id === id ? item(i) : i)) },
    details: Object.fromEntries(
      Object.entries(s.details).map(([k, d]) => [k, d && (k === id ? { ...d, info: info(d.info), item: d.item && { ...d.item, item: item(d.item.item) } } : d)]),
    ),
  })

/** Adds a comment to the end of an item's chat. */
export const patchComment =
  (parentId: string, comment: ItemInfo): Patch =>
  (s) => {
    const d = s.details[parentId]
    if (!d?.item || d.item.children.some((c) => c.id === comment.id)) return s
    return { ...s, details: { ...s.details, [parentId]: { ...d, item: { ...d.item, children: [...d.item.children, comment] } } } }
  }

/** A new topic: in the lists, in its parent's chat, and its own (empty) view, so it opens at once. */
const patchCreate =
  (id: string, title: string, parentId: string | null, me: string, at: number): Patch =>
  (s) => {
    if (!s.overview || s.overview.items.some((i) => i.id === id)) return s
    const item: Item = {
      id, type: 'topic', title, parentId, open: null, closed: false, owner: me, blockers: [], assignees: [me], withYou: true, bot: null,
      since: at, createdAt: at, closedAt: null,
    }
    const info: ItemInfo = { ...infoBase(id, parentId, at, me), owner: me, assignees: [me], params: { text: title }, resolved: false, resolvedAt: null, resolvedBy: null, summary: title }
    const withIt: Shown = {
      ...s,
      overview: { ...s.overview, items: [...s.overview.items, item] },
      details: s.details[id] ? s.details : { ...s.details, [id]: { info, parent: null, pins: [], pinnedIn: [], typeScripts: [], item: { item, parent: null, children: [] } } },
    }
    return parentId ? patchComment(parentId, info)(withIt) : withIt
  }

const compose =
  (...patches: Patch[]): Patch =>
  (s) =>
    patches.reduce((acc, p) => p(acc), s)

const firstLine = (text: string, title: string) => [title, ...text.split('\n').slice(1)].join('\n')
/** A new id, in a secure context or not (the phone over plain http has no randomUUID). */
export const uuid = (): string =>
  typeof crypto.randomUUID === 'function'
    ? crypto.randomUUID()
    : '10000000-1000-4000-8000-100000000000'.replace(/[018]/g, (c) => (Number(c) ^ (crypto.getRandomValues(new Uint8Array(1))[0] & (15 >> (Number(c) / 4)))).toString(16))

/** How each change made here is drawn, by its call. */
function patchFor(w: Write, me: string): Patch {
  const a = w.args
  const at = Date.now()
  switch (w.method) {
    case 'send': {
      const [itemId, presetId, text, commentId] = a as [string, string, string, string | undefined]
      const to = presetId.startsWith('assign:') ? presetId.slice('assign:'.length) : null
      const said = presetId === 'note' || to ? text.trim() : ''
      const comment = { ...infoBase(commentId ?? `pending:${at}`, itemId, at, me), params: { text: said }, summary: said.split('\n')[0] }
      return compose(
        said ? patchComment(itemId, comment) : (s) => s,
        // Handed on: to them, and off whoever sent it.
        to ? patchItem(itemId, (i) => ({ ...i, assignees: [...i.assignees.filter((x) => x !== me && x !== to), to], withYou: false })) : (s) => s,
      )
    }
    case 'create': {
      const [title, parentId, id] = a as [string, string | null, string]
      return patchCreate(id, title, parentId, me, at)
    }
    case 'close': {
      const [id] = a as [string]
      return patchItem(
        id,
        (i) => ({ ...i, closed: true, open: i.type === 'topic' ? i.open : false, assignees: [], owner: null, withYou: false, closedAt: at }),
        (i) => ({ ...i, closed: true, owner: null, assignees: [] }),
      )
    }
    case 'reopen': {
      const [id] = a as [string]
      return patchItem(
        id,
        // Whoever reopens it has it (engine `reopen`).
        (i) => ({ ...i, closed: false, open: i.type === 'topic' ? i.open : true, closedAt: null, owner: me, assignees: [me], withYou: !i.blockers.length }),
        (i) => ({ ...i, closed: false, owner: me, assignees: [me] }),
      )
    }
    case 'assign': {
      const [id, who, on] = a as [string, string, boolean]
      return patchItem(id, (i) => ({
        ...i,
        assignees: on ? [...i.assignees.filter((x) => x !== who), who] : i.assignees.filter((x) => x !== who),
        withYou: who === me ? on && !i.closed && !i.blockers.length : i.withYou,
      }))
    }
    case 'setOwner': {
      const [id, owner] = a as [string, string | null]
      return patchItem(id, (i) => ({ ...i, owner }), (i) => ({ ...i, owner }))
    }
    case 'rename': {
      const [id, title] = a as [string, string]
      return patchItem(
        id,
        (i) => ({ ...i, title }),
        (i) => ({ ...i, summary: firstLine(i.summary, title), params: { ...i.params, text: firstLine(String(i.params.text ?? ''), title) } }),
      )
    }
    case 'setValue': {
      const [id, key, value] = a as [string, string, unknown]
      if (key !== 'display') return (s) => s
      return patchItem(id, (i) => ({ ...i, display: String(value) }), (i) => ({ ...i, params: { ...i.params, display: value } }))
    }
    default:
      return (s) => s
  }
}

/** The calls that go through the outbox: drawn at once, answered at once, sent behind. */
const OUTBOX = ['send', 'create', 'close', 'reopen', 'assign', 'setOwner', 'rename', 'setValue', 'pin'] as const

/**
 * WorkflowsAPI with its common changes drawn at once and sent behind (through `cache`'s outbox): sending in a chat,
 * making a topic, closing and reopening, assigning, owning, renaming, picking a view, pinning. Each answers at once
 * (`create` with the id it will have); what the backend refuses comes back through `cache.onError`. Everything else
 * is as it was.
 */
export function withOptimism(api: WorkflowsAPI, cache: Cache): WorkflowsAPI {
  cache.patchFor = (w) => patchFor(w, cache.me())
  const queued = Object.fromEntries(
    OUTBOX.map((method) => [
      method,
      async (...args: unknown[]) => {
        // What it adds has an id chosen here, so it is drawn as it will be, and sending it again adds nothing.
        if (method === 'send' && !args[3]) args = [args[0], args[1], args[2], uuid()]
        if (method === 'create' && !args[2]) args = [args[0], args[1] ?? null, uuid()]
        cache.write({ method, args })
        if (method === 'create') return args[2]
      },
    ]),
  )
  return {
    ...api,
    ...(queued as unknown as Partial<WorkflowsAPI>),
    setUser: (u) => {
      api.setUser(u)
      cache.setUser(u)
    },
  }
}
