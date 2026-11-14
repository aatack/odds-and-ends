// What the app shows, kept here rather than asked for each time it is drawn: the overview and each item's detail,
// as the backend last said them, with the changes made here and not yet answered laid over them. A view draws
// whatever is here at once (nothing yet: it says it is loading), and the backend is asked in the background:
// after a change it says, an item opened, and every change made here. On the phone (a browser) it is also kept in
// local storage, so the app opens onto what it last showed, and the items waiting on the person are fetched ahead,
// so opening one is instant.
//
// A change made here (`optimistic`, on api()'s calls) is drawn at once, and stays drawn until the backend has
// answered it and a refresh begun after that has landed; one that fails is taken off.

import type { Item, ItemInfo, ItemInfoDetail, Overview, WorkflowsAPI } from '../../../shared/types'

export interface Shown {
  overview: Overview | null
  /** Each item's detail: undefined not fetched yet, null not there. */
  details: Record<string, ItemInfoDetail | null | undefined>
}

type Patch = (s: Shown) => Shown

const EMPTY_OVERVIEW: Overview = { me: { id: '', name: '' }, items: [], bots: [], users: [], recentAssignees: [], scripts: [] }
/** Where it is kept, in a browser; and how many details are kept with it (the latest shown). */
const KEEP_KEY = 'workflows.cache'
const KEEP_DETAILS = 15
/** How many of the person's items are fetched ahead, and how many at once. */
const AHEAD = 12
const AHEAD_AT_ONCE = 3

export class Cache {
  /** What the backend last said. */
  private server: Shown = { overview: null, details: {} }
  /** The order of each detail as fetched (the fetch's number): an older fetch landing later is dropped. */
  private fetchedAt = new Map<string, number>()
  /** Details fetched before the latest change: shown, and fetched afresh when opened. */
  private stale = new Set<string>()
  private pending = new Set<{ patch: Patch; settled: number | null }>()
  private shown: Shown = this.server
  private listeners = new Set<() => void>()
  /** Every fetch is numbered; `landed` is the number of the last full refresh to land. */
  private seq = 0
  private landed = 0
  private running = false
  private again = false
  private current: string | null = null
  private user: string | null = null
  private ahead = new Set<string>()
  private keepTimer: ReturnType<typeof setTimeout> | null = null

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

  private emit(): void {
    let s = this.server
    for (const p of this.pending) s = p.patch(s)
    this.shown = s
    for (const fn of this.listeners) fn()
    this.scheduleKeep()
  }

  // ---- who, and what is on screen ----

  /** The person signed in: what was kept for them is shown until the backend answers. */
  setUser(user: string | null): void {
    if (user === this.user) return
    this.user = user
    this.server = { overview: null, details: {} }
    this.fetchedAt.clear()
    this.stale.clear()
    this.pending.clear()
    if (user && this.keep) {
      try {
        const kept = JSON.parse(this.keep.getItem(KEEP_KEY) ?? 'null') as ({ user: string } & Shown) | null
        if (kept?.user === user) {
          this.server = { overview: kept.overview, details: kept.details }
          for (const id of Object.keys(kept.details)) this.stale.add(id)
        }
      } catch {
        // Nothing usable kept.
      }
    }
    this.emit()
  }

  /** The item on screen: shown from here at once, and fetched if it is not here or may have changed. */
  show(id: string | null): void {
    this.current = id
    if (id && (this.server.details[id] === undefined || this.stale.has(id))) void this.fetchDetail(id)
  }

  // ---- asking the backend ----

  /** The overview and the item on screen, afresh; asked again once more if something changes meanwhile. */
  refresh = async (): Promise<void> => {
    if (this.running) return void (this.again = true)
    this.running = true
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
        for (const p of this.pending) if (p.settled !== null && p.settled < this.landed) this.pending.delete(p)
        this.emit()
        this.fetchAhead()
      } while (this.again)
    } catch {
      // Offline, or the backend is restarting: what is here stays shown, and the next change asks again.
    } finally {
      this.running = false
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

  /** On the phone: the open items waiting on the person, not fetched yet, so opening one is instant. */
  private fetchAhead(): void {
    if (!this.keep) return
    const o = this.server.overview
    if (!o) return
    const todo = o.items.filter((i) => i.withYou && !i.closed && this.server.details[i.id] === undefined && !this.ahead.has(i.id)).slice(0, AHEAD)
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
      if (!this.keep || !this.user || !this.server.overview) return
      // The details shown lately: the item on screen, then the person's own.
      const ids = [...new Set([this.current, ...this.server.overview.items.filter((i) => i.withYou).map((i) => i.id)])].filter(
        (id): id is string => !!id && !!this.server.details[id],
      )
      const details = Object.fromEntries(ids.slice(0, KEEP_DETAILS).map((id) => [id, this.server.details[id]]))
      try {
        this.keep.setItem(KEEP_KEY, JSON.stringify({ user: this.user, overview: this.server.overview, details }))
      } catch {
        // Full: it is only a head start.
      }
    }, 1000)
  }

  // ---- changes made here ----

  /** Draws `patch` at once, while `call` goes to the backend; then refreshes, and the patch goes once that lands. */
  async optimistic<T>(patch: Patch, call: () => Promise<T>): Promise<T> {
    const p = { patch, settled: null as number | null }
    this.pending.add(p)
    this.emit()
    try {
      const out = await call()
      p.settled = this.seq
      void this.refresh()
      return out
    } catch (e) {
      this.pending.delete(p)
      this.emit()
      throw e
    }
  }

  /** The person signed in, as the overview says (the kept one, before the backend answers). */
  me(): string {
    return this.server.overview?.me.id || this.user || ''
  }
}

// ---- patches ----

/** Changes an item wherever it is shown: in the overview's list and in its own detail. */
export const patchItem =
  (id: string, item: (i: Item) => Item, info: (i: ItemInfo) => ItemInfo = (i) => i): Patch =>
  (s) => ({
    overview: s.overview && { ...s.overview, items: s.overview.items.map((i) => (i.id === id ? item(i) : i)) },
    details: Object.fromEntries(
      Object.entries(s.details).map(([k, d]) => [
        k,
        d && (k === id ? { ...d, info: info(d.info), item: d.item && { ...d.item, item: item(d.item.item) } } : d),
      ]),
    ),
  })

/** Adds a comment to the end of an item's chat. */
export const patchComment =
  (parentId: string, comment: ItemInfo): Patch =>
  (s) => {
    const d = s.details[parentId]
    if (!d?.item) return s
    return { ...s, details: { ...s.details, [parentId]: { ...d, item: { ...d.item, children: [...d.item.children, comment] } } } }
  }

export const compose =
  (...patches: Patch[]): Patch =>
  (s) =>
    patches.reduce((acc, p) => p(acc), s)

/** A comment as it will be, until the backend has it. */
export function pendingComment(parentId: string, text: string, me: string): ItemInfo {
  const now = Date.now()
  return {
    id: `pending:${now}:${Math.random().toString(36).slice(2)}`,
    parentId,
    via: 'comments',
    owner: null,
    type: 'topic',
    params: { text },
    createdBy: me,
    createdAt: now,
    resolved: true,
    closed: false,
    resolvedAt: now,
    resolvedBy: me,
    cancelled: false,
    data: null,
    actor: me,
    summary: text.split('\n')[0],
    message: null,
    nextCheckAt: null,
    lastError: null,
    runs: 0,
    pins: [],
    pinnedIn: [],
  }
}

const firstLine = (text: string, title: string) => [title, ...text.split('\n').slice(1)].join('\n')

/**
 * WorkflowsAPI with its common changes drawn at once (through `cache`): sending in a chat, closing and reopening,
 * assigning, renaming, picking a view. Everything else is as it was; the refresh after each brings in the rest.
 */
export function withOptimism(api: WorkflowsAPI, cache: Cache): WorkflowsAPI {
  const me = () => cache.me()
  return {
    ...api,
    setUser: (u) => {
      api.setUser(u)
      cache.setUser(u)
    },
    send: (itemId, presetId, text) => {
      const to = presetId.startsWith('assign:') ? presetId.slice('assign:'.length) : null
      const said = presetId === 'note' || to ? text.trim() : ''
      return cache.optimistic(
        compose(
          said ? patchComment(itemId, pendingComment(itemId, said, me())) : (s) => s,
          // Handed on: to them, and off whoever sent it.
          to ? patchItem(itemId, (i) => ({ ...i, assignees: [...i.assignees.filter((a) => a !== me() && a !== to), to], withYou: false })) : (s) => s,
        ),
        () => api.send(itemId, presetId, text),
      )
    },
    close: (id) =>
      cache.optimistic(
        patchItem(
          id,
          (i) => ({ ...i, closed: true, open: i.open === null ? null : false, assignees: [], owner: null, withYou: false, closedAt: Date.now() }),
          (i) => ({ ...i, closed: true, owner: null }),
        ),
        () => api.close(id),
      ),
    reopen: (id) =>
      cache.optimistic(
        patchItem(
          id,
          (i) => ({ ...i, closed: false, open: i.open === null ? null : true, closedAt: null }),
          (i) => ({ ...i, closed: false }),
        ),
        () => api.reopen(id),
      ),
    assign: (id, who, on) =>
      cache.optimistic(
        patchItem(id, (i) => ({
          ...i,
          assignees: on ? [...i.assignees.filter((a) => a !== who), who] : i.assignees.filter((a) => a !== who),
          withYou: who === me() ? on && !i.closed && !i.blockers.length : i.withYou,
        })),
        () => api.assign(id, who, on),
      ),
    rename: (id, title) =>
      cache.optimistic(
        patchItem(
          id,
          (i) => ({ ...i, title }),
          (i) => ({ ...i, summary: firstLine(i.summary, title), params: { ...i.params, text: firstLine(String(i.params.text ?? ''), title) } }),
        ),
        () => api.rename(id, title),
      ),
    setValue: (id, key, value) =>
      cache.optimistic(
        key === 'display' ? patchItem(id, (i) => i, (i) => ({ ...i, params: { ...i.params, display: value } })) : (s) => s,
        () => api.setValue(id, key, value),
      ),
  }
}
