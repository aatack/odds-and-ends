// `Egdb`: one branch of one server, as seen from the browser. It holds the
// entity cache in a Jotai atom, loads into it, and writes through it. The
// React hooks are thin readers of what is here; nothing in this file needs
// React.

import { atom, createStore, type Atom, type PrimitiveAtom } from 'jotai/vanilla'
import {
  applyOps,
  emptyCache,
  entitiesOf,
  groupsOf,
  loadedFromWalk,
  mergeLoaded,
  viewOf,
  type CacheState,
  type EntityView,
  type GroupKey,
  type Loaded,
  type PendingWrite,
} from './cache.js'
import type { EgdbClient } from './client.js'
import type { BranchId, Direction, EntityId, Filter, FindOptions, FindOrder, LinkPair, Op, Path, WriteResult } from './types.js'
import { pathKey, type WalkQuery } from './walk.js'

export type JotaiStore = ReturnType<typeof createStore>

export interface EgdbOptions {
  client: EgdbClient
  branch: BranchId
  /** Who writes: the `author` of every batch. */
  author: string
  /** The Jotai store to keep the atoms in. Default: a new one. */
  store?: JotaiStore
  /** Nodes per preload walk. Default 200. */
  preloadSteps?: number
  /** Called with every failed load or write (a write's promise rejects too). */
  onError?: (error: unknown) => void
}

/** One `useFind` query's results so far. */
export interface FindState {
  ids: EntityId[]
  next: string | null
  done: boolean
  loading: boolean
  error?: unknown
  /** The write counter when the first page was asked for. */
  since: number
}

/** A filter that matches nothing: a preload wants the cache, not rows. */
const NO_ROWS: Filter = { not_null: { key: '\u0000' } }

export class Egdb {
  readonly client: EgdbClient
  readonly branch: BranchId
  readonly author: string
  readonly store: JotaiStore
  /** The whole cache: confirmed records and pending writes. */
  readonly cacheAtom: PrimitiveAtom<CacheState>
  /** The cache as reads see it: confirmed records with pending writes on top. */
  readonly viewAtom: Atom<EntityView>
  /** The results of each find query, by query key. */
  readonly findsAtom: PrimitiveAtom<ReadonlyMap<string, FindState>>
  /** The last load or write that failed. */
  readonly errorAtom: PrimitiveAtom<unknown>
  /** How many of this client's batches can be undone and redone. */
  readonly historyAtom: PrimitiveAtom<{ undo: number; redo: number }>

  private readonly preloadSteps: number
  private readonly onError?: (error: unknown) => void
  private writes = 0
  /** Per entity, the write counter of the last confirmed write to it. */
  private readonly stamps = new Map<EntityId, number>()
  private readonly preloads = new Map<string, 'running' | 'done'>()
  private queued: { ids: Set<EntityId>; groups: Map<GroupKey, LinkPair> } | null = null
  private pendingId = 0
  /** This client's confirmed batches, oldest first: the ops sent and their sequence IDs. */
  private done: { ops: Op[]; first: number; last: number }[] = []
  /** Batches that undo removed, newest last: redo sends them again. */
  private undone: Op[][] = []
  /** Writes not yet confirmed: undo waits for them, so it removes the right batch. */
  private inflight = new Set<Promise<unknown>>()

  constructor(options: EgdbOptions) {
    this.client = options.client
    this.branch = options.branch
    this.author = options.author
    this.store = options.store ?? createStore()
    this.preloadSteps = options.preloadSteps ?? 200
    this.onError = options.onError
    this.cacheAtom = atom<CacheState>(emptyCache)
    this.viewAtom = atom((get) => viewOf(get(this.cacheAtom)))
    this.findsAtom = atom<ReadonlyMap<string, FindState>>(new Map())
    this.errorAtom = atom<unknown>(undefined)
    this.historyAtom = atom({ undo: 0, redo: 0 })
  }

  /** The cache as reads see it now. */
  view(): EntityView {
    return this.store.get(this.viewAtom)
  }

  private fail(error: unknown) {
    this.store.set(this.errorAtom, error)
    this.onError?.(error)
  }

  /** Merge a load that started at write counter `since`: skip what a later write changed. */
  private merge(loaded: Loaded, since: number) {
    const skip = (id: EntityId) => (this.stamps.get(id) ?? 0) > since
    this.store.set(this.cacheAtom, (state) => ({ ...state, base: mergeLoaded(state.base, loaded, skip) }))
  }

  // --- loading ---------------------------------------------------------------

  /**
   * Load a stretch of a walk into the cache: `steps` nodes from `from`, in
   * `direction`, with the query's follow map, descend filter and collapsed
   * rows. One call per (path, direction, query) until a write lands.
   */
  async preloadWalk(root: Path, query: WalkQuery, from: Path, direction: 'forward' | 'backward') {
    // `include` only picks rows; a preload loads the same entities with or without it.
    const key = JSON.stringify([pathKey(root), queryKey({ ...query, include: undefined }), pathKey(from), direction])
    if (this.preloads.has(key)) return
    this.preloads.set(key, 'running')
    // The server never climbs above `start[0]`, so the start begins at the root's last entity.
    const cut = root.length - 1
    const since = this.writes
    try {
      const page = await this.client.walk(this.branch, {
        start: from.slice(cut),
        follow: query.follow,
        descend: query.descend,
        include: NO_ROWS,
        collapsed: (query.collapsed ?? [])
          .filter((p) => p.length >= root.length && root.every((id, i) => p[i] === id))
          .map((p) => p.slice(cut)),
        options: { steps: this.preloadSteps, return_cache: true, direction },
      })
      if (page.cache) this.merge(loadedFromWalk(page.cache), since)
      if (this.preloads.get(key) === 'running') this.preloads.set(key, 'done')
    } catch (error) {
      this.preloads.delete(key)
      this.fail(error)
    }
  }

  /**
   * Load entities' values (and, with `groups`, those link groups) into the
   * cache. Calls in the same tick share one request.
   */
  load(ids: readonly EntityId[], groups: readonly LinkPair[] = []): Promise<void> {
    if (!this.queued) {
      const queued = { ids: new Set<EntityId>(), groups: new Map<GroupKey, LinkPair>() }
      this.queued = queued
      this.flush = Promise.resolve().then(async () => {
        this.queued = null
        await this.fetch([...queued.ids], [...queued.groups.values()])
      })
    }
    for (const id of ids) this.queued.ids.add(id)
    for (const pair of groups) this.queued.groups.set(pair.join('\u0000'), pair)
    return this.flush
  }
  private flush: Promise<void> = Promise.resolve()

  private async fetch(ids: EntityId[], groups: LinkPair[]) {
    if (ids.length === 0) return
    const since = this.writes
    try {
      const [values, links] = await Promise.all([
        this.client.values(this.branch, ids),
        groups.length > 0 ? this.client.links(this.branch, ids, groups) : undefined,
      ])
      this.merge({ values, links }, since)
    } catch (error) {
      this.fail(error)
    }
  }

  // --- writing ---------------------------------------------------------------

  /**
   * Write a batch. It shows in every view at once; the promise settles when
   * the server has it. If the server refuses it, it leaves the views again.
   */
  write(ops: Op | readonly Op[]): Promise<WriteResult> {
    const batch = Array.isArray(ops) ? [...ops] : [ops as Op]
    // A new change makes the undone batches stale.
    this.undone = []
    return this.track(this.send(batch))
  }

  private track<T>(promise: Promise<T>): Promise<T> {
    this.inflight.add(promise)
    const forget = () => this.inflight.delete(promise)
    promise.then(forget, forget)
    this.syncHistory()
    return promise
  }

  private syncHistory() {
    this.store.set(this.historyAtom, { undo: this.done.length, redo: this.undone.length })
  }

  private async send(batch: Op[]): Promise<WriteResult> {
    const pending: PendingWrite = { id: ++this.pendingId, ops: batch, timestamp: Date.now() }
    const drop = (state: CacheState) => state.pending.filter((p) => p !== pending)
    this.store.set(this.cacheAtom, (state) => ({ ...state, pending: [...state.pending, pending] }))
    let result: WriteResult
    try {
      result = await this.client.write({ branch: this.branch, author: this.author, ops: batch })
    } catch (error) {
      this.store.set(this.cacheAtom, (state) => ({ ...state, pending: drop(state) }))
      this.fail(error)
      throw error
    }
    if (result.first_seq <= result.last_seq) {
      this.done.push({ ops: batch, first: result.first_seq, last: result.last_seq })
      this.syncHistory()
    }
    await this.readBack(batch, () => drop, pending.timestamp)
    return result
  }

  /**
   * After the server changed the entities of `batch`: read them again and
   * merge them in, applying `update` to the pending list in the same step.
   */
  private async readBack(
    batch: Op[],
    update: () => (state: CacheState) => readonly PendingWrite[],
    timestamp?: number,
  ) {
    // Loads that started before now may hold the old state of these entities.
    const since = ++this.writes
    const ids = entitiesOf(batch)
    for (const id of ids) this.stamps.set(id, since)
    this.preloads.clear()
    const drop = update()
    // Read back what the server made of it (middleware may have changed it),
    // then swap the guess for the answer in one update, so nothing flickers.
    try {
      const pairs = this.knownGroups(ids, batch)
      const [values, links] = await Promise.all([
        this.client.values(this.branch, ids),
        pairs.length > 0 ? this.client.links(this.branch, ids, pairs) : undefined,
      ])
      const skip = (id: EntityId) => (this.stamps.get(id) ?? 0) > since
      this.store.set(this.cacheAtom, (state) => ({
        base: mergeLoaded(state.base, { values, links }, skip),
        pending: drop(state),
      }))
    } catch (error) {
      // The write is in; keep the guess as the cache's state.
      this.store.set(this.cacheAtom, (state) => ({
        base: timestamp === undefined ? state.base : applyOps(state.base, batch, timestamp),
        pending: drop(state),
      }))
      this.fail(error)
    }
  }

  /** The link groups worth reading back after a write: the ones it touched and the ones loaded. */
  private knownGroups(ids: EntityId[], ops: Op[]): LinkPair[] {
    const keys = new Set<GroupKey>()
    for (const set of groupsOf(ops).values()) for (const key of set) keys.add(key)
    const base = this.store.get(this.cacheAtom).base
    for (const id of ids) for (const key of Object.keys(base.get(id)?.links ?? {})) keys.add(key)
    return [...keys].map((key) => {
      const [linkType, direction] = key.split('\u0000') as [string, Direction]
      return [linkType, direction] as const
    })
  }

  /**
   * Undo this author's newest batch on the server, then read back what it
   * touched. It is not optimistic: the browser does not know the values the
   * batch replaced. When the batch is one this client wrote, `redo` can send
   * it again.
   */
  async undo() {
    await Promise.allSettled([...this.inflight])
    const outcome = await this.client.undo(this.branch, this.author)
    if (!outcome) return outcome
    const at = this.done.findIndex((d) => d.first === outcome.removed[0])
    if (at < 0) {
      // Another client's batch, with the same author: what it touched is not known here.
      await this.refresh()
      return outcome
    }
    const [batch] = this.done.splice(at, 1)
    this.undone.push(batch!.ops)
    this.syncHistory()
    await this.readBack(batch!.ops, () => (state) => state.pending)
    return outcome
  }

  /** Send the newest undone batch again, as a new write (optimistic, like any write). */
  redo(): Promise<WriteResult> | undefined {
    const ops = this.undone.pop()
    if (!ops) return undefined
    const sent = this.send(ops)
    // Refused: it can be redone again later.
    sent.catch(() => {
      this.undone.push(ops)
      this.syncHistory()
    })
    return this.track(sent)
  }

  /** Forget what preloads have done and read every cached entity again. */
  async refresh() {
    this.preloads.clear()
    const since = ++this.writes
    const base = this.store.get(this.cacheAtom).base
    const ids = [...base.keys()]
    for (const id of ids) this.stamps.set(id, since)
    const keys = new Set<GroupKey>()
    for (const record of base.values()) for (const key of Object.keys(record.links)) keys.add(key)
    const pairs = [...keys].map((key) => key.split('\u0000') as unknown as LinkPair)
    if (ids.length === 0) return
    try {
      const [values, links] = await Promise.all([
        this.client.values(this.branch, ids),
        pairs.length > 0 ? this.client.links(this.branch, ids, pairs) : undefined,
      ])
      this.merge({ values, links }, since)
      this.store.set(this.findsAtom, new Map())
    } catch (error) {
      this.fail(error)
    }
  }

  // --- find --------------------------------------------------------------------

  /** The write counter now: a find compares it with the stamps to find local changes. */
  get counter(): number {
    return this.writes
  }

  /** Entities written since counter `since`, confirmed or pending. */
  writtenSince(since: number): Set<EntityId> {
    const out = new Set<EntityId>()
    for (const [id, stamp] of this.stamps) if (stamp > since) out.add(id)
    for (const pending of this.store.get(this.cacheAtom).pending) for (const id of entitiesOf(pending.ops)) out.add(id)
    return out
  }

  /** Make sure the find query `key` has at least `want` rows, or has them all. */
  async findMore(key: string, filter: Filter, options: FindQueryOptions, want: number) {
    for (;;) {
      const state = this.store.get(this.findsAtom).get(key)
      if (state && (state.loading || state.done || state.ids.length >= want || state.error)) return
      const since = state?.since ?? this.writes
      const update = (patch: Partial<FindState>) =>
        this.store.set(this.findsAtom, (all) => {
          const next = new Map(all)
          const now = next.get(key) ?? { ids: [], next: null, done: false, loading: false, since }
          next.set(key, { ...now, ...patch })
          return next
        })
      update({ loading: true })
      const loadedSince = this.writes
      try {
        const page = await this.client.find(this.branch, filter, {
          ...options,
          limit: options.pageSize ?? 200,
          cursor: state?.next ?? undefined,
          values: true,
        })
        const values = Object.fromEntries(page.rows.map((row) => [row.id, row.values]))
        this.merge({ values }, loadedSince)
        update({
          ids: [...(state?.ids ?? []), ...page.rows.map((row) => row.id)],
          next: page.next,
          done: page.next === null,
          loading: false,
        })
      } catch (error) {
        update({ loading: false, error })
        this.fail(error)
        return
      }
    }
  }

  /** Drop a find query's results, so that the next read asks the server again. */
  resetFind(key: string) {
    this.store.set(this.findsAtom, (all) => {
      const next = new Map(all)
      next.delete(key)
      return next
    })
  }
}

export interface FindQueryOptions {
  order?: FindOrder
  include_archived?: boolean
  /** Rows per request to the server. Default 200. */
  pageSize?: number
  scan_limit?: FindOptions['scan_limit']
}

/** A stable key for a walk query: the same query gives the same key. */
export function queryKey(query: WalkQuery): string {
  return stableJson([query.follow, query.descend ?? null, query.include ?? null, (query.collapsed ?? []).map(pathKey).sort()])
}

/** JSON with object keys sorted, so that equal values give equal strings. */
export function stableJson(value: unknown): string {
  return JSON.stringify(value, (_, v) =>
    v && typeof v === 'object' && !Array.isArray(v)
      ? Object.fromEntries(Object.entries(v).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)))
      : v,
  )
}
