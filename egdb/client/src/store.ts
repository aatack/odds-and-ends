// `Egdb`: one branch of one server, as seen from the browser. It holds the
// entity cache in a Jotai atom, loads into it, and writes through it. The
// React hooks are thin readers of what is here; nothing in this file needs
// React.

import { atom, createStore, type Atom, type PrimitiveAtom } from 'jotai/vanilla'
import {
  applyChanges,
  applyOps,
  emptyCache,
  entitiesOf,
  groupsOf,
  inverseOf,
  loadedFromWalk,
  mergeLoaded,
  pinTimestamps,
  viewOf,
  type CacheState,
  type EntityView,
  type GroupKey,
  type Loaded,
  type PendingWrite,
} from './cache.js'
import type { EgdbClient } from './client.js'
import {
  UNDO_WINDOW_MS,
  type BranchId,
  type Direction,
  type EntityId,
  type Filter,
  type FindOptions,
  type FindOrder,
  type LinkPair,
  type Op,
  type Path,
  type UndoOutcome,
  type WriteResult,
} from './types.js'
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

/** One of this client's batches, as undo and redo keep it. */
interface Batch {
  ops: Op[]
  /** Ops that put back what the batch replaced, as far as the cache knew: undo shows them at once. */
  inverse: Op[]
  /** The local clock when it was written. */
  at: number
  /** Settles with the server's answer (its seqs and timestamp), or rejects if the write failed. */
  sent: Promise<WriteResult>
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
  readonly undoAtom: PrimitiveAtom<{ undo: number; redo: number }>
  /** The change feed's cursor (`follow`): the last seq applied, or null while not following. `useLog` reads again as it moves. */
  readonly headAtom: PrimitiveAtom<number | null>

  private readonly preloadSteps: number
  private readonly onError?: (error: unknown) => void
  private writes = 0
  /** Per entity, the write counter of the last confirmed write to it. */
  private readonly stamps = new Map<EntityId, number>()
  private readonly preloads = new Map<string, 'running' | 'done'>()
  private queued: { ids: Set<EntityId>; groups: Map<GroupKey, LinkPair> } | null = null
  private pendingId = 0
  /** This client's batches that undo can reach, oldest first, from the moment they are written. */
  private done: Batch[] = []
  /** Batches that undo took off, newest last: redo sends them again. */
  private undone: Batch[] = []
  /** Writes not yet confirmed: an undo waits for the ones before it, so it removes the right batch. */
  private inflight = new Set<Promise<unknown>>()
  /** The undos sent to the server, in order: a write waits for them, so it cannot land before them. */
  private undoing: Promise<unknown> = Promise.resolve()

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
    this.undoAtom = atom({ undo: 0, redo: 0 })
    this.headAtom = atom<number | null>(null)
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
    // The server returns only the entities on the paths it emits, so it emits them all.
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
        collapsed: (query.collapsed ?? [])
          .filter((p) => p.length >= root.length && root.every((id, i) => p[i] === id))
          .map((p) => p.slice(cut)),
        // A filter hides most nodes, so the browser steps over many per row: load more at once.
        options: { steps: query.include ? this.preloadSteps * 10 : this.preloadSteps, direction },
      })
      this.merge(loadedFromWalk(page.cache), since)
      // The linked entities that the walk did not reach (a child type that no
      // rule follows, or past the end of the page) are not loaded: load their
      // values, so that the browser can tell which ones to follow.
      const view = this.view()
      const unknown = new Set<EntityId>()
      for (const groups of Object.values(page.cache.links)) {
        for (const group of groups) {
          for (const link of group.links) {
            const other = group.direction === 'outbound' ? link.destination : link.source
            if (!view.get(other)?.values) unknown.add(other)
          }
        }
      }
      if (unknown.size > 0) await this.load([...unknown])
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
    this.syncUndo()
    return promise
  }

  private syncUndo() {
    this.store.set(this.undoAtom, { undo: this.done.length, redo: this.undone.length })
  }

  /**
   * Show `batch` at once and send it. `wire` gives the ops to send, when they
   * differ from the ones shown (a redo's links, fixed at the server's time).
   */
  private async send(batch: Op[], wire: () => Promise<Op[]> = async () => batch): Promise<WriteResult> {
    const pending: PendingWrite = { id: ++this.pendingId, ops: batch, timestamp: Date.now() }
    const drop = (state: CacheState) => state.pending.filter((p) => p !== pending)
    const inverse = inverseOf(this.view(), batch)
    this.store.set(this.cacheAtom, (state) => ({ ...state, pending: [...state.pending, pending] }))
    // After the undos already asked for: they must reach the server first.
    const sent = this.undoing.then(async () =>
      this.client.write({ branch: this.branch, author: this.author, ops: await wire() }),
    )
    const entry: Batch = { ops: batch, inverse, at: pending.timestamp, sent }
    // Undoable at once: an undo before the server answers waits for the answer.
    this.done.push(entry)
    this.syncUndo()
    const forget = () => {
      const at = this.done.indexOf(entry)
      if (at >= 0) this.done.splice(at, 1)
      this.syncUndo()
    }
    let result: WriteResult
    try {
      result = await sent
    } catch (error) {
      forget()
      this.store.set(this.cacheAtom, (state) => ({ ...state, pending: drop(state) }))
      this.fail(error)
      throw error
    }
    // No events of its own (the server wrote it some other way): nothing to undo.
    if (result.first_seq > result.last_seq) forget()
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
   * Undo this client's newest batch. It is optimistic: the views show what the
   * batch replaced (as the cache knew it when the batch was written) at once,
   * and `redo` can send it again at once. Then, after the writes before it,
   * the server removes that batch, named by its first seq, so a newer batch of
   * the same author from elsewhere is never removed instead. If the server
   * refuses, the batch shows again and the promise rejects. `null`: nothing to
   * undo (none, too old, or a write that never landed).
   */
  undo(): Promise<UndoOutcome | null> {
    const entry = this.done[this.done.length - 1]
    // The server would refuse it; don't show an undo that cannot happen.
    if (!entry || Date.now() - entry.at >= UNDO_WINDOW_MS) return Promise.resolve(null)
    this.done.pop()
    this.undone.push(entry)
    this.syncUndo()
    const overlay: PendingWrite = { id: ++this.pendingId, ops: entry.inverse, timestamp: Date.now() }
    const drop = (state: CacheState) => state.pending.filter((p) => p !== overlay)
    this.store.set(this.cacheAtom, (state) => ({ ...state, pending: [...state.pending, overlay] }))
    const unshow = () => this.store.set(this.cacheAtom, (state) => ({ ...state, pending: drop(state) }))
    const notUndone = () => {
      const at = this.undone.indexOf(entry)
      if (at >= 0) this.undone.splice(at, 1)
      this.syncUndo()
    }
    const before = [...this.inflight]
    const run = this.undoing.then(async () => {
      let result: WriteResult
      try {
        result = await entry.sent
      } catch {
        // It never landed; its own failure already took it off the views.
        notUndone()
        unshow()
        return null
      }
      if (result.first_seq > result.last_seq) {
        notUndone()
        unshow()
        return null
      }
      await Promise.allSettled(before)
      let outcome: UndoOutcome | null
      try {
        outcome = await this.client.undo(this.branch, this.author, result.first_seq)
      } catch (error) {
        notUndone()
        unshow()
        // Back in its place among the batches undo can reach.
        const at = this.done.findIndex((d) => d.at > entry.at)
        this.done.splice(at < 0 ? this.done.length : at, 0, entry)
        this.syncUndo()
        this.fail(error)
        throw error
      }
      if (!outcome) {
        // Too old for the server after all: nothing changed.
        notUndone()
        unshow()
        return null
      }
      await this.readBack(entry.ops, () => drop)
      return outcome
    })
    this.undoing = run.catch(() => undefined)
    return run
  }

  /**
   * Send the newest undone batch again, as a new write (optimistic, like any
   * write). Its `timestamp` links keep the sort the server gave them, so they
   * go back to their places.
   */
  redo(): Promise<WriteResult> | undefined {
    const entry = this.undone.pop()
    if (!entry) return undefined
    this.syncUndo()
    const sent = this.send(pinTimestamps(entry.ops, entry.at), async () =>
      pinTimestamps(entry.ops, (await entry.sent).timestamp),
    )
    // Refused: it can be redone again later.
    sent.catch(() => {
      this.undone.push(entry)
      this.syncUndo()
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

  // --- the change feed ---------------------------------------------------------

  /**
   * Follow the branch's change feed: hold `/log` open after the last seq seen, and apply each event to what is loaded
   * (values of loaded entities, loaded link groups), so writes from elsewhere show with no reads. Events on what is
   * not loaded are passed over: a later load brings them. Entities it changes count as written (`writtenSince`), so
   * cached finds re-check them and older loads in flight don't undo them. A cursor the server doesn't know (`reset`)
   * reads everything held again. Network errors retry, backing off. Answers how to stop.
   */
  follow(options: { wait?: number; limit?: number } = {}): () => void {
    let stopped = false
    const wait = options.wait ?? 25_000
    const limit = options.limit ?? 1000
    const run = async () => {
      let failures = 0
      let cursor = this.store.get(this.headAtom)
      while (!stopped) {
        try {
          if (cursor === null) {
            cursor = (await this.client.branch(this.branch)).head
            this.store.set(this.headAtom, cursor)
            continue
          }
          const page = await this.client.log(this.branch, { after: cursor, wait, limit })
          if (stopped) return
          failures = 0
          if (page.reset) {
            cursor = (await this.client.branch(this.branch)).head
            await this.refresh()
          } else if (page.events.length > 0) {
            const since = ++this.writes
            this.preloads.clear()
            this.store.set(this.cacheAtom, (state) => {
              const { base, touched } = applyChanges(state.base, page.events)
              for (const id of touched) this.stamps.set(id, since)
              return { ...state, base }
            })
            cursor = page.head
          } else cursor = page.head
          this.store.set(this.headAtom, cursor)
        } catch (error) {
          if (stopped) return
          failures++
          if (failures === 1) this.fail(error)
          await new Promise((r) => setTimeout(r, [1000, 2000, 5000, 10_000][Math.min(failures - 1, 3)]))
        }
      }
    }
    void run()
    return () => {
      stopped = true
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
