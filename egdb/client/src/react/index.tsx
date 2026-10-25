// React hooks over `Egdb`. Each hook reads the cache synchronously and always
// returns something to draw; asking for data that is not there is what starts
// the load. See PARADIGM.md for the walk and its anchor.

import { useAtomValue } from 'jotai/react'
import { createContext, useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import type { EntityRecord, EntityView } from '../cache.js'
import { localize, rowsOf, type FindRowView } from '../find.js'
import { op } from '../ops.js'
import { stableJson, queryKey, type Egdb, type FindQueryOptions } from '../store.js'
import type { EntityId, Filter, Follow, LinkPair, Op, Path, Sort, Value, WriteResult } from '../types.js'
import { computeWindow, countBefore, pathKey, type TreeRow, type WalkQuery } from '../walk.js'

export type { FindRowView } from '../find.js'

const EgdbContext = createContext<Egdb | null>(null)

export function EgdbProvider({ egdb, children }: { egdb: Egdb; children?: ReactNode }) {
  return <EgdbContext.Provider value={egdb}>{children}</EgdbContext.Provider>
}

export function useEgdb(): Egdb {
  const egdb = useContext(EgdbContext)
  if (!egdb) throw new Error('egdb hooks need an <EgdbProvider>')
  return egdb
}

/** The cache as reads see it: confirmed data with pending writes on top. */
export function useEntityView(): EntityView {
  const egdb = useEgdb()
  return useAtomValue(egdb.viewAtom, { store: egdb.store })
}

// --- entities ----------------------------------------------------------------

/**
 * One entity from the cache, loading it if it is not there. `groups` are link
 * groups to load with it.
 */
export function useEntity(id: EntityId, groups: readonly LinkPair[] = []): EntityRecord & { status: 'ready' | 'loading' } {
  const egdb = useEgdb()
  const record = useEntityView().get(id)
  const missing = !record?.values || groups.some(([t, d]) => !record.links[`${t}\u0000${d}`] && !record.local)
  const groupsKey = JSON.stringify(groups)
  useEffect(() => {
    if (missing) void egdb.load([id], groups)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [egdb, id, missing, groupsKey])
  return { ...(record ?? { id, links: {} }), status: record?.values ? 'ready' : 'loading' }
}

// --- writes ------------------------------------------------------------------

export interface Writer {
  /** Write a batch. It shows at once; the promise settles when the server has it. */
  write(ops: Op | readonly Op[]): Promise<WriteResult>
  set(entity: EntityId, key: string, value: Value): Promise<WriteResult>
  link(source: EntityId, destination: EntityId, linkType: string, sort?: Sort): Promise<WriteResult>
  unlink(source: EntityId, destination: EntityId, linkType: string): Promise<WriteResult>
  archive(entity: EntityId): Promise<WriteResult>
  /** Undo this author's newest batch (not optimistic: it waits for the server). */
  undo(): ReturnType<Egdb['undo']>
  /** Send the newest undone batch again (optimistic); `undefined` when there is none. */
  redo(): ReturnType<Egdb['redo']>
}

export function useWrite(): Writer {
  const egdb = useEgdb()
  return useMemo(
    () => ({
      write: (ops) => egdb.write(ops),
      set: (entity, key, value) => egdb.write(op.set(entity, key, value)),
      link: (source, destination, linkType, sort) => egdb.write(op.link(source, destination, linkType, sort)),
      unlink: (source, destination, linkType) => egdb.write(op.unlink(source, destination, linkType)),
      archive: (entity) => egdb.write(op.archive(entity)),
      undo: () => egdb.undo(),
      redo: () => egdb.redo(),
    }),
    [egdb],
  )
}

/** How many of this client's batches can be undone and redone. */
export function useHistory(): { undo: number; redo: number } {
  const egdb = useEgdb()
  return useAtomValue(egdb.historyAtom, { store: egdb.store })
}

// --- walk --------------------------------------------------------------------

export interface UseWalkParams {
  /** The walk's root path. Rows are its last entity and that entity's subtree. */
  root: Path
  /** Which links to follow out of an entity, by its type. */
  follow: Follow
  /** A node is a row only when it matches. */
  include?: Filter
  /** The walk goes below a node only when it matches. */
  descend?: Filter
  /** Rows not to go below (folded rows), by path. */
  collapsed?: readonly Path[]
  /** Without `range`: how many rows from the top. Default 500. */
  limit?: number

  // For a virtualized table:

  /**
   * The rows on screen, by absolute index (`end` exclusive). With it, rows
   * are counted from an anchor near the screen, so only the rows near the
   * screen are calculated, however far down the table is. See PARADIGM.md.
   */
  range?: { start: number; end: number }
  /** Rows to calculate past each end of the range. Default 20. */
  overscan?: number
  /** The calculated window grows and moves in steps of this many rows. Default 50. */
  chunk?: number
  /**
   * Rows to add past the last known row while the end is not known, so the
   * user can scroll on. The count never goes down until the end is known.
   * Default 200.
   */
  buffer?: number
  /** How far (in rows) the range may move from the anchor before the anchor moves to it. Default 100. */
  reanchorAfter?: number
  /** The anchor to start at. Default: the root, as row 0. */
  initialAnchor?: { path: Path; index: number }
  /**
   * Rows were found or lost above the anchor, so every index moved by
   * `shift`. Scroll by `shift` rows in the same frame to keep the view still.
   */
  onShift?: (shift: number) => void
  /** Rows past each end of the window to check for missing data. Default 50. */
  lookahead?: number
}

export interface UseWalkResult {
  /** The calculated rows, in order, from absolute index `first`. */
  rows: TreeRow[]
  first: number
  /** The row at an absolute index, if it is calculated. */
  rowAt(index: number): TreeRow | undefined
  /** Rows for a virtualizer: the rows known so far plus a margin, until the walk ends. */
  count: number
  /** The walk has no rows after the last of `rows`. */
  atEnd: boolean
  /** Some row is loading, or has children loading. */
  loading: boolean
  /** The anchor path (as it resolves now) and its absolute index. */
  anchor: EntityId[]
  anchorIndex: number
  /** Move the anchor: `path` will be row `index`. For "reveal this row". */
  reanchor(path: Path, index: number): void
}

/**
 * A walk of the tree under `root`, calculated from the cache on every render,
 * so writes, folding and filter changes show at once. Missing data shows as
 * loading rows and is preloaded in batches.
 *
 * Plain use, `useWalk({ root, follow })`, gives the first `limit` rows. Give
 * `range` (the rows on screen) for a virtualized table of any size.
 */
export function useWalk(params: UseWalkParams): UseWalkResult {
  const egdb = useEgdb()
  const view = useEntityView()
  const { root, follow, include, descend, collapsed, range, onShift } = params
  const virtual = range !== undefined
  const overscan = virtual ? (params.overscan ?? 20) : 0
  const chunk = virtual ? Math.max(1, params.chunk ?? 50) : 1
  const reanchorAfter = params.reanchorAfter ?? 100
  const lookahead = params.lookahead ?? 50
  const buffer = params.buffer ?? 200
  const visible = range ?? { start: 0, end: params.limit ?? 500 }

  const query: WalkQuery = { follow, include, descend, collapsed }
  const key = queryKey(query)
  const rootKey = pathKey(root)
  const [state, setState] = useState(() => ({
    rootKey,
    anchor: [...(params.initialAnchor?.path ?? root)],
    anchorIndex: params.initialAnchor?.index ?? 0,
  }))
  // A new root starts again at its top.
  let current = state.rootKey === rootKey ? state : { rootKey, anchor: [...root], anchorIndex: 0 }
  // A new query (filter, folding) unrolls from the first row on screen, so
  // the table shows the new rows of the same place in the tree.
  const last = useRef<{ key: string; rows: TreeRow[]; first: number } | null>(null)
  if (virtual && last.current && last.current.key !== key) {
    const top = last.current.rows[visible.start - last.current.first]
    if (top && (top.key !== pathKey(current.anchor) || current.anchorIndex !== visible.start)) {
      current = { ...current, anchor: top.path, anchorIndex: visible.start }
    }
  }
  if (current !== state) setState(current)
  const { anchor, anchorIndex } = current
  const anchorKey = pathKey(anchor)
  const high = useRef({ rootKey, count: 0 })
  if (high.current.rootKey !== rootKey) high.current = { rootKey, count: 0 }

  // The window, relative to the anchor, in whole chunks: scrolling inside a
  // chunk asks for the same window, so nothing is calculated again.
  const from = Math.floor((visible.start - overscan - anchorIndex) / chunk) * chunk
  const to = Math.ceil((visible.end + overscan - anchorIndex) / chunk) * chunk
  const window = useMemo(
    () => computeWindow(view, { root, anchor, query, offset: from, limit: Math.max(0, to - from), lookahead }),
    // The keys stand for root, anchor and query, which are often new objects with the same content.
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [view, rootKey, anchorKey, key, from, to, lookahead],
  )

  // Preload where the cache lacks data.
  const forward = window.missing.forward
  const backward = window.missing.backward
  const forwardKey = forward ? pathKey(forward) : null
  const backwardKey = backward ? pathKey(backward) : null
  useEffect(() => {
    if (forward) void egdb.preloadWalk(root, query, forward, 'forward')
    if (backward) void egdb.preloadWalk(root, query, backward, 'backward')
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [egdb, rootKey, key, forwardKey, backwardKey])

  const first = anchorIndex + window.first
  const known = window.rows.length > 0
  last.current = { key, rows: window.rows, first }

  // Rows above index 0, or a gap above the first row of the tree: move every
  // index so that the tree's first row is row 0.
  // Rows above index 0: count all the rows above the anchor, so that one
  // shift is enough (a shift per window could repeat past React's limit).
  const shift = useMemo(() => {
    if (!known) return 0
    if (first < 0) return countBefore(view, { root, anchor, query }).count - anchorIndex
    return window.atStart && first > 0 ? -first : 0
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [known, first, window, anchorIndex])
  useLayoutEffect(() => {
    if (shift === 0) return
    setState((s) => ({ ...s, anchorIndex: s.anchorIndex + shift }))
    high.current.count += shift
    onShift?.(shift)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shift, anchorIndex])

  // The screen has moved far from the anchor: move the anchor to the screen.
  const target =
    virtual && Math.abs(visible.start - anchorIndex) > reanchorAfter ? window.rows[visible.start - first] : undefined
  useEffect(() => {
    if (shift !== 0 || !target) return
    setState((s) => ({ ...s, anchor: target.path, anchorIndex: visible.start }))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shift, target?.key, visible.start])

  // Without a range, the rows are exactly [0, limit).
  const rows = useMemo(
    () => (virtual ? window.rows : window.rows.slice(Math.max(0, -first), visible.end - first)),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [window, virtual, first, visible.end],
  )
  const rowsFirst = virtual ? first : Math.max(0, first)
  const end = rowsFirst + rows.length
  const atEnd = window.atEnd && end >= first + window.rows.length
  // The count only grows until the end is known, so the scroll bar does not
  // jump as the window moves, and a new query does not clamp the scroll.
  // While the end is not known, it grows in whole steps of `buffer` rows.
  let count = atEnd || !virtual ? end : Math.ceil((Math.max(end, visible.end) + buffer) / buffer) * buffer
  if (virtual && !atEnd) count = Math.max(count, high.current.count)
  high.current.count = count
  const rowAt = useCallback((index: number) => rows[index - rowsFirst], [rows, rowsFirst])
  const reanchor = useCallback(
    (path: Path, index: number) => setState((s) => ({ ...s, anchor: [...path], anchorIndex: index })),
    [],
  )
  const loading = rows.some((row) => row.status === 'loading' || row.childrenLoading)
  return {
    rows,
    first: rowsFirst,
    rowAt,
    count: Math.max(0, count),
    atEnd,
    loading,
    anchor: window.anchor,
    anchorIndex,
    reanchor,
  }
}

// --- find --------------------------------------------------------------------

export interface UseFindParams extends FindQueryOptions {
  filter: Filter
  /** First row wanted. Default 0. */
  offset?: number
  /** Rows wanted. Default 100. Pages are fetched until `offset + limit` rows are in. */
  limit?: number
}

export interface UseFindResult {
  rows: FindRowView[]
  /** Rows known so far (all of them once `done`). */
  total: number
  /** The server has no more rows. */
  done: boolean
  loading: boolean
  error?: unknown
  /** Forget the results and ask the server again. */
  refresh(): void
}

/**
 * Rows [offset, offset + limit) of entities that match `filter`. The server
 * gives the list; values come from the cache, so edits show at once, and an
 * entity written here moves in or out of the list as soon as it is written.
 */
export function useFind(params: UseFindParams): UseFindResult {
  const egdb = useEgdb()
  const view = useEntityView()
  const finds = useAtomValue(egdb.findsAtom, { store: egdb.store })
  const { filter, order, include_archived, scan_limit, pageSize } = params
  const offset = params.offset ?? 0
  const limit = params.limit ?? 100
  const key = stableJson([filter, order ?? null, include_archived ?? false, scan_limit ?? null])
  const state = finds.get(key)
  const want = offset + limit
  useEffect(() => {
    void egdb.findMore(key, filter, { order, include_archived, scan_limit, pageSize }, want)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [egdb, key, want, state?.ids.length, state?.loading])
  const ids = useMemo(() => {
    if (!state) return []
    return localize(view, {
      ids: state.ids,
      done: state.done,
      filter,
      order,
      includeArchived: include_archived,
      written: egdb.writtenSince(state.since),
    })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [view, state, key])
  const rows = useMemo(() => rowsOf(view, ids.slice(offset, offset + limit)), [view, ids, offset, limit])
  const refresh = useCallback(() => egdb.resetFind(key), [egdb, key])
  return {
    rows,
    total: ids.length,
    done: state?.done ?? false,
    loading: state?.loading ?? true,
    error: state?.error,
    refresh,
  }
}
