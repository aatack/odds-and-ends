// React hooks over `Egdb`. Each hook reads the cache synchronously and always
// returns something to draw; asking for data that is not there is what starts
// the load. See PARADIGM.md for the walk and its anchor.

import { useAtomValue } from 'jotai/react'
import { createContext, useCallback, useContext, useEffect, useLayoutEffect, useMemo, useState, type ReactNode } from 'react'
import type { EntityRecord, EntityView } from '../cache.js'
import { localize, rowsOf, type FindRowView } from '../find.js'
import { op } from '../ops.js'
import { stableJson, queryKey, type Egdb, type FindQueryOptions } from '../store.js'
import type { EntityId, Filter, LinkPair, Op, Path, Sort, Value, WriteResult } from '../types.js'
import { computeWindow, pathKey, type TreeRow, type WalkQuery, type WalkWindow } from '../walk.js'

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
    }),
    [egdb],
  )
}

// --- walk --------------------------------------------------------------------

export interface UseWalkParams {
  /** The walk's root path. Rows are its last entity and that entity's subtree. */
  root: Path
  /** Where rows are counted from. Default: the root. */
  anchor?: Path
  query: WalkQuery
  /** First row wanted, relative to the anchor (may be negative). */
  offset: number
  /** Number of rows wanted. */
  limit: number
  /** Rows past each end of the window to check for missing data. Default 50. */
  lookahead?: number
}

export interface UseWalkResult extends WalkWindow {
  /** Some row in the window is loading, or has children loading. */
  loading: boolean
}

/**
 * Rows [offset, offset + limit) of a walk, counted from `anchor`. The rows are
 * stepped out of the cache on every render, so writes, folding and filter
 * changes show at once; missing data shows as loading rows and is preloaded in
 * batches.
 */
export function useWalk(params: UseWalkParams): UseWalkResult {
  const egdb = useEgdb()
  const view = useEntityView()
  const { root, anchor, query, offset, limit, lookahead } = params
  const rootKey = pathKey(root)
  const anchorKey = anchor ? pathKey(anchor) : ''
  const key = queryKey(query)
  const window = useMemo(
    () => computeWindow(view, { root, anchor, query, offset, limit, lookahead }),
    // The keys stand for root, anchor and query, which are often new objects with the same content.
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [view, rootKey, anchorKey, key, offset, limit, lookahead],
  )
  const forward = window.missing.forward
  const backward = window.missing.backward
  const forwardKey = forward ? pathKey(forward) : null
  const backwardKey = backward ? pathKey(backward) : null
  useEffect(() => {
    if (forward) void egdb.preloadWalk(root, query, forward, 'forward')
    if (backward) void egdb.preloadWalk(root, query, backward, 'backward')
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [egdb, rootKey, key, forwardKey, backwardKey])
  const loading = window.rows.some((row) => row.status === 'loading' || row.childrenLoading)
  return { ...window, loading }
}

export interface UseWalkViewportParams {
  root: Path
  query: WalkQuery
  /** The rows on screen: absolute indices, `end` exclusive. */
  range: { start: number; end: number }
  /** Rows to compute past each end of the range. Default 20. */
  overscan?: number
  /**
   * How far (in rows) the range may move from the anchor before the anchor
   * moves to the first row on screen. Default 0: the anchor is always the
   * first row on screen, so nothing that changes above the screen can move
   * what is on it.
   */
  reanchorAfter?: number
  /** The anchor to start at. Default: the root, as row 0. */
  initialAnchor?: { path: Path; index: number }
  /**
   * Rows were found or lost above the anchor, so every index moved by
   * `shift`. Scroll by `shift` rows in the same frame to keep the view still.
   */
  onShift?: (shift: number) => void
}

export interface WalkViewport {
  /** Rows for the virtualizer. Grows as the user nears the end, until the walk ends. */
  count: number
  /** The row at an absolute index, if it is computed. */
  rowAt(index: number): TreeRow | undefined
  /** The computed rows, from absolute index `first`. */
  rows: TreeRow[]
  first: number
  /** The anchor path and its absolute index. */
  anchor: EntityId[]
  anchorIndex: number
  /** Move the anchor: `path` will be row `index`. For "reveal this row", or a new root. */
  reanchor(path: Path, index: number): void
  atEnd: boolean
  loading: boolean
}

/**
 * `useWalk` for a virtualized table. The table says which rows are on screen;
 * this keeps the anchor near them, so that only a few hundred rows are ever
 * stepped, however far down the table is scrolled. See PARADIGM.md.
 */
export function useWalkViewport(params: UseWalkViewportParams): WalkViewport {
  const { root, query, range, onShift } = params
  const overscan = params.overscan ?? 20
  const reanchorAfter = params.reanchorAfter ?? 0
  const rootKey = pathKey(root)
  const [state, setState] = useState(() => ({
    rootKey,
    anchor: [...(params.initialAnchor?.path ?? root)],
    anchorIndex: params.initialAnchor?.index ?? 0,
  }))
  // A new root starts again at its top.
  const current = state.rootKey === rootKey ? state : { rootKey, anchor: [...root], anchorIndex: 0 }
  if (current !== state) setState(current)
  const { anchor, anchorIndex } = current

  const walk = useWalk({
    root,
    anchor,
    query,
    offset: range.start - anchorIndex - overscan,
    limit: Math.max(0, range.end - range.start) + 2 * overscan,
    lookahead: overscan,
  })
  const first = anchorIndex + walk.first
  const known = walk.rows.length > 0

  // Rows above index 0, or a gap above the first row of the tree: move every
  // index so that the tree's first row is row 0.
  const shift = known && (first < 0 || (walk.atStart && first > 0)) ? -first : 0
  useLayoutEffect(() => {
    if (shift === 0) return
    setState((s) => ({ ...s, anchorIndex: s.anchorIndex + shift }))
    onShift?.(shift)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shift, anchorIndex])

  // The range has moved far from the anchor: move the anchor to the range.
  const target = Math.abs(range.start - anchorIndex) > reanchorAfter ? walk.rows[range.start - first] : undefined
  useEffect(() => {
    if (shift !== 0 || !target) return
    setState((s) => ({ ...s, anchor: target.path, anchorIndex: range.start }))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shift, target?.key, range.start])

  const end = first + walk.rows.length
  const count = walk.atEnd ? end : Math.max(end, range.end) + Math.max(1, overscan)
  const rows = walk.rows
  const rowAt = useCallback((index: number) => rows[index - first], [rows, first])
  const reanchor = useCallback(
    (path: Path, index: number) => setState((s) => ({ ...s, anchor: [...path], anchorIndex: index })),
    [],
  )
  return {
    count: Math.max(0, count),
    rowAt,
    rows,
    first,
    anchor: walk.anchor,
    anchorIndex,
    reanchor,
    atEnd: walk.atEnd,
    loading: walk.loading,
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
