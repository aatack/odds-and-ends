import type { LinkDirection, QueryPage, QueryResult, StackFrame } from '../../../core/wrapper'
import { atom } from './atom'
import { getLayout, layoutAtom } from './store'
import { directionOf } from './types'

// The query cache: the rows behind each mounted frame. Runtime only — never
// persisted, per the rule that nothing cached lives in latent state.
//
// This is an engine rather than a hook: a view "retains" a frame it wants loaded
// and reads the result, and the engine refetches whenever anything the query
// depends on changes (root, collapse set, depth cap, or a mutation bumping the
// revision). Headless callers retain frames the same way.

const PAGE_SIZE = 200

export interface FramePage {
  results: QueryResult[]
  /** Resume token for the next page; null when the whole tree has been fetched. */
  continuation: StackFrame[] | null
  loading: boolean
  error: string | null
}

export type QueryCache = Record<string, FramePage>

export const NO_PAGE: FramePage = { results: [], continuation: null, loading: false, error: null }

export const queryAtom = atom<QueryCache>({})

/**
 * Entity id → display text, harvested from every page that loads and never
 * pruned. Runtime only, like the cache, but it outlives the pages it came from:
 * dropping a frame's rows shouldn't turn its tab's label back into a uuid. After
 * a reload, tabs you haven't opened yet do show their ids for a moment.
 */
export const namesAtom = atom<Record<string, string>>({})

function harvestNames(results: QueryResult[]): void {
  const learned: Record<string, string> = {}
  for (const { entity } of results) {
    const text = entity.values.text
    if (text != null && text !== '') learned[entity.id] = String(text)
  }
  if (Object.keys(learned).length) namesAtom.set((names) => ({ ...names, ...learned }))
}

export type QueryFetcher = (
  rootId: string,
  opts: {
    maxDepth?: number
    collapsed?: string[]
    limit?: number
    continuationStack?: StackFrame[]
    direction?: LinkDirection
  },
) => Promise<QueryPage>

let fetcher: QueryFetcher | null = null
/** Bumped by mutations; part of every request key, so everything refetches. */
let revision = 0
const retained = new Map<string, number>()
/** frameId → the request key currently in flight or settled, for staleness checks. */
const issued = new Map<string, string>()

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

const patch = (frameId: string, fn: (p: FramePage) => FramePage): void =>
  queryAtom.set((cache) => ({ ...cache, [frameId]: fn(cache[frameId] ?? NO_PAGE) }))

/** Everything a frame's first page depends on. */
function requestKey(frameId: string): string | null {
  const s = getLayout()
  const frame = s.frames[frameId]
  if (!frame) return null
  const collapsed = [...(s.tabs[frame.tabId]?.collapsed ?? [])].sort()
  return JSON.stringify([
    frame.rootId,
    collapsed,
    frame.maxDepth[frame.rootId] ?? null,
    directionOf(frame),
    revision,
  ])
}

async function load(frameId: string, key: string): Promise<void> {
  const s = getLayout()
  const frame = s.frames[frameId]
  const f = fetcher
  if (!frame || !f) return
  patch(frameId, (p) => ({ ...p, loading: true, error: null }))
  try {
    const page = await f(frame.rootId, {
      // Only the root's cap reaches the server for now; the rest of the map is
      // stored against the day the query tool takes a per-entity limit.
      maxDepth: frame.maxDepth[frame.rootId] ?? undefined,
      collapsed: s.tabs[frame.tabId]?.collapsed ?? [],
      limit: PAGE_SIZE,
      direction: directionOf(frame),
    })
    if (issued.get(frameId) !== key) return
    harvestNames(page.results)
    patch(frameId, () => ({
      results: page.results,
      continuation: page.continuationStack,
      loading: false,
      error: null,
    }))
  } catch (e) {
    if (issued.get(frameId) !== key) return
    patch(frameId, (p) => ({ ...p, loading: false, error: message(e) }))
  }
}

/** Reconcile the cache with what's retained; fetch anything stale. */
function sync(): void {
  const cache = queryAtom.get()
  const stale = Object.keys(cache).filter((frameId) => !retained.has(frameId))
  if (stale.length) {
    const pruned = { ...cache }
    for (const frameId of stale) {
      delete pruned[frameId]
      issued.delete(frameId)
    }
    queryAtom.set(pruned)
  }
  if (!fetcher) return
  for (const frameId of retained.keys()) {
    const key = requestKey(frameId)
    if (key == null || issued.get(frameId) === key) continue
    issued.set(frameId, key)
    void load(frameId, key)
  }
}

// Anything that changes a frame's query (its root, its tab's collapse set, its
// depth caps) lands in the layout atom, so one subscription covers them all.
layoutAtom.subscribe(sync)

/** Point the engine at a source. Clears staleness so every frame refetches. */
export function setQueryFetcher(next: QueryFetcher | null): void {
  fetcher = next
  issued.clear()
  if (!next) queryAtom.set({})
  sync()
}

/** Ask for a frame's rows to be kept loaded. Returns the release function. */
export function retainFrame(frameId: string): () => void {
  retained.set(frameId, (retained.get(frameId) ?? 0) + 1)
  sync()
  return () => {
    const n = (retained.get(frameId) ?? 1) - 1
    if (n > 0) retained.set(frameId, n)
    else retained.delete(frameId)
    sync()
  }
}

/** Refetch everything — called after any mutation to the entity store. */
export function refreshQueries(): void {
  revision++
  issued.clear()
  sync()
}

/** Append the next page of a frame's tree, if there is one. */
export function loadMore(frameId: string): void {
  const page = queryAtom.get()[frameId]
  const s = getLayout()
  const frame = s.frames[frameId]
  const f = fetcher
  if (!page || page.loading || !page.continuation || !frame || !f) return
  const key = issued.get(frameId)
  patch(frameId, (p) => ({ ...p, loading: true }))
  void f(frame.rootId, {
    maxDepth: frame.maxDepth[frame.rootId] ?? undefined,
    collapsed: s.tabs[frame.tabId]?.collapsed ?? [],
    limit: PAGE_SIZE,
    continuationStack: page.continuation,
    direction: directionOf(frame),
  })
    .then((next) => {
      // A first-page refetch while this was in flight wins; drop the append.
      if (issued.get(frameId) !== key) return
      harvestNames(next.results)
      patch(frameId, (p) => ({
        ...p,
        results: [...p.results, ...next.results],
        continuation: next.continuationStack,
        loading: false,
      }))
    })
    .catch((e) => {
      if (issued.get(frameId) !== key) return
      patch(frameId, (p) => ({ ...p, loading: false, error: message(e) }))
    })
}

/**
 * An entity's values, from whichever cached frame happens to hold it. Used to
 * fold a call's context; deliberately best-effort, since an unmounted frame's
 * root may not be cached at all.
 */
export function cachedValues(
  cache: QueryCache,
  entityId: string,
): Record<string, unknown> | undefined {
  for (const page of Object.values(cache)) {
    for (const { entity } of page.results) {
      if (entity.id === entityId) return entity.values
    }
  }
  return undefined
}
