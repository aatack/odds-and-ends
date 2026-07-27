import type { QueryResult, StackFrame } from '../core/types'
import { connectionAtom } from '../source/connection'
import { query } from '../source/entity'
import { atom } from './atom'
import { getView, viewAtom } from './store'
import { levelKey, topLevel, type Level } from './types'

// The rows behind each level of the navigation stack. Runtime only — nothing
// cached lives in latent state.
//
// An engine rather than a hook: the view reads whatever is here, and the engine
// refetches whenever something the query depends on changes (which entity, which
// direction, or a mutation bumping the revision).
//
// One difference from the desktop worth naming. There, the set of folded entities
// is part of the query: the server is told not to expand them, and folding
// therefore refetches. Here it isn't. Folding is a filter over rows the app
// already has, so a tap opens or closes instantly with no round trip — which is
// what folding is *for* on a small screen, where it is how you get anywhere. The
// cost is that a level fetches its whole subtree (in pages) rather than only the
// open parts, and that is the right way round for a client whose network is the
// slow part.

const PAGE_SIZE = 200

export interface Page {
  results: QueryResult[]
  /** Resume token for the next page; null when the whole tree has been fetched. */
  continuation: StackFrame[] | null
  loading: boolean
  error: string | null
}

export const NO_PAGE: Page = { results: [], continuation: null, loading: false, error: null }

/** Level key → its page. Keys are pruned to the levels actually in the stack. */
export const pagesAtom = atom<Record<string, Page>>({})

/**
 * The little that is known about an entity away from its own row: enough to name
 * it in a crumb, and enough to know what shape it should take.
 */
export interface EntitySummary {
  text?: string
  type?: string
  mimeType?: string
}

/**
 * Entity id → summary, harvested from every page and never pruned. Runtime, but it
 * outlives the pages it came from: navigating away shouldn't turn a crumb back
 * into a uuid.
 */
export const summariesAtom = atom<Record<string, EntitySummary>>({})

/** A value as display text — absent when null or blank, so `??` reaches past it. */
export const str = (v: unknown): string | undefined =>
  v == null || v === '' ? undefined : String(v)

export const summaryOf = (values: Record<string, unknown>): EntitySummary => ({
  text: str(values.text),
  type: str(values.type),
  mimeType: str(values.mimeType),
})

const sameSummary = (a: EntitySummary | undefined, b: EntitySummary): boolean =>
  a != null && a.text === b.text && a.type === b.type && a.mimeType === b.mimeType

function harvestSummaries(results: QueryResult[]): void {
  const known = summariesAtom.get()
  const learned: Record<string, EntitySummary> = {}
  for (const { entity } of results) {
    const summary = summaryOf(entity.values)
    if (!sameSummary(known[entity.id], summary)) learned[entity.id] = summary
  }
  if (Object.keys(learned).length) summariesAtom.set((s) => ({ ...s, ...learned }))
}

/** Bumped by mutations; part of every request key, so everything refetches. */
let revision = 0

/** Level key → the request key currently in flight or settled, for staleness. */
const issued = new Map<string, string>()

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

const patch = (key: string, fn: (p: Page) => Page): void =>
  pagesAtom.set((pages) => ({ ...pages, [key]: fn(pages[key] ?? NO_PAGE) }))

/** Everything a level's first page depends on. */
const requestKey = (level: Level): string =>
  JSON.stringify([level.rootId, level.direction, revision])

async function load(level: Level, key: string, request: string): Promise<void> {
  patch(key, (p) => ({ ...p, loading: true, error: null }))
  try {
    const page = await query(level.rootId, { limit: PAGE_SIZE, direction: level.direction })
    if (issued.get(key) !== request) return
    harvestSummaries(page.results)
    patch(key, () => ({
      results: page.results,
      continuation: page.continuationStack,
      loading: false,
      error: null,
    }))
  } catch (e) {
    if (issued.get(key) !== request) return
    patch(key, (p) => ({ ...p, loading: false, error: message(e) }))
  }
}

/**
 * Reconcile the cache with the stack, and fetch what's stale.
 *
 * Only the level on screen is fetched. The ones beneath it keep their pages, so
 * going back is instant — but a page whose request key has moved on (any mutation
 * does that) is dropped rather than shown, so going back never shows an outline
 * that no longer matches the store. It reloads when it comes back into view.
 */
function sync(): void {
  const view = getView()
  const top = topLevel(view)
  const topKey = levelKey(top)
  const wanted = new Map(view.stack.map((level) => [levelKey(level), level]))

  const pages = pagesAtom.get()
  const drop = Object.keys(pages).filter((key) => {
    const level = wanted.get(key)
    if (!level) return true
    // Levels below the top: keep only while they are still current.
    return key !== topKey && issued.get(key) !== requestKey(level)
  })
  if (drop.length) {
    const pruned = { ...pages }
    for (const key of drop) {
      delete pruned[key]
      issued.delete(key)
    }
    pagesAtom.set(pruned)
  }

  if (!connectionAtom.get()) return
  const request = requestKey(top)
  if (issued.get(topKey) === request) return
  issued.set(topKey, request)
  void load(top, topKey, request)
}

/** Start the engine. Called once, from the app's entry point. */
export function startQueryEngine(): void {
  viewAtom.subscribe(sync)
  connectionAtom.subscribe(() => {
    // A different source means every row on screen belongs to someone else.
    pagesAtom.set({})
    summariesAtom.set({})
    issued.clear()
    sync()
  })
  sync()
}

/** Refetch everything — called after any mutation to the store. */
export function refreshQueries(): void {
  revision++
  issued.clear()
  sync()
}

/** The page behind the level on screen. */
export const currentPage = (): Page => pagesAtom.get()[levelKey(topLevel(getView()))] ?? NO_PAGE

/** Append the next page of the level on screen, if there is one. */
export function loadMore(): void {
  const level = topLevel(getView())
  const key = levelKey(level)
  const page = pagesAtom.get()[key]
  if (!page || page.loading || !page.continuation) return
  const request = issued.get(key)
  patch(key, (p) => ({ ...p, loading: true }))
  void query(level.rootId, {
    limit: PAGE_SIZE,
    direction: level.direction,
    continuationStack: page.continuation,
  })
    .then((next) => {
      // A first-page refetch while this was in flight wins; drop the append.
      if (issued.get(key) !== request) return
      harvestSummaries(next.results)
      patch(key, (p) => ({
        ...p,
        results: [...p.results, ...next.results],
        continuation: next.continuationStack,
        loading: false,
      }))
    })
    .catch((e) => {
      if (issued.get(key) !== request) return
      patch(key, (p) => ({ ...p, loading: false, error: message(e) }))
    })
}

/**
 * An entity's ordered children, from the page on screen. What "insert after this
 * row" needs, and deliberately taken from what the user is looking at rather than
 * re-read from the server: the order on screen is the order they mean.
 */
export function childOrder(entityId: string): string[] {
  for (const page of Object.values(pagesAtom.get())) {
    for (const { entity } of page.results) {
      if (entity.id === entityId) return entity.outboundLinks
    }
  }
  return []
}

/** An entity's values, from whichever cached page happens to hold it. */
export function cachedValues(entityId: string): Record<string, unknown> | undefined {
  for (const page of Object.values(pagesAtom.get())) {
    for (const { entity } of page.results) {
      if (entity.id === entityId) return entity.values
    }
  }
  return undefined
}
