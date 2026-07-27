import {
  applyEvents,
  entities,
  refreshEntities,
  removeEvents,
  setEntityFetcher,
} from '../../../src/core/cache'
import type { EntitySource } from '../../../src/core/query'
import type { Entity } from '../core/types'
import { connectionAtom } from '../source/connection'
import { scanEvents, setWriteObserver } from '../source/entity'
import { atom } from './atom'
import { buildRows, type ViewRows } from './derive'
import { getView } from './store'
import { levelKey, topLevel, type Level, type ViewState } from './types'

// A level's query, which is no longer a fetch. The client keeps every event it
// has read (`core/cache`, shared with the desktop app) and steps the traversal
// over it, so the rows are a derivation: folding, going in and out of a level,
// and every edit redraw without a round trip, and the tree fills in as events
// arrive rather than appearing all at once.
//
// That is what this module is left holding — how far each level has been
// unrolled, which is what pagination has become.

export { refreshEntities as refreshQueries }

/** Rows unrolled per page. A level's whole budget until it is scrolled. */
export const PAGE_SIZE = 200

/**
 * Level key → how many rows its traversal may produce. Runtime only, and only
 * ever raised; a key left behind by a level you have navigated away from costs
 * one number, and is what makes coming back land where you left off.
 */
export const rowLimitsAtom = atom<Record<string, number>>({})

export const rowLimit = (level: Level): number =>
  rowLimitsAtom.get()[levelKey(level)] ?? PAGE_SIZE

/** The rows on screen, against a read of the cache and the limits in hand. */
export const rowsFrom = (
  s: ViewState,
  source: EntitySource,
  limits: Record<string, number>,
): ViewRows => {
  const level = topLevel(s)
  return buildRows(s, level, source, limits[levelKey(level)] ?? PAGE_SIZE)
}

/** The rows on screen as they stand — the live counterpart to `useRows`. */
export const viewRows = (s: ViewState = getView()): ViewRows =>
  rowsFrom(s, entities(), rowLimitsAtom.get())

/**
 * Unroll another page of the level on screen, if the traversal stopped at the
 * limit rather than because there was nothing more. A level still waiting on
 * entities looks finished — one nobody has read yet has no children — so it
 * doesn't grow here; it grows when they arrive, and this is asked again on the
 * next scroll.
 */
export function loadMore(): void {
  if (viewRows().complete) return
  const key = levelKey(topLevel(getView()))
  rowLimitsAtom.set((limits) => ({ ...limits, [key]: (limits[key] ?? PAGE_SIZE) + PAGE_SIZE }))
}

/**
 * Plug the cache into the connection. Called once, from the app's entry point;
 * with no connection the fetcher simply fails, and the first read after one
 * arrives fills everything in.
 */
export function startQueryEngine(): void {
  setEntityFetcher(scanEvents)
  setWriteObserver({ applied: applyEvents, removed: removeEvents })
  connectionAtom.subscribe(() => {
    // A different source means every entity held belongs to someone else.
    setEntityFetcher(scanEvents)
  })
}

/**
 * An entity as it currently stands. Asking is what loads it, so this is also how
 * a caller says it cares about something.
 */
export const entity = (id: string): Entity => entities().get([id])[id]

/**
 * An entity's ordered children. What "insert after this row" needs — and the
 * same list the outline is drawn from, so the order on screen is the order the
 * user means.
 */
export const childOrder = (entityId: string): string[] => entity(entityId).outboundLinks

/** An entity's values, for the tools that fold a context out of them. */
export const cachedValues = (entityId: string): Record<string, unknown> => entity(entityId).values
