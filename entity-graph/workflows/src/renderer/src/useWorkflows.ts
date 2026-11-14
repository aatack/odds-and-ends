import { useAtom } from 'jotai'
import { useCallback, useEffect, useMemo, useState, useSyncExternalStore } from 'react'
import type { Catalog, WorkflowsAPI } from '../../shared/types'
import { browserApi, inBrowser } from './phone'
import { Cache, withOptimism } from './state/cache'
import { forwardAtom, HISTORY_LIMIT, reachedAtom, sameView, VIEWED_LATELY_MS, viewedAtAtom, viewedAtom, viewsAtom, type View } from './views'

declare global {
  interface Window {
    workflows: WorkflowsAPI
  }
}

let raw: WorkflowsAPI | null = null
let wrapped: WorkflowsAPI | null = null
const rawApi = (): WorkflowsAPI => (raw ??= window.workflows ?? browserApi())
/** What the app shows, kept between draws (state/cache.ts); on the phone, across reloads too. */
export const cache = new Cache(rawApi, inBrowser ? localStorage : null)
/**
 * The app's calls: the preload's in the app's window; in a browser (the phone), straight to the backend (phone.ts).
 * Its common changes are drawn at once, before the backend answers (`withOptimism`).
 */
export const api = (): WorkflowsAPI => (wrapped ??= withOptimism(rawApi(), cache))

/** Everything the app shows, kept in step with the main process. */
export function useWorkflows() {
  const shown = useSyncExternalStore(cache.subscribe, cache.get)
  const overview = useMemo(() => shown.overview ?? cache.overview(), [shown.overview])
  const [views, setViews] = useAtom(viewsAtom)
  const [forwards, setForwards] = useAtom(forwardAtom)
  const view = views[views.length - 1] ?? null
  // Home (the items view, or nothing on the stack) is the person signed in: their item, as any other is shown.
  const selected = view && 'id' in view ? view.id : overview.me.id || null
  // What is here for it at once: undefined while it is being fetched the first time.
  const known = selected ? shown.details[selected] : null
  const detail = known ?? null
  const loading = known === undefined
  const [error, setError] = useState<string | null>(null)
  const [empty, setEmpty] = useState(false)
  const [catalog, setCatalog] = useState<Catalog | null>(null)
  useEffect(() => void api().catalog().then(setCatalog), [])

  // Listened to once: each change refreshes what is here (the item on screen with it).
  useEffect(() => {
    void cache.refresh()
    return api().onChange(() => void cache.refresh())
  }, [])
  // The item on screen: drawn from here at once, fetched if it is not here or may have changed.
  useEffect(() => cache.show(selected ?? null), [selected])

  /** Runs a change, showing what went wrong if it fails. */
  const act = useCallback(async (fn: () => Promise<unknown>) => {
    setError(null)
    try {
      await fn()
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }, [])

  /** Shows an item (or any item) afresh: the stack is just it. From the list and from Next. */
  const select = useCallback(
    (id: string | null) => {
      setError(null)
      setEmpty(false)
      setViews(id ? [{ kind: 'event', id }] : [])
      setForwards([])
    },
    [setViews, setForwards],
  )

  /**
   * Puts a view on top of the stack, unless it is the one already on top. With
   * `replace`, it takes the top view's place instead (if there is one), so Back
   * goes past it.
   */
  const [reached, setReached] = useAtom(reachedAtom)
  const [viewedAt, setViewedAt] = useAtom(viewedAtAtom)
  const open = useCallback(
    (v: View, { replace = false, direct = !replace }: { replace?: boolean; direct?: boolean } = {}) => {
      setError(null)
      const top = views[views.length - 1]
      const below = replace ? views.slice(0, -1) : views
      if (!sameView(below[below.length - 1], v)) setViews([...below, v].slice(-HISTORY_LIMIT))
      else if (replace) setViews(below)
      // Somewhere new: what Back took off is no longer ahead.
      setForwards([])
      // Reached straight from a task: Forward from it, with nothing ahead, comes back here.
      if (direct && top?.kind === 'event' && v.kind === 'event' && top.id !== v.id)
        setReached((r) => Object.fromEntries([...Object.entries(r).filter(([k]) => k !== top.id), [top.id, v.id]].slice(-200)))
    },
    [views, setViews, setForwards, setReached],
  )

  // A task that closes leaves the history (Back and Forward pass it by), unless it is the view on screen.
  useEffect(() => {
    const closed = new Set(overview.items.filter((i) => i.closed).map((i) => i.id))
    const open = (v: View) => !(v.kind === 'event' && closed.has(v.id))
    const below = views.slice(0, -1)
    if (!below.every(open)) setViews([...below.filter(open), ...views.slice(-1)])
    if (!forwards.every(open)) setForwards(forwards.filter(open))
  }, [overview.items, views, forwards, setViews, setForwards])

  /** Takes the top view off the stack, keeping it for Forward. */
  const back = useCallback(() => {
    if (!views.length) return
    setViews(views.slice(0, -1))
    setForwards([...forwards, views[views.length - 1]].slice(-HISTORY_LIMIT))
  }, [views, setViews, forwards, setForwards])

  /**
   * Puts the view Back last took off on top of the stack again. With nothing to put back, goes to what was last
   * reached straight from the task on screen, if anything was.
   */
  const forward = useCallback(() => {
    const top = views[views.length - 1]
    if (!forwards.length) {
      const to = top?.kind === 'event' ? reached[top.id] : undefined
      if (to) setViews([...views, { kind: 'event' as const, id: to }].slice(-HISTORY_LIMIT))
      return
    }
    setViews([...views, forwards[forwards.length - 1]].slice(-HISTORY_LIMIT))
    setForwards(forwards.slice(0, -1))
  }, [views, setViews, forwards, setForwards, reached])

  // What was opened here in the last half hour: Next comes to it only after everything else.
  const viewedLately = useCallback(() => {
    const since = Date.now() - VIEWED_LATELY_MS
    return Object.keys(viewedAt).filter((id) => viewedAt[id] > since)
  }, [viewedAt])

  /**
   * Next: the next item assigned to the human that they have not looked at in the last half hour (as a bot passes over
   * what it ran on lately), never the one on screen. None, and the list says nothing needs them.
   */
  const next = useCallback(async () => {
    const id = await api().next([...viewedLately(), ...(selected ? [selected] : [])])
    select(id)
    setEmpty(id === null)
  }, [selected, select, viewedLately])

  /**
   * After the human has done something to the task on screen: if it closed, goes on to the next that needs them.
   * Pushed, not replacing the stack, so Back returns to it. Handing it to someone else stays on it (for now).
   */
  const moveOn = useCallback(
    async (id: string) => {
      if (selected !== id) return
      // As drawn: a change made here shows at once (state/cache.ts), so this needs no asking.
      const item = cache.get().details[id]?.item?.item
      if (item && !item.closed) return
      const to = await api().next([...viewedLately(), id])
      if (to) open({ kind: 'event', id: to }, { direct: false })
      else {
        open({ kind: 'items' })
        setEmpty(true)
      }
    },
    [selected, open, viewedLately],
  )

  /** `act`, for something done to one item: then, if it no longer needs the human, on to the next (`moveOn`). */
  const actOn = useCallback(
    (id: string) => async (fn: () => Promise<unknown>) => {
      await act(fn)
      await moveOn(id)
    },
    [act, moveOn],
  )

  // What the finder reads as recent: what was viewed here, and who there is.
  const [viewed, setViewed] = useAtom(viewedAtom)
  const recent = useMemo(() => ({ me: overview.me.id, viewed, users: overview.users, bots: overview.bots }), [overview.me.id, viewed, overview.users, overview.bots])
  const markViewed = useCallback(
    (id: string) => {
      setViewed((v) => [id, ...v.filter((x) => x !== id)].slice(0, 50))
      setViewedAt((v) => {
        // Kept only while it still counts as lately.
        const since = Date.now() - VIEWED_LATELY_MS
        return { ...Object.fromEntries(Object.entries(v).filter(([, at]) => at > since)), [id]: Date.now() }
      })
    },
    [setViewed, setViewedAt],
  )
  // Every item on screen is viewed, whichever view shows it (its chat, its notes, its focus): Next passes over it.
  const shownId = view?.kind === 'event' ? view.id : null
  useEffect(() => {
    if (shownId) markViewed(shownId)
  }, [shownId, markViewed])

  return { recent, markViewed, overview, view, views, detail, loading, catalog, selected, select, open, back, forward, next, act, actOn, error, empty }
}
