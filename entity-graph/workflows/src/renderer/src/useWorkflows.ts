import { useAtom } from 'jotai'
import { useCallback, useEffect, useMemo, useState, useSyncExternalStore } from 'react'
import type { Catalog, WorkflowsAPI } from '../../shared/types'
import { browserApi, inBrowser } from './phone'
import { Cache, withOptimism } from './state/cache'
import { forwardAtom, HISTORY_LIMIT, sameView, VIEWED_LATELY_MS, viewedAtAtom, viewedAtom, viewsAtom, type View } from './views'

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

const CATALOG_KEY = 'workflows.catalog'

/** Everything the app shows, kept in step with the main process. */
export function useWorkflows() {
  const shown = useSyncExternalStore(cache.subscribe, cache.get)
  const overview = useMemo(() => shown.overview ?? cache.overview(), [shown.overview])
  const [views, setViews] = useAtom(viewsAtom)
  const [forwards, setForwards] = useAtom(forwardAtom)
  const view = views[views.length - 1] ?? null
  // Home (the items view, or nothing on the stack) is the person signed in: their item, as any other is shown.
  const selected = view && 'id' in view ? view.id : overview.me.id || null
  // What is here for it at once; fetched the first time, it is drawn from what the overview says of it meanwhile.
  const known = selected ? shown.details[selected] : null
  const detail = known === undefined && selected ? cache.placeholder(selected) : (known ?? null)
  const loading = known === undefined
  const [error, setError] = useState<string | null>(null)
  const [empty, setEmpty] = useState(false)
  // The catalog (types and presets) hardly changes: on the phone the kept one is drawn with at once.
  const [catalog, setCatalog] = useState<Catalog | null>(() => {
    try {
      return inBrowser ? (JSON.parse(localStorage.getItem(CATALOG_KEY) ?? 'null') as Catalog | null) : null
    } catch {
      return null
    }
  })
  useEffect(
    () =>
      void api()
        .catalog()
        .then((c) => {
          setCatalog(c)
          if (inBrowser) localStorage.setItem(CATALOG_KEY, JSON.stringify(c))
        }, () => {}),
    [],
  )

  // A change made here that the backend refused: it is taken off the screen, and this says why.
  useEffect(() => cache.onError(setError), [])
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

  const [viewedAt, setViewedAt] = useAtom(viewedAtAtom)
  /**
   * The history is two stacks, each kept to `HISTORY_LIMIT`: the views opened (the top on screen), and those Back took
   * off. Opening a view puts it on top and clears what Back took off; Back moves the top view onto the other stack;
   * Forward moves it back. Nothing else changes either.
   */
  const open = useCallback(
    (v: View) => {
      setError(null)
      setEmpty(false)
      // Opening the view already on screen opens nothing new.
      if (!sameView(views[views.length - 1], v)) setViews([...views, v].slice(-HISTORY_LIMIT))
      setForwards([])
    },
    [views, setViews, setForwards],
  )

  /** Takes the top view off the stack, keeping it for Forward. */
  const back = useCallback(() => {
    if (!views.length) return
    setViews(views.slice(0, -1))
    setForwards([...forwards, views[views.length - 1]].slice(-HISTORY_LIMIT))
  }, [views, setViews, forwards, setForwards])

  /** Puts the view Back last took off on top of the stack again. */
  const forward = useCallback(() => {
    if (!forwards.length) return
    setViews([...views, forwards[forwards.length - 1]].slice(-HISTORY_LIMIT))
    setForwards(forwards.slice(0, -1))
  }, [views, setViews, forwards, setForwards])

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
    if (id) open({ kind: 'event', id })
    else setEmpty(true)
  }, [selected, open, viewedLately])

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
      if (to) open({ kind: 'event', id: to })
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

  return { recent, markViewed, overview, view, views, detail, loading, unsent: shown.unsent, offline: shown.offline, catalog, selected, open, back, forward, next, act, actOn, error, empty }
}
