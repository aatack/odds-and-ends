import { useAtom } from 'jotai'
import { useCallback, useEffect, useMemo, useState } from 'react'
import type { Catalog, CustomAction, EventDetail, Overview, WorkflowsAPI } from '../../shared/types'
import { forwardAtom, HISTORY_LIMIT, reachedAtom, sameView, viewedAtom, viewsAtom, type View } from './views'

declare global {
  interface Window {
    workflows: WorkflowsAPI
  }
}

export const api = (): WorkflowsAPI => window.workflows

/** Everything the app shows, kept in step with the main process. */
export function useWorkflows() {
  const [overview, setOverview] = useState<Overview>({
    me: { id: '', name: '' },
    items: [],
    bots: [],
    users: [],
    recentAssignees: [],
  })
  const [views, setViews] = useAtom(viewsAtom)
  const [forwards, setForwards] = useAtom(forwardAtom)
  const view = views[views.length - 1] ?? null
  const selected = view && 'id' in view ? view.id : null
  const [detail, setDetail] = useState<EventDetail | null>(null)
  const [skipped, setSkipped] = useState<string[]>([])
  const [error, setError] = useState<string | null>(null)
  const [empty, setEmpty] = useState(false)
  const [catalog, setCatalog] = useState<Catalog | null>(null)
  useEffect(() => void api().catalog().then(setCatalog), [])

  const [customActions, setCustomActions] = useState<CustomAction[]>([])
  const refresh = useCallback(async () => {
    setOverview(await api().overview())
    setCustomActions(await api().customActions())
    setDetail(selected ? await api().eventDetail(selected) : null)
  }, [selected])

  useEffect(() => {
    void refresh()
    return api().onChange(() => void refresh())
  }, [refresh])

  /** Runs a change, showing what went wrong if it fails. */
  const act = useCallback(async (fn: () => Promise<unknown>) => {
    setError(null)
    try {
      await fn()
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }, [])

  /** Shows an item (or any event) afresh: the stack is just it. From the list and from Next. */
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

  const next = useCallback(
    async (skip: string[] = skipped) => {
      // Next moves on from what is on screen; it comes back to it only when nothing else needs the human.
      const id = (selected && (await api().next([...skip, selected]))) || (await api().next(skip))
      select(id)
      setEmpty(id === null)
    },
    [skipped, selected, select],
  )

  const skip = useCallback(
    (id: string) => {
      const all = [...skipped, id]
      setSkipped(all)
      void next(all)
    },
    [skipped, next],
  )

  /**
   * After the human has done something to the task on screen: if it closed, goes on to the next that needs them.
   * Pushed, not replacing the stack, so Back returns to it. Handing it to someone else stays on it (for now).
   */
  const moveOn = useCallback(
    async (id: string) => {
      if (selected !== id) return
      const item = (await api().eventDetail(id))?.item?.item
      if (item && !item.closed) return
      const to = await api().next([...skipped, id])
      if (to) open({ kind: 'event', id: to }, { direct: false })
      else {
        open({ kind: 'items' })
        setEmpty(true)
      }
    },
    [selected, skipped, open],
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
  const recent = useMemo(() => ({ viewed, users: overview.users }), [viewed, overview.users])
  const markViewed = useCallback((id: string) => setViewed((v) => [id, ...v.filter((x) => x !== id)].slice(0, 50)), [setViewed])

  return { recent, markViewed, overview, view, views, detail, catalog, customActions, selected, select, open, back, forward, next, skip, act, actOn, error, empty }
}
