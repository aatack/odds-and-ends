import { useAtom } from 'jotai'
import { useCallback, useEffect, useState } from 'react'
import type { Catalog, EventDetail, Overview, WorkflowsAPI } from '../../shared/types'
import { forwardAtom, sameView, viewsAtom, type View } from './views'

declare global {
  interface Window {
    workflows: WorkflowsAPI
  }
}

export const api = (): WorkflowsAPI => window.workflows

/** Everything the app shows, kept in step with the main process. */
export function useWorkflows() {
  const [overview, setOverview] = useState<Overview>({ items: [], workers: [] })
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

  const refresh = useCallback(async () => {
    setOverview(await api().overview())
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
  const open = useCallback(
    (v: View, { replace = false }: { replace?: boolean } = {}) => {
      setError(null)
      const below = replace ? views.slice(0, -1) : views
      if (!sameView(below[below.length - 1], v)) setViews([...below, v])
      else if (replace) setViews(below)
      // Somewhere new: what Back took off is no longer ahead.
      setForwards([])
    },
    [views, setViews, setForwards],
  )

  /** Takes the top view off the stack, keeping it for Forward. */
  const back = useCallback(() => {
    if (!views.length) return
    setViews(views.slice(0, -1))
    setForwards([...forwards, views[views.length - 1]])
  }, [views, setViews, forwards, setForwards])

  /** Puts the view Back last took off on top of the stack again. */
  const forward = useCallback(() => {
    if (!forwards.length) return
    setViews([...views, forwards[forwards.length - 1]])
    setForwards(forwards.slice(0, -1))
  }, [views, setViews, forwards, setForwards])

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
      void api().skipped(id)
      const all = [...skipped, id]
      setSkipped(all)
      void next(all)
    },
    [skipped, next],
  )

  return { overview, view, views, detail, catalog, selected, select, open, back, forward, next, skip, act, error, empty }
}
