import { useAtom } from 'jotai'
import { useCallback, useEffect, useRef, useState } from 'react'
import type { Catalog, EventDetail, Overview, WorkflowsAPI } from '../../shared/types'
import { httpApi } from './transport/http'
import { sameView, viewsAtom, type View } from './views'

declare global {
  interface Window {
    workflows?: WorkflowsAPI
  }
}

/** No preload (window.workflows) means we are the phone build, reached over HTTP. */
export const isPhone = !window.workflows

/** The single place the whole renderer reaches the main process: IPC on desktop, HTTP on the phone. */
export const api = (): WorkflowsAPI => window.workflows ?? httpApi

/** Everything the app shows, kept in step with the main process. */
export function useWorkflows() {
  const [overview, setOverview] = useState<Overview>({ items: [], workers: [] })
  const [views, setViews] = useAtom(viewsAtom)
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
    },
    [setViews],
  )

  /** Puts a view on top of the stack, unless it is the one already on top. */
  const open = useCallback(
    (v: View) => {
      setError(null)
      if (!sameView(views[views.length - 1], v)) setViews([...views, v])
    },
    [views, setViews],
  )

  /** Takes the top view off the stack. */
  const back = useCallback(() => setViews(views.slice(0, -1)), [views, setViews])

  // On the phone the system Back must not leave the app while the stack has views, but the stack is
  // persisted so it can outlive the history. We keep ONE guard history entry while the stack is
  // non-empty: in-app Back pops the stack directly (above); a system Back consumes the guard instead
  // of leaving, and we pop a view and re-arm. When the stack is empty there is no guard, so Back leaves.
  // Unarmed at start: a reload with a persisted stack arms the guard in the effect below.
  const armed = useRef(false)
  useEffect(() => {
    if (!isPhone) return
    if (views.length > 0 && !armed.current) {
      history.pushState(null, '')
      armed.current = true
    }
  }, [views.length])
  useEffect(() => {
    if (!isPhone) return
    const onPop = () => {
      armed.current = false // the guard entry was just consumed
      setViews((v) => {
        if (v.length === 0) return v
        const next = v.slice(0, -1)
        if (next.length > 0) {
          history.pushState(null, '')
          armed.current = true
        }
        return next
      })
    }
    window.addEventListener('popstate', onPop)
    return () => window.removeEventListener('popstate', onPop)
  }, [setViews])

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

  return { overview, view, views, detail, catalog, selected, select, open, back, next, skip, act, error, empty }
}
