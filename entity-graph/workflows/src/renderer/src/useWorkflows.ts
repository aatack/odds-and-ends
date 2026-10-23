import { useCallback, useEffect, useState } from 'react'
import type { Catalog, ItemDetail, Overview, WorkflowsAPI } from '../../shared/types'

declare global {
  interface Window {
    workflows: WorkflowsAPI
  }
}

export const api = (): WorkflowsAPI => window.workflows

/** Everything the app shows, kept in step with the main process. */
export function useWorkflows() {
  const [overview, setOverview] = useState<Overview>({ items: [], workers: [] })
  const [selected, setSelected] = useState<string | null>(null)
  const [detail, setDetail] = useState<ItemDetail | null>(null)
  const [skipped, setSkipped] = useState<string[]>([])
  const [error, setError] = useState<string | null>(null)
  const [empty, setEmpty] = useState(false)
  const [catalog, setCatalog] = useState<Catalog | null>(null)
  useEffect(() => void api().catalog().then(setCatalog), [])

  const refresh = useCallback(async () => {
    setOverview(await api().overview())
    setDetail(selected ? await api().detail(selected) : null)
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

  const select = useCallback((id: string | null) => {
    setError(null)
    setEmpty(false)
    setSelected(id)
  }, [])

  const next = useCallback(
    async (skip: string[] = skipped) => {
      const id = await api().next(skip)
      select(id)
      setEmpty(id === null)
    },
    [skipped, select],
  )

  const skip = useCallback(
    (id: string) => {
      const all = [...skipped, id]
      setSkipped(all)
      void next(all)
    },
    [skipped, next],
  )

  return { overview, detail, catalog, selected, select, next, skip, act, error, empty }
}
