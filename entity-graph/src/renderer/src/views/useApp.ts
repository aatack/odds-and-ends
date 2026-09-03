import { useCallback, useEffect, useMemo, useState } from 'react'
import type { CurrentPensive } from '../../../core/client'

const api = window.entityGraph

export interface AppActions {
  /** Rename the current user (blank ⇒ "anonymous"). */
  setUser: (name: string) => Promise<void>
}

export interface AppState {
  /** False until the persisted user and the open pensive have loaded. */
  ready: boolean
  user: string
  /** What the outliner is showing, or null when nothing is plugged in. */
  pensive: CurrentPensive | null
  /** Why there is nothing to show, when there isn't. */
  problem: string | null
  actions: AppActions
}

/**
 * What the shell needs from the main process: who the user is, and which
 * pensive is open.
 *
 * There is nothing to choose here any more. The open pensive is whatever is
 * plugged into the desktop node on the sources page, so this reads it rather
 * than picking it — and re-reads it whenever the graph changes, which is what
 * makes dragging a different store into that node take effect at once.
 *
 * Note what *isn't* here: which page is showing, and every other piece of UI
 * state, live in the state layer where a tool can reach them.
 */
export function useApp(): AppState {
  const [ready, setReady] = useState(false)
  const [user, setUserState] = useState('anonymous')
  const [pensive, setPensive] = useState<CurrentPensive | null>(null)
  const [problem, setProblem] = useState<string | null>(null)

  const read = useCallback(async () => {
    const [current, why] = await Promise.all([api.currentPensive(), api.pensiveProblem()])
    setPensive(current)
    setProblem(why)
  }, [])

  useEffect(() => {
    void (async () => {
      setUserState(await api.getUser())
      await read()
      setReady(true)
    })()
  }, [read])

  // The graph is edited on the other page; this is how the outliner finds out.
  useEffect(() => api.onPensiveChanged(() => void read()), [read])

  const actions = useMemo<AppActions>(
    () => ({
      setUser: async (name) => {
        const next = name.trim() || 'anonymous'
        await api.setUser(next)
        setUserState(next)
      },
    }),
    [],
  )

  return { ready, user, pensive, problem, actions }
}
