import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import type { ActiveSource, CurrentSource } from '../../../core/client'

const api = window.entityGraph

/** The two top-level pages: the editor (default) and the source configuration. */
export type Page = 'editor' | 'sources'

export interface AppActions {
  /** Rename the current user (blank ⇒ "anonymous"). */
  setUser: (name: string) => Promise<void>
  /** Switch the visible page. */
  setPage: (page: Page) => void
  /**
   * Pick which source the editor shows. Persists the choice, (re)opens it, and
   * jumps to the editor so the selection is immediately visible.
   */
  selectSource: (source: CurrentSource) => Promise<void>
}

export interface AppState {
  /** False until the persisted user + current source have loaded. */
  ready: boolean
  user: string
  page: Page
  /** The persisted current-source reference, or null if none picked yet. */
  current: CurrentSource | null
  /** The ephemeral handle for the open current source (null while unopened / failed). */
  active: ActiveSource | null
  /** Set when the current source could not be opened (server down, deleted, …). */
  openError: string | null
  actions: AppActions
}

/**
 * App-level logic: the current user, the active page, and the durable
 * "current source" the editor opens by default. Holds the latent state
 * (user / page / current ref) and derives the ephemeral {@link ActiveSource}
 * handle by opening that ref. Transport lives behind `window.entityGraph`, so
 * this hook never touches IPC details directly beyond those thin calls.
 */
export function useApp(): AppState {
  const [ready, setReady] = useState(false)
  const [user, setUserState] = useState('anonymous')
  const [page, setPage] = useState<Page>('editor')
  const [current, setCurrent] = useState<CurrentSource | null>(null)
  const [active, setActive] = useState<ActiveSource | null>(null)
  const [openError, setOpenError] = useState<string | null>(null)

  // The id of the open source, tracked outside React state so we can close the
  // previous handle before opening a new one without stale-closure races.
  const activeIdRef = useRef<string | null>(null)

  /** (Re)open a source reference into an ephemeral active handle. */
  const open = useCallback(async (ref: CurrentSource | null) => {
    if (activeIdRef.current) {
      void api.closeSource(activeIdRef.current)
      activeIdRef.current = null
    }
    setActive(null)
    setOpenError(null)
    if (!ref) return
    try {
      const opened = await api.openSource(ref.serverId, ref.sourceId, ref.label)
      activeIdRef.current = opened.id
      setActive(opened)
    } catch (e) {
      setOpenError(e instanceof Error ? e.message : String(e))
    }
  }, [])

  // On mount: load the persisted user + current source, then open the latter.
  useEffect(() => {
    void (async () => {
      const [u, cur] = await Promise.all([api.getUser(), api.getCurrentSource()])
      setUserState(u)
      setCurrent(cur)
      await open(cur)
      setReady(true)
    })()
  }, [open])

  const actions = useMemo<AppActions>(
    () => ({
      setUser: async (name) => {
        const next = name.trim() || 'anonymous'
        await api.setUser(next)
        setUserState(next)
      },
      setPage,
      selectSource: async (ref) => {
        await api.setCurrentSource(ref)
        setCurrent(ref)
        await open(ref)
        setPage('editor')
      },
    }),
    [open],
  )

  return { ready, user, page, current, active, openError, actions }
}
