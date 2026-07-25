import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import type { ActiveSource, CurrentSource } from '../../../core/client'
import { updateUi } from '../state/ui'

const api = window.entityGraph

export interface AppActions {
  /** Rename the current user (blank ⇒ "anonymous"). */
  setUser: (name: string) => Promise<void>
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
  /** The persisted current-source reference, or null if none picked yet. */
  current: CurrentSource | null
  /** The ephemeral handle for the open source (null while unopened / failed). */
  active: ActiveSource | null
  /** Set when the current source could not be opened (server down, deleted, …). */
  openError: string | null
  actions: AppActions
}

/**
 * What the shell needs from the main process: the current user and the durable
 * "current source" reference, resolved into an open handle. Note what *isn't*
 * here — which page is showing, and every other piece of UI state, live in the
 * state layer where a tool can reach them.
 */
export function useApp(): AppState {
  const [ready, setReady] = useState(false)
  const [user, setUserState] = useState('anonymous')
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
      selectSource: async (ref) => {
        await api.setCurrentSource(ref)
        setCurrent(ref)
        await open(ref)
        updateUi({ page: 'editor' })
      },
    }),
    [open],
  )

  return { ready, user, current, active, openError, actions }
}
