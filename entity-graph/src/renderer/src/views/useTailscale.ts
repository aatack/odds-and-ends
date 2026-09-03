import { useCallback, useEffect, useMemo, useState } from 'react'
import type { TailscaleView } from '../../../core/client'
import { APP_MOUNT, phoneAppUrl, sourceMount } from '../../../core/client'

const graph = window.entityGraph

/**
 * One switch's worth of state, derived rather than stored: whether the mount is
 * published, whether something else has it, and whether it can be moved.
 */
export interface MountState {
  /** Published, and pointing where this app would point it. */
  on: boolean
  /** Occupied by something else — what it actually serves. */
  conflict: string | null
  /** A change is in flight. */
  busy: boolean
  /** Why the switch can't be moved, if it can't. */
  blocked: string | null
}

export interface TailscaleActions {
  refresh: () => Promise<void>
  /** Publish or unpublish the phone app's build at the root of the tailnet name. */
  serveApp: (on: boolean) => Promise<void>
  /** Publish or unpublish one broadcast node at its own path. */
  serveNode: (nodeId: string, on: boolean) => Promise<void>
  /** A link that hands a phone the whole connection, token included. */
  phoneLink: (nodeId: string, author: string) => Promise<string>
}

export interface TailscaleModel {
  /** Null until the first read comes back. */
  view: TailscaleView | null
  /** The last failure from a switch, cleared when the next one is thrown. */
  error: string | null
  /** Where the phone app is opened, once there is a name to open it on. */
  appUrl: string | null
  /** The phone app's own switch. */
  app: MountState
  /** One broadcast's switch, and the URL it is reachable at once it is on. */
  node: (nodeId: string, localUrl: string | null) => MountState & { url: string | null }
  actions: TailscaleActions
}

/**
 * All the logic behind the phone-access controls: what Tailscale is currently
 * publishing, and the two switches that change it.
 *
 * **Unwired**, along with the components that read it — see
 * `components/PhoneAccess.tsx`. It comes back when serving does, as a node.
 *
 * Held in one place and passed down rather than called per row, because there is
 * one serve config for the machine — a hook instance per node would be as many
 * reads of the same thing, and they would disagree while one of them was mid-flight.
 */
export function useTailscale(): TailscaleModel {
  const [view, setView] = useState<TailscaleView | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [pending, setPending] = useState<Set<string>>(new Set())

  const refresh = useCallback(async () => {
    try {
      setView(await graph.tailscaleStatus())
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [])

  useEffect(() => {
    void refresh()
  }, [refresh])

  /** Run a change with its mount marked busy, then re-read what actually happened. */
  const change = useCallback(
    async (mount: string, fn: () => Promise<void>) => {
      setError(null)
      setPending((p) => new Set(p).add(mount))
      try {
        await fn()
      } catch (e) {
        setError(e instanceof Error ? e.message : String(e))
      } finally {
        setPending((p) => {
          const next = new Set(p)
          next.delete(mount)
          return next
        })
        await refresh()
      }
    },
    [refresh],
  )

  const actions = useMemo<TailscaleActions>(
    () => ({
      refresh,
      serveApp: (on) => change(APP_MOUNT, () => graph.tailscaleServeApp(on)),
      serveNode: (nodeId, on) =>
        change(sourceMount(nodeId), () => graph.tailscaleServeNode(nodeId, on)),
      phoneLink: (nodeId, author) => graph.tailscalePhoneLink(nodeId, author),
    }),
    [refresh, change],
  )

  /**
   * A mount is "on" only when it points where this app would point it. Anything
   * else there is a conflict rather than a state of the switch: it was put there
   * by other means, and flipping a switch shouldn't silently take it over.
   */
  const state = useCallback(
    (mount: string, target: string): MountState => {
      const busy = pending.has(mount)
      if (!view) return { on: false, conflict: null, busy, blocked: 'Still reading Tailscale’s state.' }
      if (!view.running) return { on: false, conflict: null, busy, blocked: view.problem }

      const handler = view.handlers.find((h) => h.mount === mount)
      const on = handler?.target === target
      const conflict = handler && !on ? handler.target : null

      // Adding is one idempotent command, but removing means clearing the whole
      // serve config and rebuilding it — so only turning something *off* can be
      // refused by a config the app can't put back.
      const blocked = conflict
        ? `${mount} already serves ${conflict}.`
        : on && !view.editable
          ? `Can’t remove this: ${view.locked}.`
          : null
      return { on, conflict, busy, blocked }
    },
    [view, pending],
  )

  const node = useCallback(
    (nodeId: string, localUrl: string | null) => {
      const mount = sourceMount(nodeId)
      const mountState = state(mount, localUrl ?? '')
      return {
        ...mountState,
        url: view?.domain && mountState.on ? `https://${view.domain}${mount}` : null,
      }
    },
    [state, view],
  )

  return {
    view,
    error,
    appUrl: view?.domain ? phoneAppUrl(view.domain) : null,
    app: state(APP_MOUNT, view?.app.path ?? ''),
    node,
    actions,
  }
}
