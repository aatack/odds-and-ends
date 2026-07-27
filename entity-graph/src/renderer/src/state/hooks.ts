import { useEffect, useMemo, useRef, useSyncExternalStore } from 'react'
import type { GetEntities } from '../../../core/query'
import type { Atom } from './atom'
import {
  entityLabel,
  frameCrumbs,
  summaryOf,
  type Crumb,
  type EntitySummary,
  type FrameRows,
} from './derive'
import { entitiesAtom, entitiesFrom } from '../../../core/cache'
import type { EntitySource } from '../../../core/query'
import { focusRequestAtom, focusTaken } from './focusRequest'
import { rowLimitsAtom, rowsFrom } from './query'
import { loadResource, resourcesAtom, type ResourceState } from './resources'
import { callsAtom, focusOf, layoutAtom, pendingAtom, type Focus } from './store'
import { themeAtom, uiAtom, type Theme, type UiState } from './ui'
import type { LayoutState, PendingCall, RecordedCall } from './types'

// The only React in the state layer: thin subscriptions onto the atoms, plus the
// derivations views need. Components read from here and never own domain state.

export function useAtomValue<T>(atom: Atom<T>): T {
  return useSyncExternalStore(atom.subscribe, atom.get, atom.get)
}

export const useLayoutState = (): LayoutState => useAtomValue(layoutAtom)
export const usePendingCall = (): PendingCall | null => useAtomValue(pendingAtom)
export const useCalls = (): RecordedCall[] => useAtomValue(callsAtom)
export const useUi = (): UiState => useAtomValue(uiAtom)

/** The theme, applied to <html> so the token overrides take effect. */
export function useTheme(): Theme {
  const theme = useAtomValue(themeAtom)
  useEffect(() => {
    document.documentElement.classList.toggle('dark', theme === 'dark')
  }, [theme])
  return theme
}

/**
 * Take the keyboard whenever something asks for `target` — see
 * `state/focusRequest`. Also fires on mount if the standing request names this
 * field, which is how a field that a tool has just brought into existence gets
 * the caret without knowing it was the tool that did it.
 *
 * `take` is read through a ref, so an inline arrow doesn't re-fire it.
 */
export function useFocusRequest(target: string, take: () => void): void {
  const request = useAtomValue(focusRequestAtom)
  const latest = useRef(take)
  latest.current = take
  useEffect(() => {
    if (request.target !== target) return
    latest.current()
    focusTaken()
  }, [request, target])
}

export function useFocus(): Focus {
  const layout = useLayoutState()
  return useMemo(() => focusOf(layout), [layout])
}

/** The cache as the derivations read it: entities, plus how they are getting on. */
function useEntitySource(): EntitySource {
  const cache = useAtomValue(entitiesAtom)
  return useMemo(() => entitiesFrom(cache), [cache])
}

/**
 * The public face of the entity cache: hand it ids, get entities back — now,
 * synchronously, whether or not they have loaded. Anything missing is asked for
 * in the background, and the component re-renders when it lands.
 *
 * The function changes identity whenever the cache does, so a `useMemo` keyed on
 * it recomputes exactly when what it read might have changed.
 */
export const useGetEntities = (): GetEntities => useEntitySource().get

/** A frame's rows, stepped over the cache and recomputed as entities arrive. */
export function useFrameRows(frameId: string): FrameRows {
  const layout = useLayoutState()
  const source = useEntitySource()
  const limits = useAtomValue(rowLimitsAtom)
  return useMemo(
    () => rowsFrom(layout, source, limits, frameId),
    [layout, source, limits, frameId],
  )
}

/** What an entity says about itself in passing — what a pill is drawn from. */
export function useEntitySummary(id: string): EntitySummary {
  const get = useGetEntities()
  return useMemo(() => summaryOf(get([id])[id].values), [get, id])
}

/** An entity's display name. Asking for it is what loads it. */
export const useEntityLabel = (id: string): string => entityLabel(useEntitySource(), id)

/**
 * The bytes behind a `type: 'file'` row, fetched on first render. Asking is
 * idempotent, so several rows showing the same file share one fetch.
 */
export function useResource(id: string): ResourceState {
  const cache = useAtomValue(resourcesAtom)
  useEffect(() => {
    loadResource(id)
  }, [id])
  return cache[id] ?? { status: 'loading' }
}

/**
 * A file's name, if its bytes happen to be loaded already — and pointedly without
 * asking for them. Somewhere to put a file's real name (it lives with the
 * resource, not the entity) for the callers that only want to *name* the thing: a
 * tab title has no business pulling a screenshot over the wire.
 */
export function useLoadedFileName(id: string): string | null {
  const resource = useAtomValue(resourcesAtom)[id]
  return resource?.status === 'ready' ? resource.name : null
}

/** A tab's frame stack as a trail of entities, outermost first. */
export function useCrumbs(tabId: string | null): Crumb[] {
  const layout = useLayoutState()
  const source = useEntitySource()
  return useMemo(() => frameCrumbs(layout, source, tabId), [layout, source, tabId])
}
