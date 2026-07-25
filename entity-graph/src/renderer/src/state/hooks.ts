import { useEffect, useLayoutEffect, useMemo, useSyncExternalStore } from 'react'
import type { Atom } from './atom'
import { frameRows, tabLabel, type FrameRows } from './derive'
import { namesAtom, queryAtom, retainFrame } from './query'
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

export function useFocus(): Focus {
  const layout = useLayoutState()
  return useMemo(() => focusOf(layout), [layout])
}

/**
 * A frame's rows. Retaining it tells the query engine to keep it loaded, so a
 * frame that scrolls out of the layout stops being fetched.
 */
export function useFrameRows(frameId: string): FrameRows {
  const layout = useLayoutState()
  const cache = useAtomValue(queryAtom)
  // Retained before paint, so the frame reads as loading straight away rather
  // than flashing "No entities." for one frame while the fetch is scheduled.
  useLayoutEffect(() => retainFrame(frameId), [frameId])
  return useMemo(() => frameRows(layout, cache, frameId), [layout, cache, frameId])
}

export function useTabLabel(tabId: string): string {
  const layout = useLayoutState()
  const names = useAtomValue(namesAtom)
  return useMemo(() => tabLabel(layout, names, tabId), [layout, names, tabId])
}
