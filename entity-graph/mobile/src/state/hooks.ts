import { useEffect, useMemo, useSyncExternalStore } from 'react'
import { capabilitiesAtom, connectionAtom, type Connection } from '../source/connection'
import type { Atom } from './atom'
import { crumbs, entityLabel, viewRows, type Crumb, type ViewRows } from './derive'
import { pagesAtom, summariesAtom, type EntitySummary } from './query'
import { loadResource, resourcesAtom, type ResourceState } from './resources'
import { themeAtom, viewAtom } from './store'
import { sheetAtom, type Sheet } from './ui'
import { toastsAtom, type Toast } from './toast'
import { undoAtom, type UndoStep } from './undo'
import type { Theme, ViewState } from './types'

// The only React in the state layer: thin subscriptions onto the atoms, plus the
// derivations views need. Components read from here and never own domain state.

export function useAtomValue<T>(atom: Atom<T>): T {
  return useSyncExternalStore(atom.subscribe, atom.get, atom.get)
}

export const useView = (): ViewState => useAtomValue(viewAtom)
export const useConnection = (): Connection | null => useAtomValue(connectionAtom)
export const useCapabilities = (): string[] | null => useAtomValue(capabilitiesAtom)
export const useSheet = (): Sheet | null => useAtomValue(sheetAtom)
export const useToasts = (): Toast[] => useAtomValue(toastsAtom)
export const useUndoStack = (): UndoStep[] => useAtomValue(undoAtom)

/** Whether the open source exposes a tool — how undo and files are detected. */
export const useCanCall = (toolId: string): boolean =>
  useAtomValue(capabilitiesAtom)?.includes(toolId) ?? false

/** The theme, applied to <html> so the token overrides take effect. */
export function useTheme(): Theme {
  const theme = useAtomValue(themeAtom)
  useEffect(() => {
    const dark =
      theme === 'dark' ||
      (theme === 'system' && window.matchMedia('(prefers-color-scheme: dark)').matches)
    document.documentElement.classList.toggle('dark', dark)
  }, [theme])
  // Following the system means following it as it changes, not as it was at start.
  useEffect(() => {
    if (theme !== 'system') return
    const media = window.matchMedia('(prefers-color-scheme: dark)')
    const apply = (): void => {
      document.documentElement.classList.toggle('dark', media.matches)
    }
    media.addEventListener('change', apply)
    return () => media.removeEventListener('change', apply)
  }, [theme])
  return theme
}

/** The rows on screen. Recomputed when the state or the cache moves. */
export function useRows(): ViewRows {
  const view = useView()
  const pages = useAtomValue(pagesAtom)
  return useMemo(() => viewRows(view), [view, pages])
}

/** An entity's display name, from whatever has been harvested so far. */
export const useEntityLabel = (id: string): string => entityLabel(useAtomValue(summariesAtom), id)

export const useEntitySummary = (id: string): EntitySummary | undefined =>
  useAtomValue(summariesAtom)[id]

/** The navigation stack as a trail of names, outermost first. */
export function useCrumbs(): Crumb[] {
  const view = useView()
  const summaries = useAtomValue(summariesAtom)
  return useMemo(() => crumbs(view, summaries), [view, summaries])
}

/** The bytes behind a file row, fetched on first render. */
export function useResource(id: string): ResourceState {
  const cache = useAtomValue(resourcesAtom)
  useEffect(() => loadResource(id), [id])
  return cache[id] ?? { status: 'loading' }
}
