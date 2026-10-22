import { atom, persistentAtom } from './atom'

// Shell state that isn't part of the entity graph: which page is showing, which
// panels are open, the colour theme. Runtime except for the theme, which is a
// user preference worth surviving a reload.

export type Page = 'editor' | 'sources'
export type Theme = 'light' | 'dark'

export interface UiState {
  page: Page
  /** The activity drawer listing finished calls. */
  activityOpen: boolean
  /** Entity whose raw values and links are being inspected, if any. */
  inspectEntityId: string | null
  /** File entity being shown full size over the tree, if any. */
  resourceId: string | null
  /** A debug surface open over everything, if any. */
  debug: 'schemaForm' | 'schemaBuilder' | null
}

export const uiAtom = atom<UiState>({
  page: 'editor',
  activityOpen: false,
  inspectEntityId: null,
  resourceId: null,
  debug: null,
})

export const updateUi = (patch: Partial<UiState>): void =>
  uiAtom.set((s) => ({ ...s, ...patch }))

export const themeAtom = persistentAtom<Theme>(
  'entity-graph.theme',
  'light',
  (v) => v === 'light' || v === 'dark',
)

export const toggleTheme = (): void => themeAtom.set((t) => (t === 'dark' ? 'light' : 'dark'))

/**
 * The dialogs open now, innermost last, each by how it closes. Runtime only. A
 * dialog owns the keyboard while it is up: the key router hands every key to it
 * rather than to the frames behind, and Escape closes the innermost.
 */
export const modalsAtom = atom<readonly (() => void)[]>([])

/** Note a dialog as open; the returned function notes it closed. */
export function openModal(close: () => void): () => void {
  modalsAtom.set((list) => [...list, close])
  return () => modalsAtom.set((list) => list.filter((c) => c !== close))
}
