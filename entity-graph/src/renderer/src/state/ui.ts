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
  /** The source-level debug modal. */
  debugSource: boolean
}

export const uiAtom = atom<UiState>({
  page: 'editor',
  activityOpen: false,
  inspectEntityId: null,
  resourceId: null,
  debugSource: false,
})

export const updateUi = (patch: Partial<UiState>): void =>
  uiAtom.set((s) => ({ ...s, ...patch }))

export const themeAtom = persistentAtom<Theme>(
  'entity-graph.theme',
  'light',
  (v) => v === 'light' || v === 'dark',
)

export const toggleTheme = (): void => themeAtom.set((t) => (t === 'dark' ? 'light' : 'dark'))
