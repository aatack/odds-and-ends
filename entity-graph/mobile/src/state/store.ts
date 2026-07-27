import { persistentAtom } from './atom'
import { defaultView, isViewState, type Theme, type ViewState } from './types'

// The atoms holding latent state, and the two accessors everything else uses.
// Runtime state (the query cache, summaries, resources, toasts, which sheet is
// open) lives beside its own machinery rather than here.

export const viewAtom = persistentAtom<ViewState>(
  'entity-graph-mobile.view',
  defaultView(),
  isViewState,
)

export const themeAtom = persistentAtom<Theme>(
  'entity-graph-mobile.theme',
  'system',
  (v) => v === 'light' || v === 'dark' || v === 'system',
)

export const getView = (): ViewState => viewAtom.get()

export const updateView = (fn: (s: ViewState) => ViewState): void => viewAtom.set(fn)
