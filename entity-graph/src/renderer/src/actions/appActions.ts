// The app-level command registry: everything the shell can do, defined once so
// the top-level hotkeys, the command palette, and the header buttons all
// dispatch through the same list. Mirrors editorActions.ts, one level up — the
// editor registry owns actions against the open tree, this one owns navigation,
// the palette, and the theme.
//
// Each action's `run` receives an AppController — a small imperative handle App
// provides — so the definitions here hold no state of their own.

import type { KeyBinding } from './keys'
import type { Page } from '../views/useApp'

/** The imperative operations the app shell exposes for actions to call. */
export interface AppController {
  togglePalette: () => void
  setPage: (page: Page) => void
  toggleTheme: () => void
}

export interface AppAction {
  id: string
  label: string
  /** Keys that trigger it. Omitted for actions only reachable via palette/button. */
  keys?: KeyBinding[]
  /** Faint category label shown in the palette when the action has no hotkey. */
  hint?: string
  /** Whether it appears in the command palette (default true). */
  palette?: boolean
  run: (c: AppController) => void
}

export const APP_ACTIONS: AppAction[] = [
  {
    id: 'toggle-palette',
    label: 'Open command palette',
    keys: [{ key: 'p', mod: true }],
    palette: false,
    run: (c) => c.togglePalette(),
  },
  { id: 'go-editor', label: 'Go to editor', hint: 'Navigate', run: (c) => c.setPage('editor') },
  { id: 'go-sources', label: 'Go to sources', hint: 'Navigate', run: (c) => c.setPage('sources') },
  { id: 'toggle-theme', label: 'Toggle theme', hint: 'Theme', run: (c) => c.toggleTheme() },
]
