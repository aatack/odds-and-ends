// The styles to try, for now: a theme and a serif for what the human typed,
// chosen in the menu and kept in local storage. Applied as data attributes on
// <html>, which themes.css reads. Temporary, until one is picked.

import { atomWithStorage, createJSONStorage } from 'jotai/utils'

export const THEMES = [
  { id: 'linear', name: 'Linear' },
  { id: 'notebook', name: 'Notebook' },
  { id: 'terminal', name: 'Terminal' },
] as const

export const SERIFS = [
  { id: 'none', name: 'None' },
  { id: 'lora', name: 'Lora' },
  { id: 'source-serif', name: 'Source Serif' },
  { id: 'newsreader', name: 'Newsreader' },
  { id: 'literata', name: 'Literata' },
  { id: 'crimson', name: 'Crimson Pro' },
] as const

export type Appearance = { theme: (typeof THEMES)[number]['id']; serif: (typeof SERIFS)[number]['id'] }

const KEY = 'workflows.appearance'
const DEFAULT: Appearance = { theme: 'linear', serif: 'lora' }

export const appearanceAtom = atomWithStorage<Appearance>(KEY, DEFAULT, createJSONStorage(() => localStorage), { getOnInit: true })

/** Puts an appearance on the page. Notebook is the one light theme. */
export function applyAppearance(a: Appearance): void {
  const html = document.documentElement
  html.dataset.theme = a.theme
  html.dataset.serif = a.serif
  html.classList.toggle('dark', a.theme !== 'notebook')
}

/** The stored appearance, read before the first render so nothing flashes. */
export function storedAppearance(): Appearance {
  try {
    return { ...DEFAULT, ...JSON.parse(localStorage.getItem(KEY) ?? '{}') }
  } catch {
    return DEFAULT
  }
}
