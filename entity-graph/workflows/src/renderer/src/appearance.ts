// The styles to try, for now: a theme and a serif for what the human typed,
// chosen in the menu and kept in local storage. Applied as data attributes on
// <html>, which themes.css reads. Temporary, until one is picked.

import { atomWithStorage, createJSONStorage } from 'jotai/utils'

export const THEMES = [
  { id: 'corporate', name: 'Corporate' },
  { id: 'notebook', name: 'Notebook' },
  { id: 'terminal', name: 'Terminal' },
  { id: 'dotcom', name: 'Dotcom' },
  { id: 'discord', name: 'Discord' },
  { id: 'classic', name: 'Classic Mac' },
  { id: 'demo', name: 'Demo' },
] as const

export const SERIFS = [
  { id: 'none', name: 'None' },
  { id: 'lora', name: 'Lora' },
  { id: 'source-serif', name: 'Source Serif' },
  { id: 'newsreader', name: 'Newsreader' },
  { id: 'literata', name: 'Literata' },
  { id: 'crimson', name: 'Crimson Pro' },
] as const

export const MODES = [
  { id: 'dark', name: 'Dark' },
  { id: 'light', name: 'Light' },
] as const

export type Appearance = { theme: (typeof THEMES)[number]['id']; serif: (typeof SERIFS)[number]['id']; mode: (typeof MODES)[number]['id'] }

const KEY = 'workflows.appearance'
const DEFAULT: Appearance = { theme: 'corporate', serif: 'lora', mode: 'dark' }

export const appearanceAtom = atomWithStorage<Appearance>(KEY, DEFAULT, createJSONStorage(() => localStorage), { getOnInit: true })

/** Puts an appearance on the page: every theme comes light or dark (`.dark` on <html>, as entity-graph's tokens expect). */
export function applyAppearance(a: Appearance): void {
  const html = document.documentElement
  html.dataset.theme = a.theme
  html.dataset.serif = a.serif
  html.dataset.mode = a.mode
  html.classList.toggle('dark', a.mode === 'dark')
}

/** The stored appearance, read before the first render so nothing flashes. */
export function storedAppearance(): Appearance {
  try {
    return { ...DEFAULT, ...JSON.parse(localStorage.getItem(KEY) ?? '{}') }
  } catch {
    return DEFAULT
  }
}
