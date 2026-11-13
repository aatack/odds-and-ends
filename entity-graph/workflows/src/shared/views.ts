// What the four sizes (avatar, pill, card, focus: the window's sizes.tsx) share between the window and the backend,
// which writes them as text for bots (engine/views.ts): how each type fills the view, and an item's name as a pill.

import type { ItemKind } from './types'

/**
 * The views every item has, whatever its type (beside Debug, which is the same for everything): its focus (its type's
 * own: a file itself, a bot's settings, anything else's details), its comments (a chat), its notes (a tree), or its
 * assignments (what is assigned to it, then what it owns: a person's or a bot's home). An item's `display` value says
 * which it opens on; without one, its type does (`DISPLAY`).
 */
export type Display = 'focus' | 'comments' | 'notes' | 'assignments'
export const DISPLAYS: { id: Display; label: string }[] = [
  { id: 'focus', label: 'Focus' },
  { id: 'comments', label: 'Comments' },
  { id: 'notes', label: 'Notes' },
  { id: 'assignments', label: 'Assignments' },
]

/** The view each type opens on, unless the item says (`display`). */
export const DISPLAY: Record<ItemKind, Display> = {
  topic: 'comments',
  bot: 'assignments',
  user: 'assignments',
  script: 'comments',
  link: 'focus',
  github_pr: 'focus',
  slack_message: 'focus',
  slack_channel: 'focus',
  file: 'focus',
}

/** The view an item opens on: its `display`, else its type's. */
export const displayOf = (i: { type: ItemKind; params: Record<string, unknown> }): Display =>
  DISPLAYS.some((d) => d.id === i.params.display) ? (i.params.display as Display) : (DISPLAY[i.type] ?? 'focus')

/** How an item reads at its focus as text, for bots: its comments as a chat, its notes as a tree, or its details. */
export const focusKind = (i: { type: ItemKind; params: Record<string, unknown> }): 'chat' | 'details' | 'notes' => {
  const d = displayOf(i)
  return d === 'comments' ? 'chat' : d === 'notes' ? 'notes' : 'details'
}

/** An item's name as its pill shows it (`[id]()` in any text): the first line of what it says. */
export const pillLabel = (i: { summary: string }): string => i.summary.split('\n')[0]
