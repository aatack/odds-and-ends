// What the four sizes (avatar, pill, card, focus: the window's sizes.tsx) share between the window and the backend,
// which writes them as text for bots (engine/views.ts): how each type fills the view, and an item's name as a pill.

import type { ItemKind } from './types'

/**
 * How each type fills the view on its own: as a chat (its items, and the chat input), as its details, or (a note) as
 * the tree of notes in it (`NotesView`).
 */
export const FOCUS: Record<ItemKind, 'chat' | 'details' | 'notes'> = {
  task: 'chat',
  bot: 'chat',
  note: 'notes',
  approval: 'details',
  script: 'details',
  process: 'details',
  widget: 'details',
  link: 'details',
  search: 'details',
  slack_message: 'details',
  slack_channel: 'details',
  // Its own view (FileFocus): the file itself, filling it.
  file: 'details',
  user: 'details',
}

/** An item's name as its pill shows it (`[id]()` in any text): the first line of what it says. */
export const pillLabel = (i: { summary: string }): string => i.summary.split('\n')[0]
