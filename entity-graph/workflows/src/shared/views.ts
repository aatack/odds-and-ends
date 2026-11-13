// What the four sizes (avatar, pill, card, focus: the window's sizes.tsx) share between the window and the backend,
// which writes them as text for bots (engine/views.ts): how each type fills the view, and an item's name as a pill.

import type { ItemKind } from './types'

/**
 * How each type fills the view on its own: as a chat (its comments, and the chat input), as its details, or as the tree
 * of its notes (`NotesView`). A topic says which by its `display` (`focusKind`).
 */
export const FOCUS: Record<ItemKind, 'chat' | 'details' | 'notes'> = {
  topic: 'chat',
  bot: 'chat',
  approval: 'details',
  script: 'details',
  link: 'details',
  search: 'details',
  slack_message: 'details',
  slack_channel: 'details',
  // Its own view (FileFocus): the file itself, filling it.
  file: 'details',
  user: 'details',
}

/** How an item fills the view: a topic by its `display` (its comments as a chat, the default, or its notes as a tree); anything else by its type. */
export const focusKind = (i: { type: ItemKind; params: Record<string, unknown> }): 'chat' | 'details' | 'notes' =>
  i.type === 'topic' ? (i.params.display === 'notes' ? 'notes' : 'chat') : (FOCUS[i.type] ?? 'details')

/** An item's name as its pill shows it (`[id]()` in any text): the first line of what it says. */
export const pillLabel = (i: { summary: string }): string => i.summary.split('\n')[0]
