// The sizes anything is drawn at, as text (shared/text.ts, which the window uses too, to copy an item as text without
// asking the backend): what a bot reads of an item, in its prompt and from get_item. A bot is given ids, to refer to
// what it reads.

import type { Who } from '../shared/text'

export { cardText, focusText, pillText, viewText, FOCUS_LIMIT, type Reader, type Size, type Who } from '../shared/text'

export const ANYONE: Who = { name: (a) => a, person: (a) => a === 'human' || a === 'mcp' }
