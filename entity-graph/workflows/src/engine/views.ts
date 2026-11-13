// The sizes anything is drawn at, as text (shared/text.ts, which the window's default pills and cards use too): what a
// bot reads of an item, in its prompt and from get_item. A bot is given ids, to refer to what it reads.

import { cardText, focusText, pillText, type FocusOptions, type Reader, type Who } from '../shared/text'

export { cardText, focusText, pillText, FOCUS_LIMIT, type Reader, type Who } from '../shared/text'

export type Size = 'pill' | 'card' | 'focus'

export const ANYONE: Who = { name: (a) => a, person: (a) => a === 'human' || a === 'mcp' }

/** An item at a size, as text; null if there is no such item. */
export function viewText(read: Reader, id: string, size: Size, opts: FocusOptions = {}): string | null {
  if (size === 'focus') return focusText(read, id, opts)
  const info = read.info(id)
  return !info ? null : size === 'pill' ? pillText(info, opts) : cardText(info, opts)
}
