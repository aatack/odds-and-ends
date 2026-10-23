import type { Item } from '../../shared/types'

/** How items are grouped, in the sidebar and the items view: the human's turn, a bot's, waiting on something else, done. */
export const GROUPS: { label: string; has: (i: Item) => boolean }[] = [
  { label: 'Needs you', has: (i) => i.withHuman },
  { label: 'With bots', has: (i) => !i.withHuman && !i.closed && i.blockers.some((b) => b.type === 'bot') },
  { label: 'Waiting', has: (i) => !i.withHuman && !i.closed && !i.blockers.some((b) => b.type === 'bot') },
  { label: 'Closed today', has: (i) => i.closed },
]
