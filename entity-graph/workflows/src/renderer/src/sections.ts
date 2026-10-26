import type { Item, Overview } from '../../shared/types'
import { withBot } from './icons'

/**
 * The finder's sections: each a way to pick out items worth seeing first.
 * Every finder (the Tasks view, the pickers, `@` in the chat input) says
 * which it shows, in its order, and how many rows each shows before "n more".
 *
 * Still to come, once there is something to fill them: happening now (meetings
 * and the like, from a calendar), Slack channels, GitHub branches, topics, MCP
 * servers, and folders. Those are not items, so they wait on the finder picking
 * things other than items.
 */
export type SectionId = 'viewed' | 'needsYou' | 'withBots' | 'waiting' | 'closed'

export interface Section {
  label: string
  /** Its items, in the order shown: from every item the finder has, and the overview's recent lists. */
  items: (items: Item[], recent: Recent) => Item[]
}

export type Recent = Pick<Overview, 'viewed' | 'users'>

const open = (i: Item) => !i.closed
/** The items with these ids that the finder has, in the ids' order. */
const byIds = (ids: string[], items: Item[]): Item[] => {
  const by = new Map(items.map((i) => [i.id, i]))
  return ids.flatMap((id) => by.get(id) ?? [])
}

export const SECTIONS: Record<SectionId, Section> = {
  viewed: { label: 'Recently viewed', items: (items, r) => byIds(r.viewed, items) },
  needsYou: { label: 'Needs you', items: (items) => items.filter((i) => open(i) && i.withYou) },
  withBots: { label: 'With bots', items: (items, r) => items.filter((i) => open(i) && !i.withYou && withBot(i, r.users)) },
  waiting: { label: 'Waiting', items: (items, r) => items.filter((i) => open(i) && !i.withYou && !withBot(i, r.users)) },
  closed: { label: 'Closed today', items: (items) => items.filter((i) => i.closed) },
}

/** The Tasks view's sections: every item, by whose turn it is. */
export const TASKS_SECTIONS: SectionId[] = ['needsYou', 'withBots', 'waiting', 'closed']
/** A picker's: what was in hand lately, then the rest. */
export const PICKER_SECTIONS: SectionId[] = ['viewed', 'needsYou', 'withBots', 'waiting', 'closed']
