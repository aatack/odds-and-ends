import { createContext } from 'react'
import { DEFAULT_SECTIONS, type SearchSection } from '../../shared/search'
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
export type SectionId = 'assigned' | 'owned' | 'viewed' | 'needsYou' | 'withBots' | 'waiting' | 'closed'

export interface Section {
  /** Its heading, which may depend on whose search page it is. */
  label: string | ((recent: Recent) => string)
  /** Its items, in the order shown: from every item the finder has, and the overview's recent lists. */
  items: (items: Item[], recent: Recent) => Item[]
}

/**
 * What the finder reads as recent: what was viewed here (kept by the app, not the store), and who there is. `who` is
 * whose search page it is (the Tasks view's picker: people, bots, any of them): the person signed in, unless it says.
 */
export type Recent = { me: string; viewed: string[]; users: Overview['users']; bots: { id: string }[]; who?: string[] }

/** Whose search page it is: `who`, or the person signed in. */
const whose = (r: Recent): string[] => (r.who?.length ? r.who : [r.me])
const yours = (r: Recent) => whose(r).length === 1 && whose(r)[0] === r.me

const open = (i: Item) => !i.closed
/** The items with these ids that the finder has, in the ids' order. */
const byIds = (ids: string[], items: Item[]): Item[] => {
  const by = new Map(items.map((i) => [i.id, i]))
  return ids.flatMap((id) => by.get(id) ?? [])
}

export const SECTIONS: Record<SectionId, Section> = {
  assigned: {
    label: (r) => (yours(r) ? 'Assigned to you' : 'Assigned'),
    items: (items, r) => items.filter((i) => open(i) && i.assignees.some((a) => whose(r).includes(a))),
  },
  owned: {
    label: (r) => (yours(r) ? 'You own' : 'Owned'),
    items: (items, r) => items.filter((i) => open(i) && !!i.owner && whose(r).includes(i.owner) && !i.assignees.some((a) => whose(r).includes(a)) && i.open !== null),
  },
  viewed: { label: 'Recently viewed', items: (items, r) => byIds(r.viewed, items) },
  needsYou: { label: 'Needs you', items: (items) => items.filter((i) => open(i) && i.withYou) },
  withBots: { label: 'With bots', items: (items, r) => items.filter((i) => open(i) && !i.withYou && withBot(i, r.bots)) },
  waiting: { label: 'Waiting', items: (items, r) => items.filter((i) => open(i) && !i.withYou && !withBot(i, r.bots)) },
  closed: { label: 'Closed today', items: (items) => items.filter((i) => i.closed) },
}


/** The search page's sections as configured (the `search` item; the defaults until it is read): every finder's, unless it says. */
/** A section's heading, as `recent` reads it. */
export const sectionLabel = (id: SectionId, recent: Recent): string => {
  const l = SECTIONS[id].label
  return typeof l === 'string' ? l : l(recent)
}

export const SearchSectionsContext = createContext<SearchSection[]>(DEFAULT_SECTIONS)
