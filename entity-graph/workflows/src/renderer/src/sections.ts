import type { Item, Overview } from '../../shared/types'

/**
 * The finder's sections, the same in every finder (home, an item's Assignments, every item picker), in this order:
 * each a way to pick out items worth seeing first, for whoever the finder is for.
 */
export type SectionId = 'assigned' | 'owned' | 'pinned' | 'unassigned' | 'everything' | 'history'
export const FINDER_SECTIONS: SectionId[] = ['history', 'assigned', 'owned', 'pinned', 'unassigned', 'everything']

export interface Section {
  label: string
  /** Its items, in the order shown: from every item the finder has, and what `Recent` brings. */
  items: (items: Item[], recent: Recent) => Item[]
}

/**
 * What the finder reads as recent: what was viewed here (kept by the app, not the store), and who there is. `who` is
 * whose finder it is (home's picker: people, bots, any of them; an item's Assignments: it): the person signed in,
 * unless it says.
 */
export type Recent = {
  me: string
  viewed: string[]
  users: Overview['users']
  bots: { id: string }[]
  who?: string[]
  /** What is pinned to whoever it is for (`finder`, read by the finder). */
  pinned?: Item[]
  /** What was assigned to whoever it is for and no longer is, the latest first (`finder`, read by the finder). */
  unassigned?: Item[]
  /** What Back goes through (the views below the one on screen), the nearest first (read by the finder). */
  history?: Item[]
}

/** Whose finder it is: `who`, or the person signed in. */
const whose = (r: Recent): string[] => (r.who?.length ? r.who : [r.me])
const open = (i: Item) => !i.closed

export const SECTIONS: Record<SectionId, Section> = {
  assigned: { label: 'Assigned', items: (items, r) => items.filter((i) => open(i) && i.assignees.some((a) => whose(r).includes(a))) },
  owned: {
    label: 'Owned',
    items: (items, r) => items.filter((i) => open(i) && !!i.owner && whose(r).includes(i.owner) && !i.assignees.some((a) => whose(r).includes(a)) && i.open !== null),
  },
  pinned: { label: 'Pinned', items: (_items, r) => r.pinned ?? [] },
  unassigned: { label: 'Recently unassigned', items: (_items, r) => r.unassigned ?? [] },
  // Only while something is typed (the finder skips it otherwise): whatever matches that no section above has.
  everything: { label: 'Everything else', items: (items) => items },
  history: { label: 'History', items: (_items, r) => r.history ?? [] },
}
