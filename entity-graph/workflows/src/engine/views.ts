// The sizes anything is drawn at (the window's sizes.tsx), as text: what a bot reads of an item, in its prompt and
// from get_item. Each size is built from the smaller ones: a pill is the item's type and name; a card is what a chat
// shows of it; a focus is the whole of it, as its view fills the window (shared/views.ts `FOCUS`): a chat as the cards
// of what is in it, a note as its tree (each note in it at focus, anything else as its pill), anything else as its
// values. An avatar has no text.

import type { ItemInfo } from '../shared/types'
import { FOCUS, pillLabel } from '../shared/views'

export type Size = 'pill' | 'card' | 'focus'

/** Who did something, for a card: their name, and whether they are a person (whose words go to the bot as they are). */
export interface Who {
  name: (actor: string) => string
  person: (actor: string) => boolean
}

export const ANYONE: Who = { name: (a) => a, person: (a) => a === 'human' || a === 'mcp' }

/** What the sizes read: an item, and what is in it, oldest first. The engine is one. */
export interface Reader {
  info(id: string): ItemInfo | null
  children(id: string): ItemInfo[]
}

/** How many lines a focus is cut to, with what was cut said and the id to ask for the rest. */
export const FOCUS_LIMIT = 100

/** Inline, as `[id]()` is drawn: its type (the pill's icon), its name, and its id. */
export const pillText = (i: ItemInfo): string => `[${i.type}] ${pillLabel(i)} (${i.id})`

/** In a chat: what a person wrote, as they wrote it, after their name; anything else in square brackets. */
export function cardText(e: ItemInfo, who: Who = ANYONE): string {
  if (e.type === 'note')
    return who.person(e.createdBy) ? `${who.name(e.createdBy)}: ${e.params.text ?? ''}` : `[${who.name(e.createdBy)}: ${e.params.text ?? ''}]`
  if (e.cancelled) return `[${e.message ?? `Cancelled: ${e.summary}`}]`
  if (!e.resolved) return `[${e.type} added by ${who.name(e.createdBy)}: ${e.summary}]`
  return `[${e.message ?? `${e.summary}: done`}]`
}

/** A note as a row of a tree: a heading `#`, a checkbox `[ ]` or `[x]`, then what it says, on one line, and its id. */
const noteRow = (i: ItemInfo): string => {
  const p = i.params
  const mark = `${p.section === true ? '# ' : ''}${p.open === true ? '[ ] ' : p.open === false ? '[x] ' : ''}`
  return `${mark}${String(p.text ?? '').replace(/\n+/g, ' ')} (${i.id})`
}

/** The whole of an item, as its view shows it, under its pill; at most `limit` lines below that. */
export function focusText(read: Reader, id: string, who: Who = ANYONE, limit = FOCUS_LIMIT): string | null {
  const info = read.info(id)
  if (!info) return null
  const body: string[] = []
  const kind = FOCUS[info.type]
  if (kind === 'notes') {
    // Depth first, oldest first, walking into notes only; what doesn't fit is said, with the note to ask for.
    const count = (i: ItemInfo): number => 1 + (i.type === 'note' ? read.children(i.id).reduce((n, k) => n + count(k), 0) : 0)
    const cut: string[] = []
    const walk = (parent: string, depth: number) => {
      const kids = read.children(parent)
      for (let n = 0; n < kids.length; n++) {
        if (body.length >= limit) {
          cut.push(`... ${kids.slice(n).reduce((m, k) => m + count(k), 0)} more under ${parent}: get_item it for the rest`)
          return
        }
        const k = kids[n]
        body.push(`${'  '.repeat(depth)}- ${k.type === 'note' ? noteRow(k) : pillText(k)}`)
        if (k.type === 'note') walk(k.id, depth + 1)
      }
    }
    walk(id, 0)
    body.push(...cut)
  } else if (kind === 'chat') {
    // The latest `limit` cards; the earlier ones said, to ask for.
    const cards = read.children(id).map((e) => cardText(e, who))
    if (cards.length > limit) body.push(`... ${cards.length - limit} earlier: get_children ${id} for them`)
    body.push(...cards.slice(-limit))
  } else {
    for (const [k, v] of Object.entries(info.params).slice(0, limit)) body.push(`${k}: ${(typeof v === 'string' ? v : JSON.stringify(v)).replace(/\n+/g, ' ').slice(0, 300)}`)
  }
  return [pillText(info), ...body].join('\n')
}

/** An item at a size, as text. */
export function viewText(read: Reader, id: string, size: Size, who: Who = ANYONE): string | null {
  if (size === 'focus') return focusText(read, id, who)
  const info = read.info(id)
  return !info ? null : size === 'pill' ? pillText(info) : cardText(info, who)
}
