// Every item as text, at the sizes it is drawn at: what would appear in each view, for passing to a model, and what the
// window's default pills and cards draw (sizes.tsx). A pill is its short title; a card, whatever text it holds; a focus,
// its default view as text (a chat as its messages, a tree of notes as nested bullets, anything else as its values).
// `ids` adds each item's id, for a reader that will refer to them (a bot); a person's window leaves them out.

import type { ItemInfo } from './types'
import { focusKind } from './views'

/** The longest a pill's title runs before it is cut, with an ellipsis. */
export const PILL_MAX = 60
/** How many messages, rows or values a focus shows when not told. */
export const FOCUS_LIMIT = 100

/** Who did something, for a card: their name, and whether they are a person (whose words go to a bot as they are). */
export interface Who {
  name: (actor: string) => string
  person: (actor: string) => boolean
}

/** What a focus reads: an item, its comments and its notes, oldest first. The engine is one. */
export interface Reader {
  info(id: string): ItemInfo | null
  children(id: string): ItemInfo[]
  notes(id: string): ItemInfo[]
}

export type TextOptions = { ids?: boolean; who?: Who }
export type FocusOptions = TextOptions & { offset?: number; limit?: number }

const cut = (s: string, n: number): string => (s.length > n ? `${s.slice(0, n - 1).trimEnd()}…` : s)

/** An item's short title: its first line, cut to `PILL_MAX` with an ellipsis. With `ids`, its type before and its id after. */
export function pillText(i: ItemInfo, { ids = false }: TextOptions = {}): string {
  const title = cut(i.summary.split('\n')[0].trim(), PILL_MAX)
  return ids ? `[${i.type}] ${title} (${i.id})` : title
}

/** A topic's mark, by its status: `[ ]` open, `[x]` closed, nothing for none. */
const mark = (i: ItemInfo): string => (i.type !== 'topic' ? '' : i.params.open === true ? '[ ] ' : i.params.open === false ? '[x] ' : '')

/**
 * Whatever text an item holds, all of it, and nothing else: a topic's text (every paragraph), after its status mark;
 * anything else what it says (its message once it has one, else its summary). With `ids`, its id after.
 */
export function cardText(e: ItemInfo, { ids = false }: { ids?: boolean } = {}): string {
  const text = e.type === 'topic' ? `${mark(e)}${String(e.params.text ?? '')}` : e.cancelled ? (e.message ?? `Cancelled: ${e.summary}`) : !e.resolved ? e.summary : (e.message ?? e.summary)
  return ids ? `${text} (${e.id})` : text
}

/** Lines with their ids in a column on the left, as wide as the widest. */
const withIds = (rows: { id: string; text: string }[]): string[] => {
  const w = Math.max(0, ...rows.map((r) => r.id.length))
  return rows.map((r) => `${r.id.padEnd(w)}  ${r.text}`)
}

/**
 * An item's default view as text, under its pill. A chat: its messages' cards, the latest `limit` (`offset` skips that
 * many of the latest first), each after who wrote it given `who`; a tree of notes: nested bullets, the first `limit` rows (`offset` skips that many from the
 * top), walking into topics' notes; anything else: its values. What is left out is said, with the offset to ask for
 * it. With `ids`, each item's id in a column on the left.
 */
export function focusText(read: Reader, id: string, opts: FocusOptions = {}): string | null {
  const info = read.info(id)
  if (!info) return null
  const { ids = false, who, offset = 0, limit = FOCUS_LIMIT } = opts
  const body: string[] = []
  const kind = focusKind(info)
  if (kind === 'notes') {
    // Every row, depth first, oldest first: a topic's text on one line, anything else as its pill.
    const rows: { id: string; text: string }[] = []
    const walk = (parent: string, depth: number) => {
      for (const k of read.notes(parent)) {
        const line = k.type === 'topic' ? `${k.params.section === true ? '# ' : ''}${mark(k)}${String(k.params.text ?? '').replace(/\n+/g, ' ')}` : pillText(k, { ids })
        rows.push({ id: k.id, text: `${'  '.repeat(depth)}- ${line}` })
        if (k.type === 'topic') walk(k.id, depth + 1)
      }
    }
    walk(id, 0)
    const shown = rows.slice(offset, offset + limit)
    if (offset > 0) body.push(`... ${Math.min(offset, rows.length)} rows above, left out (offset 0 for them)`)
    body.push(...(ids ? withIds(shown) : shown.map((r) => r.text)))
    if (offset + limit < rows.length) body.push(`... ${rows.length - offset - limit} more rows below (offset ${offset + limit} for them)`)
  } else if (kind === 'chat') {
    const all = read.children(id)
    const end = Math.max(0, all.length - offset)
    const start = Math.max(0, end - limit)
    // Each message after who wrote it, given `who`.
    const shown = all.slice(start, end).map((e) => ({ id: e.id, text: `${who ? `${who.name(e.createdBy)}: ` : ''}${cardText(e)}` }))
    if (start > 0) body.push(`... ${start} earlier messages, left out (offset ${offset + (end - start)} for them)`)
    body.push(...(ids ? withIds(shown) : shown.map((r) => r.text)))
    if (offset > 0) body.push(`... ${all.length - end} later messages, left out (offset 0 for them)`)
  } else {
    const values = Object.entries(info.params)
    for (const [k, v] of values.slice(offset, offset + limit)) body.push(`${k}: ${(typeof v === 'string' ? v : JSON.stringify(v)).replace(/\n+/g, ' ').slice(0, 300)}`)
    if (offset + limit < values.length) body.push(`... ${values.length - offset - limit} more values (offset ${offset + limit} for them)`)
  }
  return [pillText(info, { ids }), ...body].join('\n')
}

export type Size = 'pill' | 'card' | 'focus'

/** An item at a size, as text; null if there is no such item. The engine's for bots, and the window's own (to copy). */
export function viewText(read: Reader, id: string, size: Size, opts: FocusOptions = {}): string | null {
  if (size === 'focus') return focusText(read, id, opts)
  const info = read.info(id)
  return !info ? null : size === 'pill' ? pillText(info, opts) : cardText(info, opts)
}
