// The search page's sections (the finder: the Tasks view, and every picker), as configured: one list for the whole
// app, kept on the `search` item (itemTypes.ts), which people change on its page and bots with set_search_sections.

/** egdb's filter (egdb-client's `Filter`), as a query section runs it over the app's events. */
export type Filter =
  | { eq: { key: string; value: unknown } }
  | { ne: { key: string; value: unknown } }
  | { lt: { key: string; value: number | string } }
  | { le: { key: string; value: number | string } }
  | { gt: { key: string; value: number | string } }
  | { ge: { key: string; value: number | string } }
  | { contains: { key: string; value: string } }
  | { contains_ignore_case: { key: string; value: string } }
  | { prefix: { key: string; value: string } }
  | { in: { key: string; values: unknown[] } }
  | { is_null: { key: string } }
  | { not_null: { key: string } }
  | { and: Filter[] }
  | { or: Filter[] }
  | { not: Filter }

/** The finder's own sections, built in: what was viewed, whose turn, what bots have, what waits, what closed today. */
export const BUILTIN_SECTIONS = ['viewed', 'needsYou', 'withBots', 'waiting', 'closed'] as const
export type BuiltinSection = (typeof BUILTIN_SECTIONS)[number]

/** One section: built in; a query (a filter over every event); or a function (JavaScript that answers rows). */
export type SearchSection =
  | { kind: 'builtin'; id: BuiltinSection; label?: string }
  | { kind: 'query'; label: string; filter: Filter; limit?: number }
  | { kind: 'function'; label: string; source: string }

/** A row a query or a function answers: what it opens, its type (for its icon), and its two sides, in markdown. */
export interface SearchRow {
  id: string
  type: string
  left: string
  right?: string
}

/** The sections when nothing is configured. */
export const DEFAULT_SECTIONS: SearchSection[] = BUILTIN_SECTIONS.map((id) => ({ kind: 'builtin', id }))

/** Whether a value-set (an event's values, and the keys made from its links) passes a filter. */
export function matches(values: Record<string, unknown>, f: Filter): boolean {
  const get = (key: string) => values[key]
  const cmp = (a: unknown, b: unknown) => (typeof a === 'number' && typeof b === 'number' ? a - b : String(a ?? '').localeCompare(String(b ?? '')))
  // A list (who has it) matches eq and in when any of it does.
  const eq = (a: unknown, b: unknown) => (Array.isArray(a) ? a.some((x) => JSON.stringify(x) === JSON.stringify(b)) : JSON.stringify(a ?? null) === JSON.stringify(b ?? null))
  if ('eq' in f) return eq(get(f.eq.key), f.eq.value)
  if ('ne' in f) return !eq(get(f.ne.key), f.ne.value)
  if ('lt' in f) return get(f.lt.key) != null && cmp(get(f.lt.key), f.lt.value) < 0
  if ('le' in f) return get(f.le.key) != null && cmp(get(f.le.key), f.le.value) <= 0
  if ('gt' in f) return get(f.gt.key) != null && cmp(get(f.gt.key), f.gt.value) > 0
  if ('ge' in f) return get(f.ge.key) != null && cmp(get(f.ge.key), f.ge.value) >= 0
  if ('contains' in f) return typeof get(f.contains.key) === 'string' && String(get(f.contains.key)).includes(f.contains.value)
  if ('contains_ignore_case' in f) {
    const v = get(f.contains_ignore_case.key)
    return typeof v === 'string' && v.toLowerCase().includes(f.contains_ignore_case.value.toLowerCase())
  }
  if ('prefix' in f) return typeof get(f.prefix.key) === 'string' && String(get(f.prefix.key)).startsWith(f.prefix.value)
  if ('in' in f) return f.in.values.some((v) => eq(get(f.in.key), v))
  if ('is_null' in f) return get(f.is_null.key) == null
  if ('not_null' in f) return get(f.not_null.key) != null
  if ('and' in f) return f.and.every((g) => matches(values, g))
  if ('or' in f) return f.or.some((g) => matches(values, g))
  if ('not' in f) return !matches(values, f.not)
  throw new Error(`Not a filter: ${JSON.stringify(f).slice(0, 100)}`)
}
