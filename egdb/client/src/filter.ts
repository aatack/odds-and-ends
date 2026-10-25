// Filters, evaluated in the browser with the same rules as the server
// (DESIGN.md, "Find"): a key is absent when it was never set or was cleared;
// a comparison with an absent key is false; ranges compare numbers with
// numbers and text with text; text tests are case-sensitive except
// `contains_ignore_case`, which folds ASCII only.
//
// One difference: JSON has no integer type, so `1` and `1.0` are the same
// number here, while the server keeps `Int(1)` and `Float(1.0)` apart.

import type { EntityId, Filter, Value, Values } from './types.js'

export function matches(filter: Filter, id: EntityId, values: Values): boolean {
  if ('eq' in filter) return equals(values, filter.eq.key, filter.eq.value)
  if ('ne' in filter) return !equals(values, filter.ne.key, filter.ne.value)
  if ('lt' in filter) return range(values[filter.lt.key], filter.lt.value, (o) => o < 0)
  if ('le' in filter) return range(values[filter.le.key], filter.le.value, (o) => o <= 0)
  if ('gt' in filter) return range(values[filter.gt.key], filter.gt.value, (o) => o > 0)
  if ('ge' in filter) return range(values[filter.ge.key], filter.ge.value, (o) => o >= 0)
  if ('contains' in filter) {
    const text = values[filter.contains.key]
    return typeof text === 'string' && text.includes(filter.contains.value)
  }
  if ('contains_ignore_case' in filter) {
    const text = values[filter.contains_ignore_case.key]
    return (
      typeof text === 'string' && foldAscii(text).includes(foldAscii(filter.contains_ignore_case.value))
    )
  }
  if ('prefix' in filter) {
    const text = values[filter.prefix.key]
    return typeof text === 'string' && text.startsWith(filter.prefix.value)
  }
  if ('in' in filter) return filter.in.values.some((v) => equals(values, filter.in.key, v))
  if ('is_null' in filter) return !present(values, filter.is_null.key)
  if ('not_null' in filter) return present(values, filter.not_null.key)
  if ('and' in filter) return filter.and.every((f) => matches(f, id, values))
  if ('or' in filter) return filter.or.some((f) => matches(f, id, values))
  if ('not' in filter) return !matches(filter.not, id, values)
  throw new Error(`unknown filter ${JSON.stringify(filter)}`)
}

function present(values: Values, key: string): boolean {
  return values[key] !== undefined && values[key] !== null
}

function equals(values: Values, key: string, value: Value): boolean {
  if (value === null) return !present(values, key)
  return present(values, key) && sameValue(values[key], value)
}

/** Deep equality of two values (objects compare key by key, in any order). */
export function sameValue(a: Value | undefined, b: Value | undefined): boolean {
  if (a === b) return true
  if (typeof a !== 'object' || typeof b !== 'object' || a === null || b === null) return false
  if (Array.isArray(a) !== Array.isArray(b)) return false
  if (Array.isArray(a) && Array.isArray(b)) {
    return a.length === b.length && a.every((x, i) => sameValue(x, b[i]))
  }
  const ka = Object.keys(a)
  const kb = Object.keys(b)
  return (
    ka.length === kb.length &&
    ka.every((k) => sameValue((a as Record<string, Value>)[k], (b as Record<string, Value>)[k]))
  )
}

function range(value: Value | undefined, bound: number | string, ok: (order: number) => boolean): boolean {
  if (typeof bound === 'number' && typeof value === 'number') return ok(Math.sign(value - bound))
  if (typeof bound === 'string' && typeof value === 'string') return ok(compareText(value, bound))
  return false
}

/** Text in UTF-8 byte order, which is code point order (the server's order). */
export function compareText(a: string, b: string): number {
  if (a === b) return 0
  const ia = a[Symbol.iterator]()
  const ib = b[Symbol.iterator]()
  for (;;) {
    const x = ia.next()
    const y = ib.next()
    if (x.done || y.done) return x.done ? (y.done ? 0 : -1) : 1
    const d = x.value.codePointAt(0)! - y.value.codePointAt(0)!
    if (d !== 0) return Math.sign(d)
  }
}

function foldAscii(text: string): string {
  return text.replace(/[A-Z]/g, (c) => c.toLowerCase())
}

/** Builders, so that filters read as `f.and(f.eq('type', 'task'), f.contains('title', 'x'))`. */
export const f = {
  eq: (key: string, value: Value): Filter => ({ eq: { key, value } }),
  ne: (key: string, value: Value): Filter => ({ ne: { key, value } }),
  lt: (key: string, value: number | string): Filter => ({ lt: { key, value } }),
  le: (key: string, value: number | string): Filter => ({ le: { key, value } }),
  gt: (key: string, value: number | string): Filter => ({ gt: { key, value } }),
  ge: (key: string, value: number | string): Filter => ({ ge: { key, value } }),
  contains: (key: string, value: string): Filter => ({ contains: { key, value } }),
  containsIgnoreCase: (key: string, value: string): Filter => ({ contains_ignore_case: { key, value } }),
  prefix: (key: string, value: string): Filter => ({ prefix: { key, value } }),
  in: (key: string, values: Value[]): Filter => ({ in: { key, values } }),
  isNull: (key: string): Filter => ({ is_null: { key } }),
  notNull: (key: string): Filter => ({ not_null: { key } }),
  and: (...filters: Filter[]): Filter => ({ and: filters }),
  or: (...filters: Filter[]): Filter => ({ or: filters }),
  not: (filter: Filter): Filter => ({ not: filter }),
}
