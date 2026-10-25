// Find results, kept up to date with local writes. The server gives a list of
// IDs; the browser then corrects it for what it has written since: a row whose
// values no longer match goes, and a written entity that now matches comes in
// at its place in the order. So a find, too, answers a write at once.

import { isTyped, type EntityView } from './cache.js'
import { compareText, matches } from './filter.js'
import type { EntityId, Filter, FindOrder, Value } from './types.js'

export interface FindRowView {
  id: EntityId
  /** `undefined` while the values are loading. */
  values: Record<string, Value> | undefined
  status: 'ready' | 'loading'
}

export interface LocalizeInput {
  ids: readonly EntityId[]
  done: boolean
  filter: Filter
  order?: FindOrder
  includeArchived?: boolean
  /** Entities written locally since the query was asked. */
  written: ReadonlySet<EntityId>
}

export function localize(view: EntityView, input: LocalizeInput): EntityId[] {
  const { ids, done, filter, order = 'entity_id', includeArchived = false, written } = input
  const ok = (id: EntityId) => {
    const values = view.get(id)?.values
    if (!values) return undefined
    return (includeArchived || isTyped(values)) && matches(filter, id, values)
  }
  const out = ids.filter((id) => !written.has(id) || ok(id) !== false)
  if (written.size === 0) return out
  const listed = new Set(ids)
  const compare = comparer(view, order)
  const last = ids[ids.length - 1]
  for (const id of written) {
    if (listed.has(id) || ok(id) !== true) continue
    // Past the last row fetched, it belongs to a page not read yet; the
    // server gives it then.
    if (!done && (order === 'insertion' || last === undefined || compare(id, last) > 0)) continue
    if (order === 'insertion') {
      out.push(id)
      continue
    }
    let at = out.findIndex((other) => compare(id, other) < 0)
    if (at < 0) at = out.length
    out.splice(at, 0, id)
  }
  return out
}

/** The server's order on IDs, as far as the browser can know it. */
function comparer(view: EntityView, order: FindOrder): (a: EntityId, b: EntityId) => number {
  if (order === 'entity_id' || order === 'insertion') return compareText
  const [key, sign] = 'value_asc' in order ? [order.value_asc, 1] : [order.value_desc, -1]
  return (a, b) => {
    const x = view.get(a)?.values?.[key]
    const y = view.get(b)?.values?.[key]
    return sign * (compareValues(x, y) || compareText(a, b))
  }
}

/** Numbers (booleans as 0 and 1) before text (JSON as its text). */
function compareValues(a: Value | undefined, b: Value | undefined): number {
  const rank = (v: Value | undefined) =>
    typeof v === 'number' || typeof v === 'boolean' ? 0 : v === undefined || v === null ? 2 : 1
  const ra = rank(a)
  const rb = rank(b)
  if (ra !== rb) return ra - rb
  if (ra === 0) return Number(a) - Number(b)
  if (ra === 1) {
    const text = (v: Value | undefined) => (typeof v === 'string' ? v : JSON.stringify(v))
    return compareText(text(a), text(b))
  }
  return 0
}

export function rowsOf(view: EntityView, ids: readonly EntityId[]): FindRowView[] {
  return ids.map((id) => {
    const values = view.get(id)?.values
    return { id, values, status: values ? 'ready' : 'loading' }
  })
}
