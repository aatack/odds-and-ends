// What v2 did, ported and left alone.
//
// The migration is a translation between two event vocabularies, and the only
// way to know a translation is faithful is to speak both languages. This is v2's
// reducer, its read order and its rollup, taken from `common/src` of the old
// repo with their quirks intact — the `-` that empties a list to `null`, the
// swap that mutates in place, the operation character that matches nothing and
// falls through unchanged. Nothing here should be improved: the point of it is
// to say what the old store *would have shown*, so the check at the end of a run
// compares against v2 rather than against this script's opinion of v2.

export type Json = Json[] | { [key: string]: Json } | string | number | boolean | null

export type Reducer = (current: Json, update: Json) => Json

export const replace: Reducer = (_, update) => update

/**
 * The reducer behind `inbound` and `outbound`: a list edited by writing one
 * string at it, the first character saying what to do and the rest naming the
 * entity. `+` adds, `-` removes, `<` moves an item one place towards the front,
 * `>` one place towards the back, and anything else is a no-op.
 */
export const array: Reducer = (current, update) => {
  if (Array.isArray(update)) {
    if (update.length === 0) {
      return current
    } else {
      return array(array(current, update[0]!), update.slice(1))
    }
  }

  if (typeof update !== 'string' || update.length < 1) {
    return current
  }

  const operation = update[0]
  const text = update.slice(1)

  if (!Array.isArray(current)) {
    current = []
  }

  const present = current.includes(text)

  if (operation === '+' && !present) {
    return [...current, text]
  } else if (operation === '-' && present) {
    const items = current.filter((item) => typeof item === 'string' && item !== text)
    return items.length === 0 ? null : items
  } else if (operation === '>' && present) {
    const index = current.indexOf(text)
    if (index >= 0 && index < current.length - 1) {
      const left: Json = current[index]!
      const right: Json = current[index + 1]!

      current[index + 1] = left
      current[index] = right

      return current
    }
  } else if (operation === '<' && present) {
    const index = current.indexOf(text)
    if (index > 0) {
      const left: Json = current[index - 1]!
      const right: Json = current[index]!

      current[index] = left
      current[index - 1] = right

      return current
    }
  }

  return current
}

/** Which keys v2 folded with {@link array}; everything else replaced. */
export const REDUCERS: Record<string, Reducer> = { inbound: array, outbound: array }

/** One row of the v2 `entities` table, its JSON already parsed. */
export interface V2Row {
  timestamp: number
  uuid: string
  key: string
  value: Json
}

const compareKeys = (left: unknown[], right: unknown[]): -1 | 0 | 1 =>
  left.length === 0 || right.length === 0
    ? 0
    : left[0] === right[0]
      ? compareKeys(left.slice(1), right.slice(1))
      : (left[0] as never) < (right[0] as never)
        ? -1
        : 1

/**
 * The order v2 read rows in: `[timestamp, uuid, key, value]`, which is what its
 * client sorted by before folding them. A write put at most one row per (uuid,
 * key), so in practice the first two settle it and the rest only ever breaks a
 * tie between two writes landing in the same millisecond — but the tie-break is
 * v2's, so it is kept, value comparison and all.
 *
 * The sort is stable, so rows that compare equal stay in the order they were
 * read, which for a SQLite table is the order they were written.
 */
export const v2Order = (rows: V2Row[]): V2Row[] =>
  [...rows].sort((left, right) =>
    compareKeys(
      [left.timestamp, left.uuid, left.key, left.value],
      [right.timestamp, right.uuid, right.key, right.value]
    )
  )

/**
 * Fold rows into entities the way v2's client did: each key reduced against what
 * that key already held, in {@link v2Order}. Takes rows already in that order.
 */
export const rollupV2 = (ordered: V2Row[]): Map<string, Record<string, Json>> => {
  const entities = new Map<string, Record<string, Json>>()

  for (const { uuid, key, value } of ordered) {
    const entity = entities.get(uuid) ?? {}
    const reducer = REDUCERS[key] ?? replace
    entity[key] = reducer(entity[key] ?? null, value)
    entities.set(uuid, entity)
  }

  return entities
}

/**
 * The entity v2 called the root: the earliest one anything was ever written to,
 * counting only `text`, which is what the old backend seeded a fresh store with.
 */
export const rootOf = (ordered: V2Row[]): string | null =>
  ordered.find((row) => row.key === 'text')?.uuid ?? null
