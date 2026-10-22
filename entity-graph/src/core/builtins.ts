import type { AppEvent } from './events'

// The type of types, which every store has whether or not anything has been
// written to it.
//
// A custom type is an entity saying `type: type`. Its id is its name — `recipe`
// is the entity a recipe's `type` value names — and two of its values mean
// something to the app:
//
//   schema  JSON Schema for the values an entity of the type holds
//   onLoad  a function, written as a string, run as an entity of the type is read
//           into the client's cache; it returns events for the cache alone
//
// `type` is itself a type, so it carries both: its own `schema` describes those
// two fields, which is what makes the details of a type a form for writing one.
//
// It is served rather than stored: read the id and its events are handed back
// with whatever the store actually holds for it, timestamped 0 and authored
// `builtin`, so anything written to the same keys wins.

/** The type of types: an entity whose `type` is this one is a type. */
export const TYPE_ID = 'type'

/** The author on a value the store supplies, as against one somebody wrote. */
const BUILTIN_AUTHOR = 'builtin'

/** The author on an event an `onLoad` function made, rather than anyone writing one. */
export const LAZY_AUTHOR = 'lazy'

/** What a type entity holds: the schema of the type of types. */
export const TYPE_SCHEMA: Record<string, unknown> = {
  type: 'object',
  description:
    'A type: an entity whose id is its name, saying `type: type`, which the entities ' +
    'naming it in their own `type` value are instances of.',
  properties: {
    text: { type: 'string', description: 'What the type is called.' },
    schema: {
      type: 'object',
      description:
        'JSON Schema for the values an instance holds, usually an object with ' +
        '`properties`. Read as a form: the properties in the order written, each with a ' +
        '`type` and a `description`, and `required` naming the ones an instance needs.',
    },
    onLoad: {
      type: 'string',
      description:
        'A JavaScript function, as source: `(entity) => [...]`. Run in the app\'s ' +
        'sandbox, once per instance per session, when the instance is read into the ' +
        'client\'s cache. `entity` is the instance\'s values with `entityId` laid in. ' +
        '`tool.<name>(...)` calls any tool of the app, synchronously; there is no ' +
        '`await`. It returns a list of events that go into the cache and are never ' +
        'written to the store: `{ key, value }` sets a value on the instance, ' +
        '`{ entityId, key, value }` on another entity, and `{ sourceId, destinationId }` ' +
        'links two. Anything written to the store wins over what it returns.',
    },
  },
}

/** Every entity the store supplies, and the values it supplies for each. */
export const BUILTIN_VALUES: Record<string, Record<string, unknown>> = {
  [TYPE_ID]: {
    text: 'Type',
    // Self-typed, so `type` reads as a type and carries the fields it describes.
    type: TYPE_ID,
    schema: TYPE_SCHEMA,
  },
}

/**
 * The events that stand behind a set of ids. Only for ids asked for by name: a
 * dump of the whole store is what is written down, and a value nobody wrote does
 * not belong in it.
 */
export function builtinEvents(entityIds: readonly string[] | undefined): AppEvent[] {
  if (!entityIds) return []
  const out: AppEvent[] = []
  for (const id of new Set(entityIds)) {
    const values = BUILTIN_VALUES[id]
    if (!values) continue
    for (const [key, value] of Object.entries(values)) {
      out.push({ type: 'value', timestamp: 0, author: BUILTIN_AUTHOR, entityId: id, key, value })
    }
  }
  return out
}

/**
 * What an `onLoad` function returned, as events about `entityId`. A bare object
 * counts as a list of one; an event naming no entity is about the instance; the
 * timestamp defaults to 0, so a lazy value sorts behind every real edit. Anything
 * unrecognisable is dropped rather than thrown over.
 */
export function lazyEvents(entityId: string, returned: unknown): AppEvent[] {
  const list = Array.isArray(returned) ? returned : returned == null ? [] : [returned]
  const out: AppEvent[] = []
  for (const raw of list) {
    if (!raw || typeof raw !== 'object') continue
    const e = raw as Record<string, unknown>
    const base = { timestamp: Number(e.timestamp ?? 0) || 0, author: LAZY_AUTHOR }
    if (e.sourceId != null || e.destinationId != null) {
      if (e.sourceId == null || e.destinationId == null) continue
      out.push({
        ...base,
        type: 'link',
        sourceId: String(e.sourceId),
        destinationId: String(e.destinationId),
        action: (Number(e.action ?? 0) || 0) as 0 | 1 | 2 | 3,
      })
      continue
    }
    if (typeof e.key !== 'string') continue
    out.push({
      ...base,
      type: 'value',
      entityId: e.entityId == null ? entityId : String(e.entityId),
      key: e.key,
      value: e.value ?? null,
    })
  }
  return out
}
