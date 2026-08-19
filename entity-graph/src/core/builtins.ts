import type { AppEvent } from './events'
import type { Schema } from './schema'

// The entities every store has whether or not anything has been written to them.
// There is one: `type`, the type of types, whose schema says what a type entity
// holds. It is served rather than stored — read the id and the events are handed
// back with the ones the store actually has — so a fresh store knows what a type
// is, and so a schema nobody has to write can't drift from the code that reads
// it.
//
// The events are timestamped 0 and authored `builtin`, which is what makes them
// defaults rather than facts: a rollup sorts by timestamp, so anything written
// to one of these keys wins, and the store is still the last word on its own
// entities.

/** The type of types: an entity whose `type` is this one is a type. */
export const TYPE_ID = 'type'

/** The author on a value the store supplies, as against one somebody wrote. */
const BUILTIN_AUTHOR = 'builtin'

/**
 * What a type entity holds. Three keys, each of which is read by name somewhere:
 * the schema by the details panel, the actions by every row of that type, and
 * the events by the client's cache as it loads one.
 */
export const TYPE_SCHEMA: Schema = {
  type: 'object',
  description:
    'A type: an entity that describes the entities naming it in their `type` value. ' +
    'Its own values are not inherited by them — a schema says what an instance ' +
    'should hold, not what it already holds.',
  properties: {
    schema: {
      type: 'object',
      description:
        'JSON Schema for the values an instance of this type holds. `properties` is ' +
        'the field list, in the order they should be shown; each field may carry a ' +
        '`type` and a `description`, and `required` names the ones an instance is ' +
        'incomplete without. Fields are offered and checked, never enforced.',
    },
    actions: {
      type: 'object',
      description:
        'Buttons every instance of this type wears, as name → the TypeScript run when ' +
        'the button is pressed. The script runs in the same sandbox a `type: code` ' +
        'entity does, with the instance as its context: `context.entityId` is the row ' +
        'it was pressed on, and `tool.…` reaches the whole tool registry.',
      additionalProperties: { type: 'string' },
    },
    events: {
      type: 'string',
      description:
        'A script run once per instance as it loads, with that instance as its ' +
        'context. What it returns is a list of events added to the client\'s cache and ' +
        'never written to the store — how an entity shows something it does not hold.',
    },
  },
}

/** Every entity the store supplies, and the values it supplies for each. */
export const BUILTIN_VALUES: Record<string, Record<string, unknown>> = {
  [TYPE_ID]: {
    text: 'Type',
    // Self-typed, so the panel for a type shows the fields above and a type reads
    // as one in an outline. One level deep, and the level is itself: a curiosity
    // rather than a hang.
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
