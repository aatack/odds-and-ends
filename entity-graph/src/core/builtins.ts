import type { AppEvent } from './events'
import type { Schema } from './schema'

// The entities every store has whether or not anything has been written to them.
// There are two, and both are types: `type`, the type of types, and `tool`, the
// type of the tools a store defines for itself. Each carries the schema for what
// an entity of it holds. They are served rather than stored — read the id and the
// events are handed back with the ones the store actually has — so a fresh store
// knows what a type and a tool are, and so a schema nobody has to write can't
// drift from the code that reads it.
//
// Which is also the only account of either that reaches an agent over MCP. It has
// the six tools and these instructions and no source code, so a shape it cannot
// read here is one it will guess at.
//
// The events are timestamped 0 and authored `builtin`, which is what makes them
// defaults rather than facts: a rollup sorts by timestamp, so anything written
// to one of these keys wins, and the store is still the last word on its own
// entities.

/** The type of types: an entity whose `type` is this one is a type. */
export const TYPE_ID = 'type'

/** The type of tools: what a note under `@tools` is expected to hold. */
export const TOOL_ID = 'tool'

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
        'A script run once per instance as it loads, whose return value is a list of ' +
        'events added to the client\'s cache and never written to the store — how an ' +
        'entity shows something it does not hold. Statements rather than an ' +
        'expression: the last one evaluated is what it returned, `tool.…` reaches the ' +
        'whole registry synchronously, and `context` is the instance\'s own values with ' +
        '`context.entityId` laid in on top. An event is `{ key, value }` — that value ' +
        'on the instance itself, which is the common case — or the same carrying an ' +
        '`entityId` to speak for another entity, or `{ sourceId, destinationId, action ' +
        '}` for a link, `0` adding and `1` removing. A bare object counts as a list of ' +
        'one and anything unrecognisable is dropped, so a script that only logs has ' +
        'still done its job. A derived event is timestamped 0, which is what keeps it ' +
        'behind every real edit rather than over one.',
    },
  },
}

/**
 * What a tool entity holds. The app reads `name` and a body and nothing else is
 * required; the rest is written here because this schema is the only place the
 * shape is spelled out for somebody who cannot read the code — the inspector
 * draws a box and a description per field, and `get_details` over MCP hands the
 * whole thing back.
 *
 * The order is the order to fill them in: what it is called, what it does, what
 * it takes, and then the handful of things most tools never say.
 */
export const TOOL_SCHEMA: Schema = {
  type: 'object',
  description:
    'A tool: a note under `@tools` that this app can run. It lists in the command ' +
    'palette, it can hold a key, other tools and scripts can call it by name, and what ' +
    'it did shows in the activity log. Two values make one — a `name` and an `execute` ' +
    'body — and it has to be linked under `@tools` to be found at all.\n\n' +
    'Definitions are read once, when the source opens. A tool written now reaches the ' +
    'palette when somebody runs **Reload your tools** in the app, so writing one and ' +
    'having one are different things: say which you have done. The body runs in the ' +
    'app and only in the app — the server has nothing to run one with, and it is not a ' +
    'tool of this connection, which has the six it started with.',
  required: ['name', 'execute'],
  properties: {
    name: {
      type: 'string',
      description:
        'What the tool is called: the palette\'s label, and the word a script reaches it ' +
        'by unless `id` says otherwise. The note\'s own `text` is not read for this — ' +
        'that is what the outline shows, so a definition can sit under a heading of ' +
        '"Slack things" without the palette calling it that.',
    },
    execute: {
      type: 'string',
      description:
        'The body: a string holding an expression that evaluates to a function, called ' +
        'with the declared arguments positionally, in the order `arguments` lists them ' +
        '— `(who, loudly) => { … }`. What it returns is the tool\'s result. In scope are ' +
        '`tool`, every other tool by its id or the camel case of its name, called ' +
        'synchronously; `context`, where the tool was invoked from, with the arguments ' +
        'laid in on top and `context.entityId` the selected note; and `console`. The ' +
        'sandbox has no promises, by design — that is what buys the synchronous calls — ' +
        'so the function must not be `async`, and a returned promise comes back as ' +
        'nothing.',
    },
    description: {
      type: 'string',
      description:
        'What the tool does, in a line. The palette matches its search against this, ' +
        'and the server passes over a definition that has no `description` and no ' +
        '`arguments`, so a tool without one is the app\'s alone.',
    },
    arguments: {
      type: 'array',
      description:
        'What it takes, one entry per argument, in the order to ask for them, and a ' +
        'real list rather than a string holding one. An entry is `{ "name": "who", ' +
        '"type": "string", "required": true }`, or a bare `"who"` when the name is all ' +
        'of it; `type` is `string`, `number`, `integer`, `boolean`, `entity`, or absent ' +
        'for a value entered as JSON, and `options` makes it a picker. Add a parameter ' +
        'to `execute` for each one, in the same order. A `default` does not fill the ' +
        'value in — it sends `null` for the body to interpret — so read an optional ' +
        'argument as `context.times ?? 1`.',
      items: { type: ['string', 'object'] },
    },
    id: {
      type: 'string',
      description:
        'What a script reaches it by: `tool.greet(…)`. Defaults to the name. A ' +
        'definition taking a built-in\'s id is unreachable by it, and two sharing one ' +
        'keep the first in outline order.',
    },
    key: {
      type: 'string',
      description:
        'A key binding: `g`, `shift+g`, `mod+shift+j`, where `mod` is Ctrl or ⌘. One ' +
        'the app already binds in the same scope loses — declared tools trail the ' +
        'built-ins, so a store cannot rebind a key out from under anybody.',
    },
    scope: {
      enum: ['frame', 'group', 'app'],
      description: 'Which part of the focus chain a key resolves against. Defaults to `app`.',
    },
    reach: {
      enum: ['ui', 'source', 'external'],
      description:
        'How far it reaches, and so whether its calls are kept in the activity log. ' +
        'Defaults to `external`: the log is the only account of what a script did, and ' +
        'a tool somebody wrote is the kind worth looking back at. One that only moves ' +
        'the selection says `ui` and stops filling it.',
    },
    safety: {
      enum: ['pure', 'safe-mutating', 'dangerous'],
      description:
        'Read by the server only, for its capability filters. Defaults to `dangerous`.',
    },
    mutates: {
      type: 'boolean',
      description:
        'Rarely needed: a body can only write through a write tool, and each of those ' +
        'refreshes the frames on its own way out.',
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
  [TOOL_ID]: {
    text: 'Tool',
    // An ordinary type, typed by the one above: nothing about it is special except
    // that nobody had to write it.
    type: TYPE_ID,
    schema: TOOL_SCHEMA,
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
