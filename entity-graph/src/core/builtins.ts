import type { AppEvent } from './events'
import type { Schema } from './schema'

// The entities every store has whether or not anything has been written to them.
// All three are types: `type`, the type of types; `tool`, the type of the tools a
// store defines for itself; and `diagram`, a note the app draws a canvas over.
// Each carries the schema for what an entity of it holds. They are served rather
// than stored — read the id and the events are handed back with the ones the store
// actually has — so a fresh store knows what a type, a tool and a diagram are, and
// so a schema nobody has to write can't drift from the code that reads it.
//
// Which is also the only account of any of them that reaches an agent over MCP. It
// has the six tools and these instructions and no source code, so a shape it
// cannot read here is one it will guess at.
//
// The events are timestamped 0 and authored `builtin`, which is what makes them
// defaults rather than facts: a rollup sorts by timestamp, so anything written
// to one of these keys wins, and the store is still the last word on its own
// entities.

/** The type of types: an entity whose `type` is this one is a type. */
export const TYPE_ID = 'type'

/** The type of tools: what a note under `@tools` is expected to hold. */
export const TOOL_ID = 'tool'

/** The type of diagrams: a note the app draws a canvas of shapes over. */
export const DIAGRAM_ID = 'diagram'

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
      type: 'array',
      description:
        'Buttons every instance of this type wears, as the ids of the tools they run — ' +
        '`["changeset.merge", "link.open"]` — in the order they should be drawn. Each is ' +
        'appended to the row\'s text as an inline button when it renders, and pressing one ' +
        'runs that tool along the row, so `context.entityId` is the row it was pressed on. ' +
        'Being ordinary tools, the same actions are in the palette and the right-click menu.',
      items: { type: 'string' },
    },
    events: {
      type: 'string',
      description:
        'A script run once per instance as it loads, whose return value is a list of ' +
        'events added to the client\'s cache and never written to the store — how an ' +
        'entity shows something it does not hold. Statements rather than an ' +
        'expression: the last one evaluated is what it returned, `tool.…` reaches the ' +
        'whole registry synchronously, and `context` is the instance\'s own values with ' +
        '`context.entityId` laid in on top. The events are the store\'s ordinary ones ' +
        '— `{ type: \'value\', entityId, key, value }` and `{ type: \'link\', sourceId, ' +
        'destinationId, action }`, `0` adding and `1` removing — and three of their ' +
        'fields default, so a script leaves them out: `entityId` is the instance it ran ' +
        'for, `author` is `derived`, and `timestamp` is 0, which keeps a derived value ' +
        'behind every real edit rather than over one.',
    },
  },
}

/**
 * What a tool entity holds. The app reads this `type`, the note's own text and a
 * body, and nothing else is required; the rest is written here because this schema
 * is the only place the shape is spelled out for somebody who cannot read the code
 * — the inspector draws a box and a description per field, and `get_details` over
 * MCP hands the whole thing back.
 *
 * The order is the order to fill them in: what it does, what it takes, and then
 * the handful of things most tools never say. What it is *called* is not among
 * them: that is the note's text, and so is written when the note is.
 */
export const TOOL_SCHEMA: Schema = {
  type: 'object',
  description:
    'A tool: a note under `@tools` that this app can run. It lists in the command ' +
    'palette, it can hold a key, other tools and scripts can call it by name, and what ' +
    'it did shows in the activity log. Three things make one — `type: tool`, an ' +
    '`execute` body, and the note\'s own `text`, which is what the tool is called — and ' +
    'it has to be linked under `@tools` to be found at all. There is no `name` value: ' +
    'the text is the name, so renaming the note renames the tool.\n\n' +
    'Definitions are read once, when the source opens. A tool written now reaches the ' +
    'palette when somebody runs **Reload your tools** in the app, so writing one and ' +
    'having one are different things: say which you have done. The body runs in the ' +
    'app and only in the app — the server has nothing to run one with, and it is not a ' +
    'tool of this connection, which has the six it started with.',
  required: ['execute'],
  properties: {
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
        'What a script reaches it by: `tool.greet(…)`. Defaults to the note\'s text. A ' +
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

/**
 * What a diagram entity holds. Unlike the two above this is not a schema anything
 * fills in as a form: one key is a form field and the rest of them are the picture,
 * under keys minted as shapes are drawn. `properties` cannot name those, so the
 * description is where the shape of one is spelled out — which is also the only
 * account of it an agent over MCP has, and an agent writing a diagram by hand is
 * very much the point of storing them as values.
 */
export const DIAGRAM_SCHEMA: Schema = {
  type: 'object',
  description:
    'A diagram: a note the app draws a pannable canvas over, above its own text. The ' +
    'canvas is as wide as the note and its shapes are the entity\'s own values — one ' +
    'per key, under a key beginning `diagram/`, so a diagram still has ordinary notes ' +
    'under it and moving one rectangle writes one key rather than the whole picture.\n\n' +
    'Each such value is an object saying which `shape` it is:\n\n' +
    '- `{ "shape": "rectangle", "x": 40, "y": 40, "width": 160, "height": 64, "text": "Ingest" }`\n' +
    '- `{ "shape": "text", "x": 40, "y": 140, "width": 160, "height": 24, "text": "a caption" }`\n' +
    '- `{ "shape": "arrow", "from": "diagram/1", "to": { "x": 320, "y": 72 }, "text": "then" }`\n\n' +
    'Coordinates are the canvas\'s own, positive y downwards, and neither the origin ' +
    'nor the extent means anything — the view is panned and zoomed over whatever is ' +
    'there. An arrow\'s `from` and `to` are each either the key of another shape, which ' +
    'the arrow then follows as that shape moves, or a bare `{ x, y }`. Everything but ' +
    '`shape` may be left out; a value that names no shape it recognises is not drawn.',
  properties: {
    aspectRatio: {
      type: ['number', 'string'],
      description:
        'How wide the canvas is against its height — `1.7778`, or `"16:9"` written as a ' +
        'ratio. Defaults to 16:9. The height follows from the width the note has, and ' +
        'dragging the bottom of the canvas is what writes this.',
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
  [DIAGRAM_ID]: {
    text: 'Diagram',
    type: TYPE_ID,
    schema: DIAGRAM_SCHEMA,
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
