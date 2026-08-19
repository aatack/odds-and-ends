import type { AppEvent } from './events'
import type { Schema } from './schema'

// The entities every store has whether or not anything has been written to them:
// the types the *app* reads by name, and the heading they hang under. They are
// served rather than stored — read one of these ids and the events come back with
// the ones the store actually holds — so a fresh store already knows what a type,
// a tool, a code block and a file are, and a schema nobody has to write can't
// drift from the code that reads it.
//
// The rule this follows: anything the app gives special meaning to is described
// here. A field read by name somewhere in `src/` and nowhere in a schema is a
// field only the source can tell you about, which is no use to an agent reading
// the store over MCP — and not much use to the details panel either.
//
// The events are timestamped 0 and authored `builtin`, which is what makes them
// defaults rather than facts: a rollup sorts by timestamp, so anything written to
// one of these keys wins, and the store is still the last word on its own
// entities.

/** The type of types: an entity whose `type` is this one is a type. */
export const TYPE_ID = 'type'

/** Where the types are collected — the built-in ones, and any you write. */
export const TYPES_ID = '@types'

/** The author on a value the store supplies, as against one somebody wrote. */
const BUILTIN_AUTHOR = 'builtin'

/**
 * What a type entity holds. Three keys, each of which is read by name somewhere:
 * the schema by the details panel, the actions by every row of that type, and the
 * events by the client's cache as it loads one.
 */
const TYPE_SCHEMA: Schema = {
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

/**
 * One entry of a tool's `arguments`, which is a JSON object in a list rather than
 * an entity of its own — so it is described here, where the list is, rather than
 * as a type nothing would ever name. `core/toolArguments.ts` is what reads it.
 */
const TOOL_ARGUMENT_SCHEMA: Schema = {
  type: 'object',
  description: 'One argument. A bare string is an argument that needs nothing but a name.',
  properties: {
    name: {
      type: 'string',
      description:
        'What the body receives it under, and what `context` keys it by. The label is ' +
        'derived from it: `pullRequest` is prompted for as "Pull request". An entry ' +
        'naming nothing is skipped, and the first of a repeated name wins.',
    },
    type: {
      enum: ['string', 'number', 'integer', 'boolean', 'object', 'array', 'entity'],
      description:
        'How the field is entered. Absent or unrecognised means the value is typed as ' +
        'JSON. `entity` gives the id picker, which can be filled by pointing rather ' +
        'than typing, and travels outside the app as an ordinary string.',
    },
    required: {
      type: 'boolean',
      default: false,
      description: 'A call will not run with a required argument outstanding.',
    },
    options: {
      type: 'array',
      description: 'Makes the field a picker, whatever `type` says.',
    },
    description: { type: 'string', description: "Shown as the field's placeholder." },
    default: {
      description:
        'Does not fill the field in: it marks the field "default", and leaving it alone ' +
        'sends `null` — the contract\'s "use the default", which the tool itself is then ' +
        'meant to apply.',
    },
  },
  required: ['name'],
}

/**
 * A tool of the app, written in the store: a note under `@tools`, read by
 * `tools/userTools.ts` here and `core/source/userTools.ts` on the server.
 * `docs/user-tools.md` is the long form.
 *
 * `required` is the *app's* answer — a name and a body, since a tool that cannot
 * run is not a tool. The server asks for something else, which the descriptions
 * say where it matters.
 */
const TOOL_SCHEMA: Schema = {
  type: 'object',
  description:
    'A tool of the app, written in the store: a note under `@tools` describing what it ' +
    'is called, what it takes, and what it does. Every child of `@tools` is read as one ' +
    'whether or not it names this type; naming it is what puts these fields on the ' +
    "note. Its own `text` is deliberately not read — that is what the outline shows, " +
    'which is a different job. The server publishes a definition over MCP only once it ' +
    'has a `description` and an `arguments` as well.',
  properties: {
    name: {
      type: 'string',
      description: "What the tool is called: the palette's label, and its id when `id` is absent.",
    },
    execute: {
      type: 'string',
      description:
        'The body: an expression evaluating to a function, applied to the declared ' +
        'arguments positionally. It runs in a QuickJS sandbox whose globals are ' +
        '`console`, `context` (the folded call context, arguments laid on top) and ' +
        '`tool` (the whole registry, called synchronously — no `await`, and no `async`).',
    },
    id: {
      type: 'string',
      description:
        'What a script reaches it by: `tool.greet(…)`. Defaults to `name`. One that ' +
        "takes a built-in's id is unreachable by it, and two definitions sharing one " +
        'keep the first in outline order.',
    },
    description: {
      type: 'string',
      description:
        "Matched by the palette's search, and required by the server: a tool with " +
        'nothing but a name and a body works in the app and never appears over MCP.',
    },
    arguments: {
      anyOf: [
        { type: 'array', items: { anyOf: [{ type: 'string' }, TOOL_ARGUMENT_SCHEMA] } },
        { type: 'object' },
        { type: 'string' },
      ],
      description:
        'What it takes: a list, one entry per argument, in the order they are asked ' +
        'for. An object is taken to be JSON Schema already and published untouched; a ' +
        'string is parsed, since a value holding a list is easy to leave as text by ' +
        'accident. Required by the server, optional here.',
    },
    scope: {
      enum: ['frame', 'group', 'app'],
      default: 'app',
      description: 'Which part of the focus chain its key resolves against.',
    },
    reach: {
      enum: ['ui', 'source', 'external'],
      default: 'external',
      description: 'How far it reaches, and so whether its calls are kept in the activity log.',
    },
    key: {
      type: 'string',
      description:
        'A binding: `g`, `shift+g`, `mod+shift+j`, where `mod` is Ctrl or ⌘. No binding ' +
        'by default, and one that collides with a built-in loses.',
    },
    mutates: {
      type: 'boolean',
      default: false,
      description:
        'Whether the frames are refreshed after it. Rarely needed: a body can only write ' +
        '*through* a write tool, and each of those refreshes on its own way out.',
    },
    safety: {
      enum: ['pure', 'safe-mutating', 'dangerous'],
      default: 'dangerous',
      description: 'Read by the server only, for its capability filters.',
    },
    script: {
      type: 'string',
      description:
        'The older shape of a body: statements rather than an expression, reading their ' +
        'arguments off `context`. Kept because it was here first; a note carrying both ' +
        'uses `execute`.',
    },
  },
  required: ['name', 'execute'],
}

/** A runnable block. The app reads one value on it, and runs it. */
const CODE_SCHEMA: Schema = {
  type: 'object',
  description:
    'A script the app can run: the row draws it as a code block with a play button, and ' +
    'shows what it logged and what it came back with underneath. It runs in the same ' +
    'sandbox a tool body does — `console`, `context` and `tool`, synchronously — with ' +
    'the context folded along the row it sits on.',
  properties: {
    text: {
      type: 'string',
      description:
        'The script itself: statements, TypeScript, whose last expression is the result. ' +
        'A code entity is the one place `text` is not prose.',
    },
  },
}

/** An attachment. The bytes are not a value; see the description. */
const FILE_SCHEMA: Schema = {
  type: 'object',
  description:
    'An attachment: bytes stored under this entity\'s id, drawn in place of the row\'s ' +
    'text. The bytes are not a value and no read of the entity returns them — they live ' +
    'in the resource store beside the events, written with `writeResource` and read with ' +
    '`readResource`, and undo does not reach them.',
  properties: {
    mimeType: {
      type: 'string',
      description:
        'What the stored bytes are, e.g. `image/png`. Held on the entity as well as with ' +
        'the bytes, so the row knows what it is about to show before they arrive.',
    },
    text: {
      type: 'string',
      description: 'A caption, drawn under the bytes. Also the alt text.',
    },
  },
}

/** One type the app itself reads by name, and what it says about its instances. */
interface BuiltinType {
  id: string
  /** What it is called: the note's text, and so what a row of it reads as. */
  text: string
  schema: Schema
}

/**
 * The types the app gives meaning to, in the order they are worth reading. A type
 * added here needs nothing else doing: it is served, it appears under
 * {@link TYPES_ID}, and the details panel draws its fields.
 */
export const BUILTIN_TYPES: BuiltinType[] = [
  { id: TYPE_ID, text: 'Type', schema: TYPE_SCHEMA },
  { id: 'tool', text: 'Tool', schema: TOOL_SCHEMA },
  { id: 'code', text: 'Code', schema: CODE_SCHEMA },
  { id: 'file', text: 'File', schema: FILE_SCHEMA },
]

/** Every entity the store supplies, and the values it supplies for each. */
export const BUILTIN_VALUES: Record<string, Record<string, unknown>> = {
  // Served as well as the types themselves, so the place they are collected exists
  // on a store where nobody has made one.
  [TYPES_ID]: { text: 'Types', section: true },
  ...Object.fromEntries(
    BUILTIN_TYPES.map(({ id, text, schema }) => [
      id,
      // Every type is an instance of `type`, `type` included — self-typed, so the
      // panel for a type shows the fields a type holds and a type reads as one in
      // an outline. One level deep, and the level is itself: a curiosity rather
      // than a hang.
      { text, type: TYPE_ID, schema },
    ]),
  ),
}

/**
 * The links served with them: the built-in types hang under {@link TYPES_ID}, so
 * they can be *found* rather than only looked up by an id you had to know. They
 * are ordinary links — a real removal event takes one out, since it is later.
 */
const BUILTIN_LINKS: { sourceId: string; destinationId: string }[] = BUILTIN_TYPES.map(
  ({ id }) => ({ sourceId: TYPES_ID, destinationId: id }),
)

/**
 * The events that stand behind a set of ids. Only for ids asked for by name: a
 * dump of the whole store is what is written down, and a value nobody wrote does
 * not belong in it.
 *
 * A link comes back when *either* end was asked for, since it is part of both: the
 * parent's children and the child's inbound links are the same event read from two
 * sides.
 */
export function builtinEvents(entityIds: readonly string[] | undefined): AppEvent[] {
  if (!entityIds) return []
  const asked = new Set(entityIds)
  const out: AppEvent[] = []
  for (const id of asked) {
    const values = BUILTIN_VALUES[id]
    if (!values) continue
    for (const [key, value] of Object.entries(values)) {
      out.push({ type: 'value', timestamp: 0, author: BUILTIN_AUTHOR, entityId: id, key, value })
    }
  }
  for (const { sourceId, destinationId } of BUILTIN_LINKS) {
    if (!asked.has(sourceId) && !asked.has(destinationId)) continue
    out.push({
      type: 'link',
      timestamp: 0,
      author: BUILTIN_AUTHOR,
      sourceId,
      destinationId,
      action: 0,
    })
  }
  return out
}
