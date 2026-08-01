// What a tool's arguments look like written by hand, and the JSON Schema they
// become.
//
// A tool declared in the graph says what it takes as a list, one entry per
// argument, in the order it wants to be asked for them:
//
//   [{ "name": "who", "type": "string", "required": true },
//    { "name": "loudly", "type": "boolean" }]
//
// Which is the format because it is the one a person will actually write into a
// note. JSON Schema is the format everything *downstream* wants — the palette
// derives its prompts from it, and the server publishes it verbatim to MCP — so
// the list is converted here rather than being a second thing to understand.
// A definition that already holds a schema is left exactly as it is.
//
// Deliberately free of dependencies: the client turns a definition into prompts
// and the server turns the same definition into a tool description, and the two
// must not be able to disagree about what it said.

/** An empty schema — a tool that takes no arguments, said in the form MCP needs. */
const NO_ARGUMENTS = { type: 'object', properties: {} } as const

/**
 * Declared type → the JSON Schema type it means. Left off entirely for anything
 * absent or unrecognised, which is how "empty for JSON" is spelled: a property
 * with no type is any value, and the app offers a JSON field for it.
 */
const SCHEMA_TYPES: Record<string, string> = {
  string: 'string',
  number: 'number',
  integer: 'integer',
  int: 'integer',
  boolean: 'boolean',
  bool: 'boolean',
  // Passed through rather than dropped: both are entered as JSON either way, but
  // saying so keeps the schema honest for anything reading it over MCP.
  object: 'object',
  array: 'array',
}

/**
 * An entity id. JSON Schema has no way to say this — it is a string as far as
 * anything outside the app is concerned — so it travels as one, with the app's own
 * reading of it alongside under an extension keyword. `x-` keywords are ignored by
 * every schema validator, which is the point: MCP sees a string, and the palette
 * still knows to offer the id picker.
 */
export const KIND_KEYWORD = 'x-kind'

const text = (v: unknown): string => (typeof v === 'string' ? v.trim() : '')

/** One entry of the list as a schema property, or null when it names nothing. */
function propertyFrom(entry: unknown): { name: string; property: Record<string, unknown>; required: boolean } | null {
  // A bare name is the whole of most arguments: `["who", "text"]`.
  if (typeof entry === 'string') {
    const name = entry.trim()
    return name ? { name, property: {}, required: false } : null
  }
  if (typeof entry !== 'object' || entry === null || Array.isArray(entry)) return null
  const declared = entry as Record<string, unknown>
  const name = text(declared.name)
  // Nothing to prompt for and nothing to pass it to `run` under, so this is not
  // an argument however much else it says about itself.
  if (!name) return null

  const property: Record<string, unknown> = {}
  const type = text(declared.type).toLowerCase()
  if (type === 'entity') {
    property.type = 'string'
    property[KIND_KEYWORD] = 'entity'
  } else if (SCHEMA_TYPES[type]) {
    property.type = SCHEMA_TYPES[type]
  }
  if (Array.isArray(declared.options) && declared.options.length > 0) {
    property.enum = declared.options
  }
  const description = text(declared.description)
  if (description) property.description = description
  if (declared.default !== undefined) property.default = declared.default

  // Absent means false, so an argument is optional until it says it isn't —
  // which is the safe direction to be wrong in: a call is refused for want of a
  // required argument, and nothing is refused for want of an optional one.
  return { name, property, required: declared.required === true }
}

function schemaFromList(declared: readonly unknown[]): Record<string, unknown> {
  const properties: Record<string, unknown> = {}
  const required: string[] = []
  for (const entry of declared) {
    const argument = propertyFrom(entry)
    if (!argument) continue
    // First of a name wins, as it does for two tools sharing one: which of them
    // the prompt meant would otherwise depend on the order of the list.
    if (argument.name in properties) continue
    properties[argument.name] = argument.property
    if (argument.required) required.push(argument.name)
  }
  return { type: 'object', properties, ...(required.length ? { required } : {}) }
}

export interface ReadArguments {
  schema: Record<string, unknown>
  /**
   * Something was written under `arguments` that arguments can't be read out of.
   * Worth saying rather than swallowing: the symptom of a declaration that didn't
   * take is a tool that runs at once, asks nothing, and calls its body with no
   * arguments — which looks like the body being wrong, not the declaration.
   */
  unreadable: boolean
}

/**
 * What a definition's `arguments` value says, as a JSON Schema object.
 *
 * A list is the form to write and an object is taken to be a schema already. A
 * *string* is the interesting case: a value field holding a list is easily
 * written as text by accident — through an editor that keeps a string a string,
 * or a write that quoted it — so the text is parsed rather than shrugged at. It
 * is unambiguous, since there is nothing else a string under `arguments` could
 * reasonably be trying to say.
 *
 * Anything left is a tool with no arguments rather than an error: refusing to show
 * a tool at all over a malformed field helps nobody.
 */
export function readToolArguments(declared: unknown): ReadArguments {
  let value = declared
  if (typeof value === 'string') {
    const trimmed = value.trim()
    // Nothing written, as distinct from something written that won't read.
    if (!trimmed) return { schema: { ...NO_ARGUMENTS }, unreadable: false }
    try {
      // Parsed once and not recursively: a string that parses to another string
      // is not a declaration wrapped twice, it is a declaration that is wrong.
      value = JSON.parse(trimmed)
    } catch {
      return { schema: { ...NO_ARGUMENTS }, unreadable: true }
    }
  }
  if (Array.isArray(value)) return { schema: schemaFromList(value), unreadable: false }
  if (value && typeof value === 'object') {
    return { schema: value as Record<string, unknown>, unreadable: false }
  }
  // `null` is a value cleared and `undefined` is one never written; a number or a
  // boolean is neither, and is worth complaining about.
  return { schema: { ...NO_ARGUMENTS }, unreadable: value != null }
}

/** Just the schema — what the server publishes, which has no one to warn. */
export const toolArgumentsSchema = (declared: unknown): Record<string, unknown> =>
  readToolArguments(declared).schema
