import { KIND_KEYWORD } from '../../../core/toolArguments'
import type { ArgKind, ArgSpec } from './types'

// Tools the app didn't declare. Two things describe a tool from the outside — the
// server's integrations, and the `@tools` entities the user writes in the graph —
// and both have to arrive as a `ToolSpec` the palette can prompt for. What they
// have in common lives here: the mapping from a JSON Schema's properties onto
// argument prompts, and the one line a result is worth summarising to.
//
// Sharing it is the point. An argument's *kind* decides how it is parsed, so two
// copies of this mapping would mean a number typed against an integration and a
// number typed against a user tool disagreeing about what they are.

interface PropertySchema {
  type?: string
  enum?: unknown[]
  default?: unknown
  description?: string
  /** What the app should make of it, where a schema type can't say. */
  [KIND_KEYWORD]?: string
}

interface ObjectSchema {
  properties?: Record<string, PropertySchema>
  required?: string[]
}

const KINDS: Record<string, ArgKind> = {
  string: 'string',
  number: 'number',
  integer: 'number',
  boolean: 'boolean',
}

/**
 * How a value is entered. The app's own kinds outrun what a schema type can say,
 * so an entity id arrives as a string carrying {@link KIND_KEYWORD} beside it —
 * see `core/toolArguments`. Anything that isn't a scalar is entered as JSON: there
 * is nothing better to offer a one-line field, and it keeps the mapping total.
 */
function kindOf(schema: PropertySchema): ArgKind {
  if (schema[KIND_KEYWORD] === 'entity') return 'entity'
  if (schema.enum) return 'select'
  return KINDS[schema.type ?? ''] ?? 'json'
}

/** `pullRequest` → "Pull request". */
export function labelOf(name: string): string {
  const words = name.replace(/([a-z0-9])([A-Z])/g, '$1 $2').toLowerCase()
  return words.charAt(0).toUpperCase() + words.slice(1)
}

function argSpec(name: string, schema: PropertySchema, required: boolean): ArgSpec {
  return {
    name,
    label: labelOf(name),
    kind: kindOf(schema),
    ...(schema.enum ? { options: schema.enum.map(String) } : {}),
    // A schema default is the tool's own: leaving the field alone sends `null`,
    // which is how the source contract spells "use the default".
    ...(schema.default !== undefined ? { hasDefault: true } : {}),
    ...(required ? {} : { optional: true }),
    ...(schema.description ? { placeholder: schema.description } : {}),
  }
}

/**
 * A JSON Schema object's properties as argument prompts, in the order the schema
 * lists them. Anything that isn't an object schema has no arguments — which is a
 * tool that takes none rather than an error, since the alternative is a tool the
 * palette refuses to show at all over a missing `properties`.
 */
export function argsFromSchema(schema: unknown): ArgSpec[] {
  if (typeof schema !== 'object' || schema === null) return []
  const { properties, required } = schema as ObjectSchema
  const isRequired = new Set(required ?? [])
  return Object.entries(properties ?? {}).map(([name, property]) =>
    argSpec(name, property, isRequired.has(name)),
  )
}

// --- Saying what came back --------------------------------------------------

const clip = (text: string, limit = 160): string =>
  text.length > limit ? `${text.slice(0, limit - 1)}…` : text

/**
 * The most telling thing a result says about itself, in the order it's worth
 * saying: what the service said back, then what the thing *is*, then where it
 * is, and failing all three how many of them came back. The toast is a
 * confirmation — the whole result is in the activity log.
 *
 * `result` trails the rest because it is the vaguest of these names, and the one
 * a payload is least likely to have meant as its headline — but it is what a
 * Claude session's reply comes back under, and that is worth a toast.
 */
const TELLING = ['output', 'text', 'title', 'permalink', 'url', 'result']

export function summarise(data: unknown): string | null {
  if (typeof data === 'string') return data.trim() ? clip(data.trim()) : null
  if (Array.isArray(data)) return `${data.length} result${data.length === 1 ? '' : 's'}`
  if (data && typeof data === 'object') {
    const record = data as Record<string, unknown>
    for (const key of TELLING) {
      const value = record[key]
      if (typeof value === 'string' && value.trim()) return clip(value.trim())
    }
    const rows = Object.values(record).find(Array.isArray)
    if (rows) return `${rows.length} result${rows.length === 1 ? '' : 's'}`
  }
  return null
}
