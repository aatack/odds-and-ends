// The small part of JSON Schema the form and the builder agree on: what a
// schema's type is, and what a fresh value of it looks like. Pure, so both
// components and any test can use it.

export type JsonSchema = Record<string, unknown>

/** The kinds of value the form draws a control for, and the builder offers. */
export const SCHEMA_TYPES = ['string', 'number', 'integer', 'boolean', 'null', 'object', 'array'] as const
export type SchemaType = (typeof SCHEMA_TYPES)[number]

export const isObject = (v: unknown): v is Record<string, unknown> =>
  !!v && typeof v === 'object' && !Array.isArray(v)

/**
 * The one type a schema stands for, or null when it doesn't say or says
 * something the form can't draw (a union of several real types). `["string",
 * "null"]` is a nullable string, which reads as a string.
 */
export function typeOf(schema: JsonSchema | undefined): SchemaType | null {
  if (!schema) return null
  const raw = schema.type
  if (raw === undefined) {
    // No type, but the shape may give it away.
    if (isObject(schema.properties)) return 'object'
    if (isObject(schema.items)) return 'array'
    return null
  }
  const named = Array.isArray(raw) && raw.length > 1 ? raw.filter((t) => t !== 'null') : [raw].flat()
  if (named.length !== 1) return null
  return (SCHEMA_TYPES as readonly unknown[]).includes(named[0]) ? (named[0] as SchemaType) : null
}

/** The schema's properties, in the order written. */
export const propertiesOf = (schema: JsonSchema): [string, JsonSchema][] =>
  isObject(schema.properties)
    ? Object.entries(schema.properties).map(([k, v]) => [k, isObject(v) ? v : {}])
    : []

export const requiredOf = (schema: JsonSchema): string[] =>
  Array.isArray(schema.required) ? schema.required.map(String) : []

/** A fresh value for a schema: its default, else the empty value of its type. */
export function defaultFor(schema: JsonSchema): unknown {
  if ('default' in schema) return schema.default
  if ('const' in schema) return schema.const
  const variants = variantsOf(schema)
  if (variants) return variants.length ? defaultFor(variants[0]) : null
  switch (typeOf(schema)) {
    case 'string':
      return ''
    case 'number':
    case 'integer':
      return 0
    case 'boolean':
      return false
    case 'array':
      return []
    case 'null':
      return null
    case 'object': {
      const out: Record<string, unknown> = {}
      const required = requiredOf(schema)
      for (const [key, property] of propertiesOf(schema)) {
        if (required.includes(key)) out[key] = defaultFor(property)
      }
      return out
    }
    default:
      return null
  }
}

/** Pretty JSON for showing, and `undefined` as nothing at all. */
export const pretty = (value: unknown): string =>
  value === undefined ? '' : (JSON.stringify(value, null, 2) ?? '')

/**
 * The alternatives a schema allows, as schemas, or null when it isn't a union.
 * `anyOf` and `oneOf` are read alike; an `enum` is a union of literals.
 */
export function variantsOf(schema: JsonSchema): JsonSchema[] | null {
  const list = Array.isArray(schema.anyOf) ? schema.anyOf : Array.isArray(schema.oneOf) ? schema.oneOf : null
  if (list) return list.map((v) => (isObject(v) ? v : {}))
  if (Array.isArray(schema.enum)) return schema.enum.map((v) => ({ const: v }))
  return null
}

const same = (a: unknown, b: unknown): boolean => JSON.stringify(a) === JSON.stringify(b)

/**
 * Whether a value fits a schema, as far as telling a union's alternatives apart
 * needs: literals by value, types by shape, an object's required fields. Not a
 * validator: bounds and formats are not looked at.
 */
export function matches(schema: JsonSchema, value: unknown): boolean {
  if ('const' in schema) return same(schema.const, value)
  const variants = variantsOf(schema)
  if (variants) return variants.some((v) => matches(v, value))
  switch (typeOf(schema)) {
    case 'string':
      return typeof value === 'string'
    case 'number':
      return typeof value === 'number'
    case 'integer':
      return Number.isInteger(value)
    case 'boolean':
      return typeof value === 'boolean'
    case 'null':
      return value === null
    case 'array':
      return Array.isArray(value)
    case 'object':
      return isObject(value) && requiredOf(schema).every((k) => k in value)
    default:
      return true
  }
}
