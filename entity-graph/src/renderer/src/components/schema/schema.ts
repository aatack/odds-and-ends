// The small part of JSON Schema the form and the builder agree on: what a
// schema's type is, and what a fresh value of it looks like. Pure, so both
// components and any test can use it.

export type JsonSchema = Record<string, unknown>

/**
 * A string meant to hold more than a line: an extension keyword, since JSON
 * Schema has no word for how a value is entered. The form draws a growing text
 * box for it; to anything else it is a string like any other.
 */
export const MULTILINE_KEYWORD = 'x-multiline'

/** A string meant for code: the form sets it in monospace. An extension keyword. */
export const MONOSPACE_KEYWORD = 'x-monospace'

/**
 * The JSON Schema meta-schema. A schema that refers to it (`$ref`) describes a
 * value that is itself a schema — which the form edits with the schema builder.
 */
export const META_SCHEMA = 'https://json-schema.org/draft/2020-12/schema'

/** Whether a schema says its value is a schema: a `$ref` to any draft's meta-schema. */
export const isSchemaRef = (schema: JsonSchema | undefined): boolean =>
  typeof schema?.$ref === 'string' && /^https?:\/\/json-schema\.org\/.*schema#?$/.test(schema.$ref)

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
  if (isSchemaRef(schema)) return { type: 'string' }
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
  if (isSchemaRef(schema)) return isObject(value) || typeof value === 'boolean'
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

const FORMAT_TESTS: Record<string, RegExp> = {
  date: /^\d{4}-\d{2}-\d{2}$/,
  'date-time': /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2}(\.\d+)?)?(Z|[+-]\d{2}:\d{2})?$/,
  time: /^\d{2}:\d{2}(:\d{2}(\.\d+)?)?$/,
  email: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
  uri: /^[a-z][a-z0-9+.-]*:\S*$/i,
  uuid: /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i,
}

const plural = (n: number, word: string): string => `${n} ${word}${n === 1 ? '' : 's'}`

/**
 * Why a value doesn't fit its schema, at this level only, in a few words — or
 * null when it does, or when there is nothing to say yet (an absent optional
 * value). What is inside an object or a list is each its own line's business.
 *
 * Soft, like every check in the app: it says, and the value is kept regardless.
 */
export function problemOf(schema: JsonSchema, value: unknown, required = false): string | null {
  if (value === undefined) return required ? 'Required' : null
  if ('const' in schema) return same(schema.const, value) ? null : `Must be ${JSON.stringify(schema.const)}`
  if (isSchemaRef(schema)) return matches(schema, value) ? null : 'Not a schema'
  const variants = variantsOf(schema)
  if (variants) return variants.some((v) => matches(v, value)) ? null : 'Fits none of the options'

  const type = typeOf(schema)
  if (type && !matches({ type }, value)) {
    return type === 'integer' && typeof value === 'number' ? 'Not a whole number' : `Not ${article(type)}`
  }

  if (typeof value === 'number') {
    const n = value
    if (typeof schema.minimum === 'number' && n < schema.minimum) return `At least ${schema.minimum}`
    if (typeof schema.maximum === 'number' && n > schema.maximum) return `At most ${schema.maximum}`
    if (typeof schema.exclusiveMinimum === 'number' && n <= schema.exclusiveMinimum) {
      return `More than ${schema.exclusiveMinimum}`
    }
    if (typeof schema.exclusiveMaximum === 'number' && n >= schema.exclusiveMaximum) {
      return `Less than ${schema.exclusiveMaximum}`
    }
    if (typeof schema.multipleOf === 'number' && schema.multipleOf > 0 && !Number.isInteger(n / schema.multipleOf)) {
      return `A multiple of ${schema.multipleOf}`
    }
  }

  if (typeof value === 'string') {
    if (typeof schema.minLength === 'number' && value.length < schema.minLength) {
      return `At least ${plural(schema.minLength, 'character')}`
    }
    if (typeof schema.maxLength === 'number' && value.length > schema.maxLength) {
      return `At most ${plural(schema.maxLength, 'character')}`
    }
    if (typeof schema.pattern === 'string') {
      try {
        if (!new RegExp(schema.pattern, 'u').test(value)) return `Doesn't match ${schema.pattern}`
      } catch {
        // A pattern that is not one says nothing about the value.
      }
    }
    if (typeof schema.format === 'string' && value && FORMAT_TESTS[schema.format]) {
      if (!FORMAT_TESTS[schema.format].test(value)) return `Not ${article(schema.format)}`
    }
  }

  if (Array.isArray(value)) {
    if (typeof schema.minItems === 'number' && value.length < schema.minItems) {
      return `At least ${plural(schema.minItems, 'item')}`
    }
    if (typeof schema.maxItems === 'number' && value.length > schema.maxItems) {
      return `At most ${plural(schema.maxItems, 'item')}`
    }
    if (schema.uniqueItems === true) {
      const seen = new Set(value.map((v) => JSON.stringify(v)))
      if (seen.size !== value.length) return 'Items repeat'
    }
  }

  if (isObject(value) && schema.additionalProperties === false) {
    const known = new Set(propertiesOf(schema).map(([k]) => k))
    const extra = Object.keys(value).filter((k) => !known.has(k))
    if (extra.length) return `Unexpected ${extra.join(', ')}`
  }
  return null
}

const article = (type: string): string => (/^[aeiou]/.test(type) ? `an ${type}` : `a ${type}`)
