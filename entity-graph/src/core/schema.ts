// What a *type* says about its instances. An entity's `type` value names another
// entity, and that entity — a type — describes the entities that name it: which
// values they are expected to hold (`schema`), what can be done with them
// (`actions`), and what is computed for them when they load (`events`).
//
// Nothing here reads the store or the cache: these are pure readings of a type
// entity's own values, so the desktop app, the phone and the server all agree
// about what a type says. `./builtins` is where the types the app itself reads by
// name are written down — `type`, `tool`, `code`, `file` — which is what these
// functions are pointed at most often.
//
// Note what a type no longer does: it does not lend its values to its instances.
// A schema says what an entity *should* hold, which is a thing to show in an
// empty box and check a value against — not a value the entity silently has.
//
// (Not to be confused with `./source/schema`, which is JSON Schema for a *tool's
// arguments*. Same notation, different subject.)

/** A JSON Schema, as far as anything here is concerned: an object of keywords. */
export type Schema = Record<string, unknown>

/** One field of a type's schema: a key its instances are expected to hold. */
export interface SchemaField {
  key: string
  schema: Schema
  /** What the field is for, if the schema says. Shown under the box. */
  description?: string
  /** Named in the schema's `required`, so an instance without it is incomplete. */
  required: boolean
  /** The shape, in a word or two: `string`, `number[]`, `"a" | "b"`. */
  label: string
}

const isObject = (v: unknown): v is Record<string, unknown> =>
  !!v && typeof v === 'object' && !Array.isArray(v)

/** The schema on a type entity's values, or null if it defines none. */
export const schemaOf = (values: Record<string, unknown> | undefined): Schema | null => {
  const schema = values?.schema
  return isObject(schema) ? schema : null
}

/**
 * The fields a schema declares, in the order they were written. Order is the
 * author's: a schema is read as a form, and the order the keys were typed in is
 * the only ordering anyone intended.
 */
export function fieldsOf(schema: Schema | null): SchemaField[] {
  const properties = schema?.properties
  if (!isObject(properties)) return []
  const required = Array.isArray(schema?.required) ? schema.required.map(String) : []
  return Object.entries(properties).map(([key, raw]) => {
    const field = isObject(raw) ? raw : {}
    return {
      key,
      schema: field,
      description: typeof field.description === 'string' ? field.description : undefined,
      required: required.includes(key),
      label: typeLabel(field),
    }
  })
}

/**
 * The actions a type defines: name → the script run when the button is pressed.
 * Only string bodies count, so a half-written `actions` value can't put a button
 * on every instance that throws the moment it is pressed.
 */
export function actionsOf(values: Record<string, unknown> | undefined): Record<string, string> {
  const actions = values?.actions
  if (!isObject(actions)) return {}
  const out: Record<string, string> = {}
  for (const [name, code] of Object.entries(actions)) {
    if (typeof code === 'string' && code.trim()) out[name] = code
  }
  return out
}

/** Just the names, in the order they were written — what a row draws as buttons. */
export const actionNames = (values: Record<string, unknown> | undefined): string[] =>
  Object.keys(actionsOf(values))

/**
 * The shape a schema describes, short enough to sit beside a field's name. A
 * choice is worth more than the word "string", so an enum is spelled out; a list
 * says what it is a list of.
 */
export function typeLabel(schema: Schema | undefined): string {
  if (!schema) return 'any'
  if (Array.isArray(schema.enum)) return schema.enum.map((v) => JSON.stringify(v)).join(' | ')
  if ('const' in schema) return JSON.stringify(schema.const)
  const branches = branchesOf(schema)
  // A field with more than one shape says all of them, deduplicated: `arguments`
  // is a list, or a schema, or that list written as text, and "any" would be a
  // less honest answer than the three of them.
  if (branches) return [...new Set(branches.map((b) => typeLabel(b)))].join(' | ')
  const type = schema.type
  if (Array.isArray(type)) return type.map(String).join(' | ')
  if (type === 'array') {
    const items = isObject(schema.items) ? typeLabel(schema.items) : null
    return items && !items.includes(' ') ? `${items}[]` : 'array'
  }
  return typeof type === 'string' ? type : 'any'
}

/**
 * Whether a field is typed as itself rather than as JSON — see the inspector. A
 * choice between strings counts: `blue` is what someone means to type, and
 * making them write `"blue"` to satisfy the notation helps nobody.
 */
export const isTextual = (schema: Schema | undefined): boolean => {
  if (!schema) return false
  if (schema.type === 'string') return true
  return (
    schema.type === undefined &&
    Array.isArray(schema.enum) &&
    schema.enum.every((option) => typeof option === 'string')
  )
}

// --- Checking ---------------------------------------------------------------

const KINDS: Record<string, (v: unknown) => boolean> = {
  string: (v) => typeof v === 'string',
  number: (v) => typeof v === 'number' && Number.isFinite(v),
  integer: (v) => typeof v === 'number' && Number.isInteger(v),
  boolean: (v) => typeof v === 'boolean',
  object: isObject,
  array: Array.isArray,
  null: (v) => v === null,
}

const same = (a: unknown, b: unknown): boolean => JSON.stringify(a) === JSON.stringify(b)

/**
 * The alternatives a field allows, if it is written as a choice between shapes.
 * `oneOf` is read as `anyOf`: the difference is whether *more* than one branch may
 * match, which nothing here has any use for.
 */
function branchesOf(schema: Schema): Schema[] | null {
  const branches = schema.anyOf ?? schema.oneOf
  if (!Array.isArray(branches)) return null
  const objects = branches.filter(isObject)
  return objects.length ? objects : null
}

/**
 * Why a value doesn't fit its field, or null when it does. Deliberately *soft*:
 * the caller is expected to say so and write the value anyway, since a store
 * where a schema could refuse a value would be a store where a schema written
 * after the fact locks its own entities out.
 *
 * A subset of JSON Schema — the keywords a hand-written field description
 * actually uses. Anything unrecognised is ignored rather than treated as a
 * failure: a schema that says more than this understands is still a schema.
 *
 * `null` and `undefined` are always fine. Null is how a value comes *off* an
 * entity in an append-only store, so a required field that has been cleared is
 * an empty field rather than a wrong one.
 */
export function checkValue(value: unknown, schema: Schema | undefined): string | null {
  if (!schema || value == null) return null

  if (Array.isArray(schema.enum) && !schema.enum.some((option) => same(option, value))) {
    return `must be one of ${schema.enum.map((v) => JSON.stringify(v)).join(', ')}`
  }

  // A choice between shapes: fitting any one of them is fitting the field, and the
  // reason a value fits none of them is what the field *is*, not one branch's
  // complaint about it.
  const branches = branchesOf(schema)
  if (branches && branches.every((branch) => checkValue(value, branch) !== null)) {
    return `expected ${typeLabel(schema)}`
  }
  if ('const' in schema && !same(schema.const, value)) {
    return `must be ${JSON.stringify(schema.const)}`
  }

  const types = Array.isArray(schema.type)
    ? schema.type.map(String)
    : typeof schema.type === 'string'
      ? [schema.type]
      : []
  if (types.length && !types.some((t) => KINDS[t]?.(value) ?? true)) {
    return `expected ${types.join(' or ')}`
  }

  if (typeof value === 'string') {
    const { minLength, maxLength, pattern } = schema
    if (typeof minLength === 'number' && value.length < minLength) {
      return `at least ${minLength} characters`
    }
    if (typeof maxLength === 'number' && value.length > maxLength) {
      return `at most ${maxLength} characters`
    }
    // A pattern that doesn't compile is the schema's problem, not the value's.
    if (typeof pattern === 'string') {
      try {
        if (!new RegExp(pattern).test(value)) return `must match ${pattern}`
      } catch {
        /* ignore */
      }
    }
  }

  if (typeof value === 'number') {
    const { minimum, maximum } = schema
    if (typeof minimum === 'number' && value < minimum) return `at least ${minimum}`
    if (typeof maximum === 'number' && value > maximum) return `at most ${maximum}`
  }

  if (Array.isArray(value)) {
    const { minItems, maxItems, items } = schema
    if (typeof minItems === 'number' && value.length < minItems) return `at least ${minItems} items`
    if (typeof maxItems === 'number' && value.length > maxItems) return `at most ${maxItems} items`
    if (isObject(items)) {
      for (let i = 0; i < value.length; i++) {
        const problem = checkValue(value[i], items)
        if (problem) return `item ${i + 1} ${problem}`
      }
    }
  }

  if (isObject(value)) {
    const required = Array.isArray(schema.required) ? schema.required.map(String) : []
    for (const key of required) {
      if (value[key] == null) return `missing ${JSON.stringify(key)}`
    }
    const properties = isObject(schema.properties) ? schema.properties : {}
    for (const [key, raw] of Object.entries(properties)) {
      if (!isObject(raw) || value[key] === undefined) continue
      const problem = checkValue(value[key], raw)
      if (problem) return `${key} ${problem}`
    }
  }

  return null
}
