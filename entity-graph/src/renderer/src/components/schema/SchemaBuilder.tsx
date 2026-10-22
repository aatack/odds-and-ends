import React, { useEffect, useState } from 'react'
import { Plus, Trash03 } from '@untitledui/icons'
import { Button } from '../ui/Button'
import { IconButton } from '../ui/IconButton'
import { Input } from '../ui/Input'
import { Select } from '../ui/Select'
import { Switch } from '../ui/Switch'
import {
  SCHEMA_TYPES,
  isObject,
  propertiesOf,
  requiredOf,
  typeOf,
  variantsOf,
  type JsonSchema,
  type SchemaType,
} from './schema'

// A form for writing a JSON Schema. Each schema is one *kind*:
//
//   a type       string, number, integer, boolean, null
//   object       named fields, each its own schema, each optionally required
//   array        one schema for its items
//   literal      exactly one value (`const`)
//   union        any of several schemas (`anyOf`) — a union of literals is what a
//                set of choices is
//   any          no constraint at all
//
// Objects, arrays and unions hold schemas of their own, drawn by this same
// component one level in. Keys it has no control for — `default`, `format`,
// bounds — are kept as they were, so a schema written by hand survives being
// edited here.

export function SchemaBuilder({
  value,
  setValue,
}: {
  value: unknown
  setValue: (value: unknown) => void
}): React.JSX.Element {
  return <SchemaNode schema={isObject(value) ? value : {}} onChange={setValue} />
}

const KINDS = [...SCHEMA_TYPES, 'literal', 'union', 'any'] as const
type Kind = (typeof KINDS)[number]

/** Which kind a schema is, as the builder reads it. */
function kindOf(schema: JsonSchema): Kind {
  if ('const' in schema) return 'literal'
  if (variantsOf(schema)) return 'union'
  return typeOf(schema) ?? 'any'
}

/** The keys that belong to one kind and mean nothing to another. */
const KIND_KEYS = ['type', 'properties', 'required', 'items', 'enum', 'const', 'anyOf', 'oneOf'] as const

function withKind(schema: JsonSchema, next: Kind): JsonSchema {
  const out: JsonSchema = { ...schema }
  for (const key of KIND_KEYS) delete out[key]
  switch (next) {
    case 'any':
      return out
    case 'literal':
      return { ...out, const: '' }
    case 'union':
      return { ...out, anyOf: [{ type: 'string' }, { type: 'null' }] }
    case 'object':
      return { ...out, type: 'object', properties: {} }
    case 'array':
      return { ...out, type: 'array', items: { type: 'string' } }
    default:
      return { ...out, type: next as SchemaType }
  }
}

/** A small uppercase heading over a nested block. */
function Heading({ children }: { children: React.ReactNode }): React.JSX.Element {
  return (
    <p className="pb-1.5 text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
      {children}
    </p>
  )
}

function SchemaNode({
  schema,
  onChange,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const kind = kindOf(schema)
  const description = typeof schema.description === 'string' ? schema.description : ''
  return (
    <div className="space-y-2">
      <div className="flex items-center gap-2">
        {/* A fixed width belongs on a wrapper: the select is full width by
            design, and two widths on one element is a coin toss. */}
        <div className="w-28 shrink-0">
          <Select value={kind} onChange={(e) => onChange(withKind(schema, e.target.value as Kind))}>
            {KINDS.map((k) => (
              <option key={k} value={k}>
                {k}
              </option>
            ))}
          </Select>
        </div>
        {kind === 'literal' && <LiteralValue schema={schema} onChange={onChange} />}
      </div>
      <Input
        placeholder="Description (optional)"
        value={description}
        onChange={(e) => {
          const out = { ...schema }
          if (e.target.value) out.description = e.target.value
          else delete out.description
          onChange(out)
        }}
      />
      {kind === 'object' && <Properties schema={schema} onChange={onChange} />}
      {kind === 'array' && (
        <Nested>
          <Heading>Each item</Heading>
          <SchemaNode
            schema={isObject(schema.items) ? schema.items : {}}
            onChange={(items) => onChange({ ...schema, items })}
          />
        </Nested>
      )}
      {kind === 'union' && <Variants schema={schema} onChange={onChange} />}
    </div>
  )
}

/** The panel a nested schema sits in, one step in from its parent. */
function Nested({ children }: { children: React.ReactNode }): React.JSX.Element {
  return <div className="ml-3 space-y-2 rounded-md bg-gray-50 p-3">{children}</div>
}

/**
 * A literal's value. What is typed is read as JSON where it parses — `3`,
 * `true`, `null`, `"quoted"` — and as a plain string where it doesn't, so a word
 * needs no quotes.
 */
function LiteralValue({
  schema,
  onChange,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const shown = typeof schema.const === 'string' ? schema.const : JSON.stringify(schema.const)
  const [text, setText] = useState(shown)
  useEffect(() => {
    if (readLiteral(text) !== schema.const) setText(shown)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shown])
  return (
    <div className="min-w-0 flex-1">
      <Input
        mono
        placeholder="Value"
        value={text}
        onChange={(e) => {
          setText(e.target.value)
          onChange({ ...schema, const: readLiteral(e.target.value) })
        }}
      />
    </div>
  )
}

function readLiteral(text: string): unknown {
  try {
    return JSON.parse(text)
  } catch {
    return text
  }
}

function Variants({
  schema,
  onChange,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const variants = variantsOf(schema) ?? []
  // Written back as `anyOf`, whichever of the three it was read from.
  const write = (next: JsonSchema[]): void => {
    const out: JsonSchema = { ...schema, anyOf: next }
    delete out.oneOf
    delete out.enum
    onChange(out)
  }
  return (
    <Nested>
      <Heading>Any one of</Heading>
      {variants.map((variant, i) => (
        <div key={i} className="flex items-start gap-1.5 rounded-md bg-white p-2 shadow-xs">
          <div className="min-w-0 flex-1">
            <SchemaNode schema={variant} onChange={(next) => write(variants.map((v, j) => (j === i ? next : v)))} />
          </div>
          <IconButton title="Remove option" onClick={() => write(variants.filter((_, j) => j !== i))}>
            <Trash03 size={14} />
          </IconButton>
        </div>
      ))}
      <Button variant="secondary" size="sm" onClick={() => write([...variants, { const: '' }])}>
        <Plus size={13} /> Add option
      </Button>
    </Nested>
  )
}

function Properties({
  schema,
  onChange,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const properties = propertiesOf(schema)
  const required = requiredOf(schema)

  /** Rebuild the properties in order, so a rename keeps a field where it was. */
  const write = (next: [string, JsonSchema][], nextRequired: string[]): void => {
    const out: JsonSchema = { ...schema, properties: Object.fromEntries(next) }
    const kept = nextRequired.filter((k) => next.some(([key]) => key === k))
    if (kept.length) out.required = kept
    else delete out.required
    onChange(out)
  }

  const fresh = (): string => {
    let n = properties.length + 1
    while (properties.some(([key]) => key === `field${n}`)) n++
    return `field${n}`
  }

  return (
    <Nested>
      <Heading>Fields</Heading>
      {properties.map(([key, property], i) => (
        <div key={i} className="space-y-2 rounded-md bg-white p-2 shadow-xs">
          <div className="flex items-center gap-2">
            <PropertyName
              name={key}
              taken={properties.filter((_, j) => j !== i).map(([k]) => k)}
              onRename={(name) =>
                write(
                  properties.map(([k, p], j) => (j === i ? [name, p] : [k, p])),
                  required.map((r) => (r === key ? name : r)),
                )
              }
            />
            <span className="text-[11px] text-gray-500">required</span>
            <Switch
              label={`${key} is required`}
              checked={required.includes(key)}
              onChange={(on) =>
                write(properties, on ? [...required, key] : required.filter((r) => r !== key))
              }
            />
            <IconButton title={`Remove ${key}`} onClick={() => write(properties.filter((_, j) => j !== i), required)}>
              <Trash03 size={14} />
            </IconButton>
          </div>
          <SchemaNode
            schema={property}
            onChange={(next) => write(properties.map(([k, p], j) => (j === i ? [k, next] : [k, p])), required)}
          />
        </div>
      ))}
      <Button
        variant="secondary"
        size="sm"
        onClick={() => write([...properties, [fresh(), { type: 'string' }]], required)}
      >
        <Plus size={13} /> Add field
      </Button>
    </Nested>
  )
}

/**
 * A field's name, written through only while it is a name: not empty and not
 * another field's, either of which would lose a field from the object.
 */
function PropertyName({
  name,
  taken,
  onRename,
}: {
  name: string
  taken: string[]
  onRename: (name: string) => void
}): React.JSX.Element {
  const [text, setText] = useState(name)
  useEffect(() => setText(name), [name])
  const trimmed = text.trim()
  const clash = !trimmed ? 'A field needs a name' : taken.includes(trimmed) ? 'Already a field' : null
  return (
    <div className="min-w-0 flex-1">
      <Input
        mono
        value={text}
        onChange={(e) => {
          setText(e.target.value)
          const next = e.target.value.trim()
          if (next && !taken.includes(next)) onRename(next)
        }}
      />
      {clash && <p className="pt-1 text-[11px] text-error-600">{clash}</p>}
    </div>
  )
}
