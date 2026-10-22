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
  type JsonSchema,
  type SchemaType,
} from './schema'

// A form for writing a JSON Schema, for the shapes a value in this store takes:
// a type, a description, choices for a string, named fields for an object (each
// with its own schema, and whether it is required) and an item schema for a
// list. `any` is a schema with no type, which accepts anything.
//
// Keys it has no control for — `default`, `format`, bounds — are kept as they
// were, so a schema written by hand survives being edited here.

export function SchemaBuilder({
  value,
  setValue,
}: {
  value: unknown
  setValue: (value: unknown) => void
}): React.JSX.Element {
  return <SchemaNode schema={isObject(value) ? value : {}} onChange={setValue} />
}

/** The keys that belong to one type and mean nothing to another. */
const TYPE_KEYS = ['type', 'properties', 'required', 'items', 'enum'] as const

function withType(schema: JsonSchema, next: SchemaType | 'any'): JsonSchema {
  const out: JsonSchema = { ...schema }
  for (const key of TYPE_KEYS) delete out[key]
  if (next === 'any') return out
  out.type = next
  if (next === 'object') out.properties = {}
  if (next === 'array') out.items = {}
  return out
}

function SchemaNode({
  schema,
  onChange,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const type = typeOf(schema)
  const description = typeof schema.description === 'string' ? schema.description : ''
  return (
    <div className="space-y-2">
      <div className="flex gap-2">
        <Select
          className="w-32 shrink-0"
          value={type ?? 'any'}
          onChange={(e) => onChange(withType(schema, e.target.value as SchemaType | 'any'))}
        >
          {SCHEMA_TYPES.map((t) => (
            <option key={t} value={t}>
              {t}
            </option>
          ))}
          <option value="any">any</option>
        </Select>
        <Input
          placeholder="Description"
          value={description}
          onChange={(e) => {
            const out = { ...schema }
            if (e.target.value) out.description = e.target.value
            else delete out.description
            onChange(out)
          }}
        />
      </div>
      {type === 'string' && <Choices schema={schema} onChange={onChange} />}
      {type === 'object' && <Properties schema={schema} onChange={onChange} />}
      {type === 'array' && (
        <div className="pl-4">
          <p className="pb-1 text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">Items</p>
          <SchemaNode
            schema={isObject(schema.items) ? schema.items : {}}
            onChange={(items) => onChange({ ...schema, items })}
          />
        </div>
      )}
    </div>
  )
}

/** A string's choices, as a comma-separated list. Empty means any string. */
function Choices({
  schema,
  onChange,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const written = Array.isArray(schema.enum) ? schema.enum.map(String).join(', ') : ''
  // Held as text so a trailing comma, mid-typing, isn't tidied away.
  const [text, setText] = useState(written)
  useEffect(() => {
    if (normalise(text) !== written) setText(written)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [written])
  return (
    <Input
      placeholder="Choices, comma-separated (optional)"
      value={text}
      onChange={(e) => {
        setText(e.target.value)
        const options = split(e.target.value)
        const out = { ...schema }
        if (options.length) out.enum = options
        else delete out.enum
        onChange(out)
      }}
    />
  )
}

const split = (text: string): string[] =>
  text
    .split(',')
    .map((s) => s.trim())
    .filter(Boolean)
const normalise = (text: string): string => split(text).join(', ')

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
    <div className="space-y-2 pl-4">
      {properties.map(([key, property], i) => (
        <div key={i} className="space-y-2 rounded-md bg-gray-50 p-2">
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
        variant="tertiary"
        size="sm"
        onClick={() => write([...properties, [fresh(), { type: 'string' }]], required)}
      >
        <Plus size={13} /> Add field
      </Button>
    </div>
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
