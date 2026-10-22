import React from 'react'
import { Plus, Trash03 } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { Button } from '../ui/Button'
import { IconButton } from '../ui/IconButton'
import { Input } from '../ui/Input'
import { Select } from '../ui/Select'
import { Switch } from '../ui/Switch'
import { JsonEditor } from './JsonEditor'
import {
  defaultFor,
  isObject,
  propertiesOf,
  requiredOf,
  typeOf,
  type JsonSchema,
} from './schema'

// A form for a value, drawn from a JSON Schema. It covers the shapes a value in
// this store actually takes — strings, numbers, booleans, choices, objects of
// named fields and lists — and falls back to a JSON box for anything else (a
// union, a schema that says nothing), so no value is ever uneditable.
//
// Every edit hands the whole value back through `setValue`. Keys the schema
// doesn't name are kept as they were: the form edits what it knows about and
// leaves the rest alone.

export function SchemaForm({
  value,
  setValue,
  schema,
}: {
  value: unknown
  setValue: (value: unknown) => void
  schema: JsonSchema
}): React.JSX.Element {
  return <SchemaField schema={schema} value={value} onChange={setValue} />
}

/** A quiet label, the same micro-label `Field` uses, with what the field is for. */
function Label({
  name,
  schema,
  required,
}: {
  name: string
  schema: JsonSchema
  required: boolean
}): React.JSX.Element {
  const title = typeof schema.title === 'string' ? schema.title : name
  const description = typeof schema.description === 'string' ? schema.description : null
  return (
    <div className="pb-1">
      <span className="text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
        {title}
      </span>
      {required && <span className="pl-1.5 text-[11px] text-gray-400">required</span>}
      {description && <p className="text-[11px] text-gray-400">{description}</p>}
    </div>
  )
}

function SchemaField({
  schema,
  value,
  onChange,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  if (Array.isArray(schema.enum)) return <EnumField options={schema.enum} value={value} onChange={onChange} />
  if ('const' in schema) {
    return <span className="font-mono text-[12.5px] text-gray-500">{JSON.stringify(schema.const)}</span>
  }
  switch (typeOf(schema)) {
    case 'string':
      return (
        <Input
          value={typeof value === 'string' ? value : ''}
          placeholder={typeof schema.default === 'string' ? schema.default : undefined}
          onChange={(e) => onChange(e.target.value)}
        />
      )
    case 'number':
    case 'integer':
      return <NumberField integer={typeOf(schema) === 'integer'} value={value} onChange={onChange} />
    case 'boolean':
      return <Switch checked={value === true} onChange={onChange} label="Toggle" />
    case 'object':
      return <ObjectField schema={schema} value={value} onChange={onChange} />
    case 'array':
      return <ArrayField schema={schema} value={value} onChange={onChange} />
    default:
      return <JsonEditor value={value} setValue={onChange} placeholder="Any JSON" />
  }
}

function EnumField({
  options,
  value,
  onChange,
}: {
  options: unknown[]
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  // Options can be any JSON, so the select works on their indices.
  const at = options.findIndex((o) => JSON.stringify(o) === JSON.stringify(value))
  return (
    <Select value={at < 0 ? '' : String(at)} onChange={(e) => onChange(options[Number(e.target.value)])}>
      {at < 0 && <option value="">Choose…</option>}
      {options.map((o, i) => (
        <option key={i} value={i}>
          {typeof o === 'string' ? o : JSON.stringify(o)}
        </option>
      ))}
    </Select>
  )
}

/**
 * A number, typed as text so a half-typed `-` or `1.` isn't thrown away. Only a
 * value that reads as a number (an integer, for an integer field) is written.
 */
function NumberField({
  integer,
  value,
  onChange,
}: {
  integer: boolean
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const [text, setText] = React.useState(typeof value === 'number' ? String(value) : '')
  React.useEffect(() => {
    if (typeof value === 'number' && Number(text) !== value) setText(String(value))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [value])
  const parsed = Number(text)
  const bad = text.trim() !== '' && (!Number.isFinite(parsed) || (integer && !Number.isInteger(parsed)))
  return (
    <div>
      <Input
        mono
        inputMode={integer ? 'numeric' : 'decimal'}
        value={text}
        onChange={(e) => {
          const next = e.target.value
          setText(next)
          const n = Number(next)
          if (next.trim() === '') onChange(undefined)
          else if (Number.isFinite(n) && (!integer || Number.isInteger(n))) onChange(n)
        }}
      />
      {bad && <p className="pt-1 text-[11px] text-error-600">{integer ? 'Not a whole number' : 'Not a number'}</p>}
    </div>
  )
}

function ObjectField({
  schema,
  value,
  onChange,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const properties = propertiesOf(schema)
  const required = requiredOf(schema)
  const current = isObject(value) ? value : {}
  // An object that names no fields is a bag of anything: JSON is the honest form.
  if (!properties.length) return <JsonEditor value={value} setValue={onChange} placeholder="{}" />
  return (
    <div className="space-y-3">
      {properties.map(([key, property]) => (
        <div key={key}>
          <Label name={key} schema={property} required={required.includes(key)} />
          <SchemaField
            schema={property}
            value={current[key]}
            onChange={(next) => {
              const out = { ...current }
              // Taking a field away leaves no key, rather than an `undefined`.
              if (next === undefined) delete out[key]
              else out[key] = next
              onChange(out)
            }}
          />
        </div>
      ))}
    </div>
  )
}

function ArrayField({
  schema,
  value,
  onChange,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const items = isObject(schema.items) ? schema.items : {}
  const list = Array.isArray(value) ? value : []
  const nested = typeOf(items) === 'object' || typeOf(items) === 'array'
  return (
    <div className="space-y-1.5">
      {list.map((item, i) => (
        <div key={i} className={cn('flex items-start gap-1.5', nested && 'rounded-md bg-gray-50 p-2')}>
          <div className="min-w-0 flex-1">
            <SchemaField
              schema={items}
              value={item}
              onChange={(next) => onChange(list.map((old, j) => (j === i ? next : old)))}
            />
          </div>
          <IconButton title="Remove" onClick={() => onChange(list.filter((_, j) => j !== i))}>
            <Trash03 size={14} />
          </IconButton>
        </div>
      ))}
      <Button variant="tertiary" size="sm" onClick={() => onChange([...list, defaultFor(items)])}>
        <Plus size={13} /> Add
      </Button>
    </div>
  )
}
