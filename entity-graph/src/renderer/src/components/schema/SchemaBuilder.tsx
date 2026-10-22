import React, { useEffect, useState } from 'react'
import { Check, ChevronDown, Plus, Trash03 } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { Button } from '../ui/Button'
import { Dropdown, DropdownSeparator } from '../ui/Dropdown'
import { Input } from '../ui/Input'
import { Select } from '../ui/Select'
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

// A form for writing a JSON Schema, one row per schema. A row is a box holding
// its name — a field's name, or what the schema is for — with a menu at its
// right end saying what *kind* of schema it is:
//
//   a type       string, number, integer, boolean, null
//   object       named fields, each its own row, each required unless optional
//   array        one row for its items
//   literal      exactly one value (`const`)
//   union        any of several rows (`anyOf`) — a union of literals is a set
//                of choices
//   any          no constraint at all
//
// Beneath the kinds the same menu ticks the extras a kind can carry —
// optional, a description, bounds, a format — and, last, removes the row. Each
// ticked extra that takes a
// value gets a box under the row. Objects, arrays and unions draw their own rows
// in a panel one step in. Keys the builder has no control for are kept as they
// were, so a schema written by hand survives being edited here.

export function SchemaBuilder({
  value,
  setValue,
}: {
  value: unknown
  setValue: (value: unknown) => void
}): React.JSX.Element {
  return <SchemaNode schema={isObject(value) ? value : {}} onChange={setValue} label="Schema" />
}

const KINDS = [...SCHEMA_TYPES, 'literal', 'union', 'any'] as const
type Kind = (typeof KINDS)[number]

/** Which kind a schema is, as the builder reads it. */
function kindOf(schema: JsonSchema): Kind {
  if ('const' in schema) return 'literal'
  if (variantsOf(schema)) return 'union'
  return typeOf(schema) ?? 'any'
}

/**
 * The extras a kind can carry: JSON Schema keywords, ticked on and off in the
 * menu. One with an `input` takes a value, entered in a box under the row; one
 * without is the tick alone, written as `set`.
 */
interface Extra {
  key: string
  label: string
  kinds: readonly Kind[] | 'all'
  input?: 'text' | 'number' | 'format'
  /** What ticking it writes, before anything is typed. */
  initial: unknown
}

const FORMATS = ['date', 'date-time', 'time', 'email', 'uri', 'uuid'] as const

const EXTRAS: Extra[] = [
  { key: 'description', label: 'Description', kinds: 'all', input: 'text', initial: '' },
  { key: 'minimum', label: 'Minimum', kinds: ['number', 'integer'], input: 'number', initial: 0 },
  { key: 'maximum', label: 'Maximum', kinds: ['number', 'integer'], input: 'number', initial: 100 },
  { key: 'minLength', label: 'Minimum length', kinds: ['string'], input: 'number', initial: 1 },
  { key: 'maxLength', label: 'Maximum length', kinds: ['string'], input: 'number', initial: 100 },
  { key: 'pattern', label: 'Pattern', kinds: ['string'], input: 'text', initial: '' },
  { key: 'format', label: 'Format', kinds: ['string'], input: 'format', initial: 'date' },
  { key: 'minItems', label: 'Minimum items', kinds: ['array'], input: 'number', initial: 1 },
  { key: 'maxItems', label: 'Maximum items', kinds: ['array'], input: 'number', initial: 10 },
  { key: 'uniqueItems', label: 'Unique items', kinds: ['array'], initial: true },
  { key: 'additionalProperties', label: 'No other fields', kinds: ['object'], initial: false },
]

const extrasFor = (kind: Kind): Extra[] =>
  EXTRAS.filter((e) => e.kinds === 'all' || e.kinds.includes(kind))

/** The keys that belong to one kind and mean nothing to another. */
const KIND_KEYS = [
  'type',
  'properties',
  'required',
  'items',
  'enum',
  'const',
  'anyOf',
  'oneOf',
  ...EXTRAS.filter((e) => e.kinds !== 'all').map((e) => e.key),
]

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

/** How a field sits in the object holding it: its name, and whether it is required. */
interface Slot {
  name: string
  taken: string[]
  onRename: (name: string) => void
  required: boolean
  onRequired: (required: boolean) => void
  onRemove: () => void
}

function SchemaNode({
  schema,
  onChange,
  slot,
  label,
  onRemove,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
  /** Set for a field of an object: the name box is its name. */
  slot?: Slot
  /** What the box says when there is no name to edit — "Each item", "Option 2". */
  label?: string
  /** Set for a union's option, which can be taken out. */
  onRemove?: () => void
}): React.JSX.Element {
  const kind = kindOf(schema)
  const set = (key: string, value: unknown): void => {
    const out = { ...schema }
    if (value === undefined) delete out[key]
    else out[key] = value
    onChange(out)
  }
  const shown = extrasFor(kind).filter((e) => e.input && e.key in schema)
  const remove = slot?.onRemove ?? onRemove

  return (
    <div className="space-y-1.5">
      <NameBox
        slot={slot}
        label={label}
        menu={
          <KindMenu
            schema={schema}
            kind={kind}
            onKind={(next) => onChange(withKind(schema, next))}
            onExtra={set}
            slot={slot}
            onRemove={remove}
          />
        }
      />
      {kind === 'literal' && <LiteralValue schema={schema} onChange={onChange} />}
      {shown.map((extra) => (
        <ExtraInput key={extra.key} extra={extra} value={schema[extra.key]} onChange={(v) => set(extra.key, v)} />
      ))}
      {kind === 'object' && <Properties schema={schema} onChange={onChange} />}
      {kind === 'array' && (
        <Nested>
          <SchemaNode
            schema={isObject(schema.items) ? schema.items : {}}
            onChange={(items) => onChange({ ...schema, items })}
            label="Each item"
          />
        </Nested>
      )}
      {kind === 'union' && <Variants schema={schema} onChange={onChange} />}
    </div>
  )
}

/**
 * The row's box: a field's name, full width, typed into directly — or, for a
 * schema with no name, what it is for — with the kind menu inside it at the
 * right. One surface, styled as an input, so the menu reads as part of the name.
 */
function NameBox({
  slot,
  label,
  menu,
}: {
  slot?: Slot
  label?: string
  menu: React.ReactNode
}): React.JSX.Element {
  const [text, setText] = useState(slot?.name ?? '')
  useEffect(() => setText(slot?.name ?? ''), [slot?.name])
  const trimmed = text.trim()
  const clash = !slot
    ? null
    : !trimmed
      ? 'A field needs a name'
      : slot.taken.includes(trimmed)
        ? 'Already a field'
        : null
  return (
    <div className="min-w-0 flex-1">
      <div
        className={cn(
          'flex h-8 items-center rounded-md border border-gray-200 bg-white shadow-xs',
          'focus-within:border-brand-500 focus-within:ring-2 focus-within:ring-brand-500/40',
        )}
      >
        {slot ? (
          <input
            className="h-full min-w-0 flex-1 bg-transparent px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 focus:outline-none"
            value={text}
            placeholder="Field name"
            onChange={(e) => {
              setText(e.target.value)
              const next = e.target.value.trim()
              if (next && !slot.taken.includes(next)) slot.onRename(next)
            }}
          />
        ) : (
          <span className="min-w-0 flex-1 truncate px-2.5 text-[13px] text-gray-500">{label}</span>
        )}
        <div className="shrink-0 pr-1">{menu}</div>
      </div>
      {clash && <p className="pt-1 text-[11px] text-error-600">{clash}</p>}
    </div>
  )
}

/** One line of the menu, with a tick where it is on. */
function MenuItem({
  on,
  onClick,
  children,
}: {
  on: boolean
  onClick: () => void
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <button
      className="flex w-full items-center gap-2 px-3 py-1.5 text-left text-[13px] text-gray-700 hover:bg-gray-100/70 focus:outline-none"
      onClick={onClick}
    >
      <span className="flex size-3.5 shrink-0 items-center justify-center text-brand-600">
        {on && <Check size={13} />}
      </span>
      {children}
    </button>
  )
}

/** The kinds, then — under a divider — the extras this kind can carry. */
function KindMenu({
  schema,
  kind,
  onKind,
  onExtra,
  slot,
  onRemove,
}: {
  schema: JsonSchema
  kind: Kind
  onKind: (kind: Kind) => void
  onExtra: (key: string, value: unknown) => void
  slot?: Slot
  /** Set where the row can be taken out: a field, or a union's option. */
  onRemove?: () => void
}): React.JSX.Element {
  return (
    <Dropdown
      align="right"
      menuClassName="mt-1 max-h-[60vh] overflow-y-auto"
      trigger={({ toggle }) => (
        <button
          className="flex h-6 items-center gap-0.5 rounded bg-gray-100 px-1.5 text-[12px] text-gray-600 hover:bg-gray-200 focus:outline-none"
          onClick={toggle}
        >
          {kind}
          {slot && !slot.required && <span className="text-gray-400">?</span>}
          <ChevronDown size={12} className="text-gray-400" />
        </button>
      )}
    >
      {(close) => (
        <>
          {KINDS.map((k) => (
            <MenuItem
              key={k}
              on={k === kind}
              onClick={() => {
                if (k !== kind) onKind(k)
                close()
              }}
            >
              {k}
            </MenuItem>
          ))}
          <DropdownSeparator />
          {slot && (
            <MenuItem on={!slot.required} onClick={() => slot.onRequired(!slot.required)}>
              Optional
            </MenuItem>
          )}
          {extrasFor(kind).map((extra) => {
            const on = extra.input ? extra.key in schema : schema[extra.key] === extra.initial
            return (
              <MenuItem
                key={extra.key}
                on={on}
                onClick={() => onExtra(extra.key, on ? undefined : extra.initial)}
              >
                {extra.label}
              </MenuItem>
            )
          })}
          {onRemove && (
            <button
              className="flex w-full items-center gap-2 px-3 py-1.5 text-left text-[13px] text-error-600 hover:bg-error-50 focus:outline-none"
              onClick={() => {
                close()
                onRemove()
              }}
            >
              <Trash03 size={13} className="shrink-0" />
              Remove
            </button>
          )}
        </>
      )}
    </Dropdown>
  )
}

/** The box an extra that takes a value is entered in, labelled with what it is. */
function ExtraInput({
  extra,
  value,
  onChange,
}: {
  extra: Extra
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  return (
    <div className="flex items-center gap-2 pl-2.5">
      <span className="w-28 shrink-0 text-[11px] text-gray-500">{extra.label}</span>
      <div className="min-w-0 flex-1">
        {extra.input === 'format' ? (
          <Select value={String(value ?? '')} onChange={(e) => onChange(e.target.value)}>
            {FORMATS.map((f) => (
              <option key={f} value={f}>
                {f}
              </option>
            ))}
          </Select>
        ) : extra.input === 'number' ? (
          <NumberInput value={value} onChange={onChange} />
        ) : (
          <Input
            mono={extra.key === 'pattern'}
            value={typeof value === 'string' ? value : ''}
            onChange={(e) => onChange(e.target.value)}
          />
        )}
      </div>
    </div>
  )
}

/** A number, held as text so a half-typed `-` survives; only a number is written. */
function NumberInput({
  value,
  onChange,
}: {
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const [text, setText] = useState(typeof value === 'number' ? String(value) : '')
  useEffect(() => {
    if (typeof value === 'number' && Number(text) !== value) setText(String(value))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [value])
  return (
    <Input
      mono
      inputMode="decimal"
      value={text}
      onChange={(e) => {
        setText(e.target.value)
        const n = Number(e.target.value)
        if (e.target.value.trim() && Number.isFinite(n)) onChange(n)
      }}
    />
  )
}

/**
 * Where a nested schema's rows sit: one step in from the row they belong to,
 * with a soft rail down the left so the depth reads. Indented on the left only —
 * the rows keep the full width to the right.
 */
function Nested({ children }: { children: React.ReactNode }): React.JSX.Element {
  return <div className="ml-2 space-y-2 border-l-2 border-gray-100 pl-4">{children}</div>
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
    <div className="flex items-center gap-2 pl-2.5">
      <span className="w-28 shrink-0 text-[11px] text-gray-500">Value</span>
      <div className="min-w-0 flex-1">
        <Input
          mono
          value={text}
          onChange={(e) => {
            setText(e.target.value)
            onChange({ ...schema, const: readLiteral(e.target.value) })
          }}
        />
      </div>
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
      {variants.map((variant, i) => (
        <SchemaNode
          key={i}
          schema={variant}
          label={`Option ${i + 1}`}
          onChange={(next) => write(variants.map((v, j) => (j === i ? next : v)))}
          onRemove={() => write(variants.filter((_, j) => j !== i))}
        />
      ))}
      <Button variant="tertiary" size="sm" onClick={() => write([...variants, { const: '' }])}>
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
      {properties.map(([key, property], i) => (
        <SchemaNode
          key={i}
          schema={property}
          onChange={(next) => write(properties.map(([k, p], j) => (j === i ? [k, next] : [k, p])), required)}
          slot={{
            name: key,
            taken: properties.filter((_, j) => j !== i).map(([k]) => k),
            onRename: (name) =>
              write(
                properties.map(([k, p], j) => (j === i ? [name, p] : [k, p])),
                required.map((r) => (r === key ? name : r)),
              ),
            required: required.includes(key),
            onRequired: (on) => write(properties, on ? [...required, key] : required.filter((r) => r !== key)),
            onRemove: () => write(properties.filter((_, j) => j !== i), required),
          }}
        />
      ))}
      <Button
        variant="tertiary"
        size="sm"
        // New fields are required: optional is the thing worth saying.
        onClick={() => write([...properties, [fresh(), { type: 'string' }]], [...required, fresh()])}
      >
        <Plus size={13} /> Add field
      </Button>
    </Nested>
  )
}
