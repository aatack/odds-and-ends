import React, { createContext, useContext, useEffect, useRef, useState } from 'react'
import { Check, ChevronDown, ChevronRight, Plus, Trash03 } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { Button } from '../ui/Button'
import { Dropdown, DropdownSeparator } from '../ui/Dropdown'
import { MenuChip, MenuItem } from './Menu'
import { SchemaForm } from './SchemaForm'
import {
  META_SCHEMA,
  MONOSPACE_KEYWORD,
  MULTILINE_KEYWORD,
  SCHEMA_TYPES,
  isSchemaRef,
  isObject,
  optionNames,
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
//   schema       a value that is itself a schema (a `$ref` to the meta-schema),
//                which the form edits with this builder
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
  bare = false,
}: {
  value: unknown
  setValue: (value: unknown) => void
  /**
   * Inside another surface — a line of the schema form — rather than on its own.
   * The builder then draws no boxes of its own: its rows are lines divided by
   * hairlines, filling the space they are given, as the form's own lines are.
   */
  bare?: boolean
}): React.JSX.Element {
  return (
    <Surface.Provider value={bare}>
      <div className={cn(bare && 'w-full divide-y divide-gray-100')}>
        {/* Bare, the line it sits in already says what it is. */}
        <SchemaNode
          schema={isObject(value) ? value : {}}
          onChange={setValue}
          label={bare ? '' : 'Schema'}
        />
      </div>
    </Surface.Provider>
  )
}

/** Whether the builder is bare (see {@link SchemaBuilder}). */
const Surface = createContext(false)

/** The kinds the menu offers, in its order. */
const KINDS = [...SCHEMA_TYPES, 'literal', 'union', 'schema'] as const
/** A kind, or `unset`: a schema that says nothing yet, waiting for one to be chosen. */
type Kind = (typeof KINDS)[number] | 'unset'

const KIND_LABELS: Record<Kind, string> = {
  string: 'String',
  number: 'Number',
  integer: 'Integer',
  boolean: 'Boolean',
  null: 'Null',
  object: 'Object',
  array: 'Array',
  literal: 'Literal',
  union: 'Union',
  schema: 'Schema',
  unset: 'Choose type',
}

/** Which kind a schema is, as the builder reads it. */
function kindOf(schema: JsonSchema): Kind {
  if ('const' in schema) return 'literal'
  if (isSchemaRef(schema)) return 'schema'
  if (variantsOf(schema)) return 'union'
  return typeOf(schema) ?? 'unset'
}

/**
 * The extras a kind can carry: JSON Schema keywords, ticked on and off in the
 * menu. One with a `field` takes a value: `field` is that value's own schema,
 * and the ticked ones are drawn by the schema form, under the row. One without
 * is the tick alone, written as `initial`.
 */
interface Extra {
  key: string
  label: string
  kinds: readonly Kind[] | 'all'
  field?: JsonSchema
  /** What ticking it writes, before anything is typed. */
  initial: unknown
}

const FORMATS = ['date', 'date-time', 'time', 'email', 'uri', 'uuid'] as const

const EXTRAS: Extra[] = [
  {
    key: 'description',
    label: 'Description',
    kinds: 'all',
    field: { type: 'string', [MULTILINE_KEYWORD]: true },
    initial: '',
  },
  {
    key: 'minimum',
    label: 'Minimum',
    kinds: ['number', 'integer'],
    field: { type: 'number' },
    initial: 0,
  },
  {
    key: 'maximum',
    label: 'Maximum',
    kinds: ['number', 'integer'],
    field: { type: 'number' },
    initial: 100,
  },
  {
    key: 'minLength',
    label: 'Minimum length',
    kinds: ['string'],
    field: { type: 'integer', minimum: 0 },
    initial: 1,
  },
  {
    key: 'maxLength',
    label: 'Maximum length',
    kinds: ['string'],
    field: { type: 'integer', minimum: 0 },
    initial: 100,
  },
  { key: MULTILINE_KEYWORD, label: 'Multi-line', kinds: ['string'], initial: true },
  { key: MONOSPACE_KEYWORD, label: 'Monospace', kinds: ['string'], initial: true },
  {
    key: 'pattern',
    label: 'Pattern',
    kinds: ['string'],
    field: { type: 'string', [MONOSPACE_KEYWORD]: true },
    initial: '',
  },
  {
    key: 'format',
    label: 'Format',
    kinds: ['string'],
    field: { anyOf: FORMATS.map((f) => ({ const: f })) },
    initial: 'date',
  },
  {
    key: 'minItems',
    label: 'Minimum items',
    kinds: ['array'],
    field: { type: 'integer', minimum: 0 },
    initial: 1,
  },
  {
    key: 'maxItems',
    label: 'Maximum items',
    kinds: ['array'],
    field: { type: 'integer', minimum: 0 },
    initial: 10,
  },
  {
    key: 'uniqueItems',
    label: 'Unique items',
    kinds: ['array'],
    initial: true,
  },
  {
    key: 'additionalProperties',
    label: 'No other fields',
    kinds: ['object'],
    initial: false,
  },
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
  '$ref',
  ...EXTRAS.filter((e) => e.kinds !== 'all').map((e) => e.key),
]

function withKind(schema: JsonSchema, next: Kind): JsonSchema {
  const out: JsonSchema = { ...schema }
  for (const key of KIND_KEYS) delete out[key]
  switch (next) {
    case 'unset':
      return out
    case 'schema':
      return { ...out, $ref: META_SCHEMA }
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
  named,
  onRemove,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
  /** Set for a field of an object: the name box is its name. */
  slot?: Slot
  /** What the box says when there is no name to edit — "Each item". */
  label?: string
  /**
   * Set for a union's option: the box is its name in the form's picker, written
   * as the schema's `title`, and `placeholder` is what it is called without one.
   */
  named?: { placeholder: string }
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
  const shown = extrasFor(kind).filter((e) => e.field && e.key in schema)
  const remove = slot?.onRemove ?? onRemove
  // Anything holding schemas of its own folds, so a deep schema reads as an
  // outline. Open to begin with: a fold is something done to it.
  const nests = kind === 'object' || kind === 'array' || kind === 'union'
  const [open, setOpen] = useState(true)

  const bare = useContext(Surface)
  return (
    <div className={bare ? 'divide-y divide-gray-100' : 'space-y-1.5'}>
      <NameBox
        slot={slot}
        label={label}
        title={
          named && {
            value: typeof schema.title === 'string' ? schema.title : '',
            placeholder: named.placeholder,
            onChange: (title: string) => set('title', title || undefined),
          }
        }
        fold={nests ? { open, toggle: () => setOpen((o) => !o) } : undefined}
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
      >
        {kind === 'literal' && <LiteralValue schema={schema} onChange={onChange} />}
        {shown.length > 0 && <ExtraFields extras={shown} schema={schema} onChange={onChange} />}
      </NameBox>
      {open && kind === 'object' && <Properties schema={schema} onChange={onChange} />}
      {open && kind === 'array' && (
        <Nested>
          <SchemaNode
            schema={isObject(schema.items) ? schema.items : {}}
            onChange={(items) => onChange({ ...schema, items })}
            label="Each item"
          />
        </Nested>
      )}
      {open && kind === 'union' && <Variants schema={schema} onChange={onChange} />}
    </div>
  )
}

/**
 * The row's box: a field's name, full width, typed into directly — or, for a
 * schema with no name, what it is for — with the kind menu inside it at the
 * right, and a fold at the left for a schema that holds others. The extras the
 * schema carries are lines of the same box, under a hairline each: one surface,
 * so all of it reads as the one field.
 */
function NameBox({
  slot,
  label,
  title,
  menu,
  fold,
  children,
}: {
  slot?: Slot
  label?: string
  /** An editable name that isn't a field's: a union option's `title`. */
  title?: { value: string; placeholder: string; onChange: (title: string) => void } | undefined
  menu: React.ReactNode
  fold?: { open: boolean; toggle: () => void }
  children?: React.ReactNode
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
  const bare = useContext(Surface)
  return (
    <div className="min-w-0 flex-1">
      <div
        className={cn(
          bare
            ? 'focus-within:bg-brand-50/40'
            : 'rounded-md border border-gray-200 bg-white shadow-xs focus-within:border-brand-500 focus-within:ring-2 focus-within:ring-brand-500/40',
        )}
      >
        <div className="flex h-8 items-center">
          {fold && (
            <button
              className="flex h-full shrink-0 items-center pl-1.5 text-gray-400 hover:text-gray-700 focus:outline-none"
              title={fold.open ? 'Fold' : 'Unfold'}
              onClick={fold.toggle}
            >
              {fold.open ? <ChevronDown size={14} /> : <ChevronRight size={14} />}
            </button>
          )}
          {slot ? (
            <input
              className={BARE}
              value={text}
              placeholder="Field name"
              onChange={(e) => {
                setText(e.target.value)
                const next = e.target.value.trim()
                if (next && !slot.taken.includes(next)) slot.onRename(next)
              }}
            />
          ) : title ? (
            <input
              className={BARE}
              value={title.value}
              placeholder={title.placeholder}
              onChange={(e) => title.onChange(e.target.value)}
            />
          ) : (
            <span className="min-w-0 flex-1 truncate px-2.5 text-[13px] text-gray-500">
              {label}
            </span>
          )}
          <div className="shrink-0 pr-1">{menu}</div>
        </div>
        {children}
      </div>
      {clash && <p className="pt-1 text-[11px] text-error-600">{clash}</p>}
    </div>
  )
}

/** A text box with no surface of its own, for a line inside a name box. */
const BARE =
  'h-full min-w-0 flex-1 bg-transparent px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 focus:outline-none'
const BARE_MONO = cn(BARE, 'font-mono text-[12.5px]')

/** One line of a name box under the name: a hairline, a label, and its control. */
function BoxLine({
  label,
  children,
}: {
  label: string
  children: React.ReactNode
}): React.JSX.Element {
  const line = useRef<HTMLDivElement>(null)
  return (
    <div ref={line} className="flex h-8 items-stretch border-t border-gray-100">
      {/* Pressing the label puts the caret in its control. */}
      <span
        className="flex w-28 shrink-0 cursor-default items-center border-r border-gray-100 pl-2.5 text-[11px] text-gray-500"
        onClick={() => line.current?.querySelector<HTMLElement>('input, select')?.focus()}
      >
        {label}
      </span>
      {children}
    </div>
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
      trigger={({ toggle }) => (
        <MenuChip onClick={toggle}>
          {KIND_LABELS[kind]}
          {slot && !slot.required && <span className="text-gray-400">?</span>}
        </MenuChip>
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
              {KIND_LABELS[k]}
            </MenuItem>
          ))}
          <DropdownSeparator />
          {slot && (
            <MenuItem on={!slot.required} onClick={() => slot.onRequired(!slot.required)}>
              Optional
            </MenuItem>
          )}
          {extrasFor(kind).map((extra) => {
            const on = extra.field ? extra.key in schema : schema[extra.key] === extra.initial
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

/**
 * The ticked extras that take a value, drawn by the schema form — bare, as lines
 * of the name box — from each extra's own schema. So a description is the
 * form's multi-line box, a format its picker, a bound its number box, and any
 * value that doesn't fit says so, as it would anywhere else.
 */
function ExtraFields({
  extras,
  schema,
  onChange,
}: {
  extras: Extra[]
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
}): React.JSX.Element {
  const fields: JsonSchema = {
    type: 'object',
    properties: Object.fromEntries(extras.map((e) => [e.key, { ...e.field, title: e.label }])),
  }
  const values = Object.fromEntries(extras.map((e) => [e.key, schema[e.key]]))
  return (
    <div className="border-t border-gray-100">
      <SchemaForm
        bare
        schema={fields}
        value={values}
        setValue={(next) => {
          const out = { ...schema }
          // A box emptied is the extra still ticked, with nothing in it yet: it
          // comes off from the menu, not by clearing it.
          for (const e of extras) {
            const v = isObject(next) ? next[e.key] : undefined
            if (v !== undefined) out[e.key] = v
          }
          onChange(out)
        }}
      />
    </div>
  )
}

/**
 * Where a nested schema's rows sit: one step in from the row they belong to,
 * with a soft rail down the left so the depth reads. Indented on the left only —
 * the rows keep the full width to the right.
 */
function Nested({ children }: { children: React.ReactNode }): React.JSX.Element {
  const bare = useContext(Surface)
  // Bare, the rows run on as lines: a step in from the left and a hairline
  // between each, with nothing to space them apart.
  if (bare)
    return <div className="ml-4 divide-y divide-gray-100 border-l border-gray-100">{children}</div>
  return <div className="ml-2 space-y-2 border-l-2 border-gray-100 pl-4">{children}</div>
}

/** The last row of a nested run: adds another field or option. */
function AddRow({
  onClick,
  children,
}: {
  onClick: () => void
  children: React.ReactNode
}): React.JSX.Element {
  const bare = useContext(Surface)
  if (bare) {
    return (
      <button
        className="flex h-8 w-full items-center gap-1.5 px-2.5 text-[12px] text-gray-400 hover:bg-gray-50 hover:text-gray-700 focus:outline-none focus-visible:bg-gray-50"
        onClick={onClick}
      >
        <Plus size={13} /> {children}
      </button>
    )
  }
  return (
    <Button variant="tertiary" size="sm" onClick={onClick}>
      <Plus size={13} /> {children}
    </Button>
  )
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
    <BoxLine label="Value">
      <input
        className={BARE_MONO}
        value={text}
        onChange={(e) => {
          setText(e.target.value)
          onChange({ ...schema, const: readLiteral(e.target.value) })
        }}
      />
    </BoxLine>
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
  // What each is called without a name of its own — so the box shows it.
  const defaults = optionNames(variants.map(({ title: _, ...rest }) => rest))
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
          named={{ placeholder: defaults[i] }}
          onChange={(next) => write(variants.map((v, j) => (j === i ? next : v)))}
          onRemove={() => write(variants.filter((_, j) => j !== i))}
        />
      ))}
      <AddRow onClick={() => write([...variants, { const: '' }])}>Add option</AddRow>
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
          onChange={(next) =>
            write(
              properties.map(([k, p], j) => (j === i ? [k, next] : [k, p])),
              required,
            )
          }
          slot={{
            name: key,
            taken: properties.filter((_, j) => j !== i).map(([k]) => k),
            onRename: (name) =>
              write(
                properties.map(([k, p], j) => (j === i ? [name, p] : [k, p])),
                required.map((r) => (r === key ? name : r)),
              ),
            required: required.includes(key),
            onRequired: (on) =>
              write(properties, on ? [...required, key] : required.filter((r) => r !== key)),
            onRemove: () =>
              write(
                properties.filter((_, j) => j !== i),
                required,
              ),
          }}
        />
      ))}
      <AddRow
        // New fields are required: optional is the thing worth saying.
        onClick={() =>
          write([...properties, [fresh(), { type: 'string' }]], [...required, fresh()])
        }
      >
        Add field
      </AddRow>
    </Nested>
  )
}
