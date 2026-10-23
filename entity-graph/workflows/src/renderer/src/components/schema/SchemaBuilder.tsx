import React, { useEffect, useRef, useState } from 'react'
import { ChevronDown, ChevronRight, Trash03 } from '@untitledui/icons'
import { Dropdown, DropdownSeparator } from '../ui/Dropdown'
import { EditorsProvider, editorsFor, useEditors, type Editors } from './editors'
import { AddLine, BARE_MONO, MenuChip, MenuItem, cx } from './parts'
import { SchemaForm } from './SchemaForm'
import {
  KINDS,
  KIND_NAMES,
  EDITOR_KEYWORD,
  META_SCHEMA,
  MONOSPACE_KEYWORD,
  MULTILINE_KEYWORD,
  POPOUT_KEYWORD,
  isObject,
  kindOf,
  optionNames,
  propertiesOf,
  requiredOf,
  variantsOf,
  type JsonSchema,
  type Kind,
  type SchemaType,
} from './schema'

// A form for writing a JSON Schema, one row per schema: its name — a field's,
// an option's, or what it is for — and at the right end a menu of the kinds a
// schema can be (`kindOf`). Under a divider the same menu ticks the extras that
// kind can carry, and removes the row. An object's fields, an array's items and
// a union's options are rows of their own, a step in.
//
// The rows are lines under hairlines, as the schema form's are. Only the
// outermost builder draws a border. Keys it has no control for are kept as they
// were, so a schema written by hand survives being edited here.

export function SchemaBuilder({
  value,
  setValue,
  bare = false,
  editors,
}: {
  value: unknown
  setValue: (value: unknown) => void
  /** Inside another surface — a line of the schema form: no border of its own. */
  bare?: boolean
  /**
   * The custom editors a field may name, offered under Editor in its menu for
   * the kinds each says it edits — added to any registered further out. See
   * `./editors`.
   */
  editors?: Editors
}): React.JSX.Element {
  return (
    <EditorsProvider editors={editors}>
      <div
        className={cx(
          'w-full divide-y divide-gray-100',
          !bare && 'overflow-hidden rounded-md border border-gray-200 bg-white shadow-xs',
        )}
      >
        <SchemaNode schema={isObject(value) ? value : {}} onChange={setValue} />
      </div>
    </EditorsProvider>
  )
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
    key: POPOUT_KEYWORD,
    label: 'Pop out',
    kinds: ['object', 'array', 'union', 'schema', 'json'],
    initial: true,
  },
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

/**
 * The extras a schema can carry: those of its kind, and — where any editor
 * registered here suits the kind — the choice of a custom editor, made from the
 * editors there are, by the form like any other extra.
 */
function extrasFor(schema: JsonSchema, editors: Editors): Extra[] {
  const kind = kindOf(schema)
  const own = EXTRAS.filter((e) => e.kinds === 'all' || e.kinds.includes(kind))
  const offered = editorsFor(editors, schema)
  if (!offered.length) return own
  return [
    ...own,
    {
      key: EDITOR_KEYWORD,
      label: 'Editor',
      kinds: 'all',
      field: { anyOf: offered.map(([id, e]) => ({ const: id, title: e.name ?? id })) },
      initial: offered[0][0],
    },
  ]
}

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
  // An editor suits a kind, so it goes when the kind does.
  EDITOR_KEYWORD,
  ...EXTRAS.filter((e) => e.kinds !== 'all').map((e) => e.key),
]

function withKind(schema: JsonSchema, next: Kind): JsonSchema {
  const out: JsonSchema = { ...schema }
  for (const key of KIND_KEYS) delete out[key]
  switch (next) {
    case 'json':
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

/** Set for a field of an object: whether it is required. */
interface Slot {
  required: boolean
  onRequired: (required: boolean) => void
}

/**
 * What a row's name box holds: a name to type, or (with no `onChange`) a fixed
 * label. A name is written only while `check` finds nothing wrong with it.
 */
interface Name {
  text: string
  placeholder?: string
  onChange?: (text: string) => void
  check?: (text: string) => string | null
}

function SchemaNode({
  schema,
  onChange,
  name,
  slot,
  onRemove,
}: {
  schema: JsonSchema
  onChange: (schema: JsonSchema) => void
  /** What the name box holds. Absent at the root, which the box around it names. */
  name?: Name
  /** Set for a field of an object: whether it is required. */
  slot?: Slot
  /** Set where the row can be taken out: a field, or a union's option. */
  onRemove?: () => void
}): React.JSX.Element {
  const kind = kindOf(schema)
  const set = (key: string, value: unknown): void => {
    const out = { ...schema }
    if (value === undefined) delete out[key]
    else out[key] = value
    onChange(out)
  }
  const extras = extrasFor(schema, useEditors())
  const shown = extras.filter((e) => e.field && e.key in schema)
  // Anything with more than its name line folds — the schemas it holds, and
  // the lines of its own options — so a deep schema reads as an outline. Open
  // to begin with: a fold is something done to it.
  const nests = kind === 'object' || kind === 'array' || kind === 'union'
  const folds = nests || kind === 'literal' || shown.length > 0
  const [open, setOpen] = useState(true)

  return (
    <div className="divide-y divide-gray-100">
      <NameBox
        name={name}
        fold={folds ? { open, toggle: () => setOpen((o) => !o) } : undefined}
        menu={
          <KindMenu
            schema={schema}
            kind={kind}
            extras={extras}
            onKind={(next) => onChange(withKind(schema, next))}
            onExtra={set}
            slot={slot}
            onRemove={onRemove}
          />
        }
      >
        {open && kind === 'literal' && <LiteralValue schema={schema} onChange={onChange} />}
        {open && shown.length > 0 && (
          <ExtraFields extras={shown} schema={schema} onChange={onChange} />
        )}
      </NameBox>
      {open && kind === 'object' && <Properties schema={schema} onChange={onChange} />}
      {open && kind === 'array' && (
        <Nested>
          <SchemaNode
            schema={isObject(schema.items) ? schema.items : {}}
            onChange={(items) => onChange({ ...schema, items })}
            name={{ text: 'Each item' }}
          />
        </Nested>
      )}
      {open && kind === 'union' && <Variants schema={schema} onChange={onChange} />}
    </div>
  )
}

/**
 * The row's name line — its name, typed into directly, or a fixed label — with
 * the kind menu at the right and a fold at the left for a row with more than
 * this line. The lines of its extras follow in the same surface.
 */
function NameBox({
  name,
  menu,
  fold,
  children,
}: {
  name?: Name
  menu: React.ReactNode
  fold?: { open: boolean; toggle: () => void }
  children?: React.ReactNode
}): React.JSX.Element {
  // Held here, so a name that can't be written yet — empty, or taken — can still
  // be typed, and says why it isn't one.
  const [text, setText] = useState(name?.text ?? '')
  useEffect(() => setText(name?.text ?? ''), [name?.text])
  const problem = name?.check?.(text) ?? null
  return (
    <div className="min-w-0 flex-1 focus-within:bg-brand-50/40">
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
        {name?.onChange ? (
          <input
            className={BARE}
            value={text}
            placeholder={name.placeholder}
            onChange={(e) => {
              setText(e.target.value)
              if (!name.check?.(e.target.value)) name.onChange?.(e.target.value)
            }}
          />
        ) : (
          <span className="min-w-0 flex-1 truncate px-2.5 text-[13px] text-gray-500">
            {name?.text}
          </span>
        )}
        <div className="shrink-0 pr-1">{menu}</div>
      </div>
      {problem && <p className="px-2.5 pb-1.5 text-[11px] text-error-600">{problem}</p>}
      {children}
    </div>
  )
}

/** The name line's text box: the shared one, filling what the menu leaves. */
const BARE =
  'h-full min-w-0 flex-1 bg-transparent px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 focus:outline-none'

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
  extras,
  onKind,
  onExtra,
  slot,
  onRemove,
}: {
  schema: JsonSchema
  kind: Kind
  /** The extras this schema can carry, ticked on and off under the divider. */
  extras: Extra[]
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
          {KIND_NAMES[kind]}
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
              {KIND_NAMES[k]}
            </MenuItem>
          ))}
          <DropdownSeparator />
          {slot && (
            <MenuItem on={!slot.required} onClick={() => slot.onRequired(!slot.required)}>
              Optional
            </MenuItem>
          )}
          {extras.map((extra) => {
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
  // The rows run on as lines: a step in from the left and a hairline between
  // each, with nothing to space them apart.
  return <div className="ml-4 divide-y divide-gray-100 border-l border-gray-100">{children}</div>
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
          // Its name in the form's picker, written as its `title`.
          name={{
            text: typeof variant.title === 'string' ? variant.title : '',
            placeholder: defaults[i],
            onChange: (title) => {
              const next: JsonSchema = { ...variant, title }
              if (!title) delete next.title
              write(variants.map((v, j) => (j === i ? next : v)))
            },
          }}
          onChange={(next) => write(variants.map((v, j) => (j === i ? next : v)))}
          onRemove={() => write(variants.filter((_, j) => j !== i))}
        />
      ))}
      <AddLine onClick={() => write([...variants, { const: '' }])}>Add option</AddLine>
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
          name={{
            text: key,
            placeholder: 'Field name',
            check: (text) =>
              !text.trim()
                ? 'A field needs a name'
                : properties.some(([k], j) => j !== i && k === text.trim())
                  ? 'Already a field'
                  : null,
            onChange: (text) =>
              write(
                properties.map(([k, p], j) => (j === i ? [text.trim(), p] : [k, p])),
                required.map((r) => (r === key ? text.trim() : r)),
              ),
          }}
          slot={{
            required: required.includes(key),
            onRequired: (on) =>
              write(properties, on ? [...required, key] : required.filter((r) => r !== key)),
          }}
          onRemove={() =>
            write(
              properties.filter((_, j) => j !== i),
              required,
            )
          }
        />
      ))}
      <AddLine
        // New fields are required: optional is the thing worth saying.
        onClick={() =>
          write([...properties, [fresh(), { type: 'string' }]], [...required, fresh()])
        }
      >
        Add field
      </AddLine>
    </Nested>
  )
}
