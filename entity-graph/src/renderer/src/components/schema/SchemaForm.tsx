import React, { useEffect, useRef, useState } from 'react'
import { Plus, Trash03 } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { Switch } from '../ui/Switch'
import { TextEditor } from '../ui/TextEditor'
import { JsonEditor } from './JsonEditor'
import {
  defaultFor,
  isObject,
  MULTILINE_KEYWORD,
  matches,
  problemOf,
  propertiesOf,
  requiredOf,
  typeOf,
  variantsOf,
  type JsonSchema,
} from './schema'

// A form for a value, drawn from a JSON Schema. It covers the shapes a value in
// this store actually takes — strings, numbers, booleans, null, literals, unions
// (a union of literals is a set of choices), objects of named fields and lists —
// and falls back to a JSON box for anything else, so no value is ever uneditable.
//
// The whole form is one box, a line per value: its label on the left and its
// control on the right, a hairline between lines. An object or a list is a line
// of its own followed by its lines one step in, so depth reads as indentation
// inside the same box rather than as boxes inside boxes.
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
  // An object is its fields, with no line for itself: the box is the object.
  const fields = typeOf(schema) === 'object' && propertiesOf(schema).length > 0
  return (
    <div
      className={cn(
        'divide-y divide-gray-100 overflow-hidden rounded-md border border-gray-200 bg-white shadow-xs',
      )}
    >
      {fields ? (
        <Fields schema={schema} value={value} onChange={setValue} depth={0} />
      ) : (
        <Entry label="Value" schema={schema} value={value} onChange={setValue} depth={0} />
      )}
    </div>
  )
}

/** A text box with no surface of its own: the line is the surface. */
const BARE =
  'h-8 w-full min-w-0 bg-transparent px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 focus:outline-none'
const BARE_MONO = cn(BARE, 'font-mono text-[12.5px]')
const MUTED = 'px-2.5 font-mono text-[12.5px] leading-8 text-gray-400'

/** How far in a line's label starts, per level. */
const INDENT = 16

interface EntryProps {
  label: string
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
  depth: number
  required?: boolean
  /** Set for a list's item, which can be taken out. */
  onRemove?: () => void
}

/**
 * One line: the label, a hairline, and the control. The label's column is the
 * same width at every depth, its text pushed in by the depth, so the controls
 * line up down the whole box; pressing the label puts the caret in its control.
 * A value that doesn't fit its schema tints the line in the warning tone and
 * says why at its end — and is written all the same.
 */
function Line({
  label,
  depth,
  required,
  description,
  problem,
  tall,
  aside,
  children,
}: {
  label: string
  depth: number
  required?: boolean
  description?: string
  /** Why the value doesn't fit, if it doesn't. */
  problem?: string | null
  /** The control grows past one line, so the label sits at its top. */
  tall?: boolean
  /** Something small at the end of the line: a remove, an add. */
  aside?: React.ReactNode
  children?: React.ReactNode
}): React.JSX.Element {
  const control = useRef<HTMLDivElement>(null)
  return (
    <div
      className={cn(
        'group flex min-h-8 items-stretch',
        problem ? 'bg-warning-50/60' : 'focus-within:bg-brand-50/40',
      )}
    >
      <div
        className={cn(
          'flex w-40 shrink-0 cursor-default gap-1 truncate border-r border-gray-100 py-1.5 pr-2 text-[12px] leading-5 text-gray-500',
          tall ? 'items-start' : 'items-center',
        )}
        style={{ paddingLeft: 10 + depth * INDENT }}
        title={description}
        onClick={() => control.current?.querySelector<HTMLElement>('input, select, textarea, button')?.focus()}
      >
        <span className="truncate">{label}</span>
        {required && <span className="shrink-0 text-gray-300">*</span>}
      </div>
      <div ref={control} className={cn('flex min-w-0 flex-1', tall ? 'items-start' : 'items-center')}>
        {children}
      </div>
      {problem && (
        <div className="flex shrink-0 items-center px-2 text-[11px] text-warning-700">{problem}</div>
      )}
      {aside && <div className="flex shrink-0 items-center pr-1.5">{aside}</div>}
    </div>
  )
}

/** A small control at the end of a line, quiet until the line is hovered. */
function LineButton({
  title,
  onClick,
  children,
}: {
  title: string
  onClick: () => void
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <button
      title={title}
      aria-label={title}
      onClick={onClick}
      className="flex size-6 items-center justify-center rounded text-gray-300 group-hover:text-gray-500 hover:bg-gray-100 hover:text-gray-700 focus:outline-none focus-visible:text-gray-700"
    >
      {children}
    </button>
  )
}

const descriptionOf = (schema: JsonSchema): string | undefined =>
  typeof schema.description === 'string' ? schema.description : undefined

const titleOf = (schema: JsonSchema, fallback: string): string =>
  typeof schema.title === 'string' ? schema.title : fallback

/** An object's fields, a line (or a line and its lines) each. */
function Fields({
  schema,
  value,
  onChange,
  depth,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
  depth: number
}): React.JSX.Element {
  const required = requiredOf(schema)
  const current = isObject(value) ? value : {}
  return (
    <>
      {propertiesOf(schema).map(([key, property]) => (
        <Entry
          key={key}
          label={titleOf(property, key)}
          schema={property}
          value={current[key]}
          required={required.includes(key)}
          depth={depth}
          onChange={(next) => {
            const out = { ...current }
            // Taking a field away leaves no key, rather than an `undefined`.
            if (next === undefined) delete out[key]
            else out[key] = next
            onChange(out)
          }}
        />
      ))}
    </>
  )
}

/**
 * One value of the form, whatever its shape: a line, and for an object, a list
 * or a union of those, the lines inside it. Rendered as siblings, so every line
 * in the box takes the same hairline above it.
 */
function Entry({
  label,
  schema,
  value,
  onChange,
  depth,
  required,
  onRemove,
}: EntryProps): React.JSX.Element {
  const description = descriptionOf(schema)
  const remove = onRemove && (
    <LineButton title="Remove" onClick={onRemove}>
      <Trash03 size={13} />
    </LineButton>
  )
  const line = (
    children: React.ReactNode,
    aside: React.ReactNode = remove,
    tall = false,
  ): React.JSX.Element => (
    <Line
      tall={tall}
      label={label}
      depth={depth}
      required={required}
      description={description}
      problem={problemOf(schema, value, required)}
      aside={aside}
    >
      {children}
    </Line>
  )

  if ('const' in schema) return line(<span className={MUTED}>{JSON.stringify(schema.const)}</span>)

  const variants = variantsOf(schema)
  if (variants) {
    // Every option a literal: a choice, drawn as one.
    if (variants.every((v) => 'const' in v)) {
      return line(
        <Choice options={variants.map((v) => v.const)} value={value} onChange={onChange} />,
      )
    }
    return <Union variants={variants} value={value} onChange={onChange} line={line} depth={depth} />
  }

  switch (typeOf(schema)) {
    case 'string':
      if (schema[MULTILINE_KEYWORD] === true) {
        return line(
          <div className="w-full px-2.5 py-1.5">
            <TextEditor
              multiline
              eager
              value={typeof value === 'string' ? value : ''}
              setValue={onChange}
              className="text-[13px] leading-5 text-gray-900"
            />
          </div>,
          remove,
          true,
        )
      }
      return line(
        <input
          className={BARE}
          value={typeof value === 'string' ? value : ''}
          placeholder={typeof schema.default === 'string' ? schema.default : undefined}
          onChange={(e) => onChange(e.target.value)}
        />,
      )
    case 'number':
    case 'integer':
      return line(
        <NumberInput integer={typeOf(schema) === 'integer'} value={value} onChange={onChange} />,
      )
    case 'boolean':
      return line(
        <div className="px-2.5">
          <Switch checked={value === true} onChange={onChange} label={label} />
        </div>,
      )
    case 'null':
      return line(<span className={MUTED}>null</span>)
    case 'object':
      if (!propertiesOf(schema).length) break
      return (
        <>
          {line(<span className={MUTED}>{summary(value)}</span>)}
          <Fields schema={schema} value={value} onChange={onChange} depth={depth + 1} />
        </>
      )
    case 'array':
      return <List schema={schema} value={value} onChange={onChange} line={line} depth={depth} />
  }
  // Anything the form can't draw a control for — no type, a bag of keys — is JSON.
  return line(
    <div className="w-full py-1 pr-1.5">
      <JsonEditor value={value} setValue={onChange} placeholder="Any JSON" />
    </div>,
  )
}

/** What an object or a list holds, in a word, on its own line. */
function summary(value: unknown): string {
  if (Array.isArray(value)) return `${value.length} item${value.length === 1 ? '' : 's'}`
  if (isObject(value)) {
    const n = Object.keys(value).length
    return `${n} field${n === 1 ? '' : 's'}`
  }
  return ''
}

type LineMaker = (
  children: React.ReactNode,
  aside?: React.ReactNode,
  tall?: boolean,
) => React.JSX.Element

function List({
  schema,
  value,
  onChange,
  line,
  depth,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
  line: LineMaker
  depth: number
}): React.JSX.Element {
  const items = isObject(schema.items) ? schema.items : {}
  const list = Array.isArray(value) ? value : []
  const add = (
    <LineButton title="Add an item" onClick={() => onChange([...list, defaultFor(items)])}>
      <Plus size={13} />
    </LineButton>
  )
  return (
    <>
      {line(<span className={MUTED}>{summary(list)}</span>, add)}
      {list.map((item, i) => (
        <Entry
          key={i}
          label={String(i + 1)}
          schema={items}
          value={item}
          depth={depth + 1}
          onChange={(next) => onChange(list.map((old, j) => (j === i ? next : old)))}
          onRemove={() => onChange(list.filter((_, j) => j !== i))}
        />
      ))}
    </>
  )
}

/** How an option of a union is named in its picker. */
function variantLabel(variant: JsonSchema, i: number): string {
  if (typeof variant.title === 'string') return variant.title
  if ('const' in variant) return JSON.stringify(variant.const)
  return typeOf(variant) ?? `Option ${i + 1}`
}

/**
 * One of several schemas: a picker for which, then that schema's control — on
 * the same line where it is a single value, on lines below where it holds others.
 * The option shown is the first the value fits, so nothing about the choice is
 * stored but the value; picking another starts it from that option's default.
 */
function Union({
  variants,
  value,
  onChange,
  line,
  depth,
}: {
  variants: JsonSchema[]
  value: unknown
  onChange: (value: unknown) => void
  line: LineMaker
  depth: number
}): React.JSX.Element {
  const fitted = variants.findIndex((v) => matches(v, value))
  const at = fitted < 0 ? 0 : fitted
  const variant = variants[at]
  const picker = (
    <select
      className={cn(BARE, 'w-28 shrink-0 pr-2 text-gray-500')}
      value={String(at)}
      onChange={(e) => onChange(defaultFor(variants[Number(e.target.value)]))}
    >
      {variants.map((v, i) => (
        <option key={i} value={i}>
          {variantLabel(v, i)}
        </option>
      ))}
    </select>
  )
  const nests = typeOf(variant) === 'object' || typeOf(variant) === 'array'
  if (nests) {
    return (
      <>
        {line(picker)}
        <Entry label="" schema={variant} value={value} onChange={onChange} depth={depth + 1} />
      </>
    )
  }
  // The chosen option's own control, beside the picker: a line inside the line.
  return line(
    <>
      {picker}
      <div className="flex min-w-0 flex-1 items-center border-l border-gray-100">
        <Inline schema={variant} value={value} onChange={onChange} />
      </div>
    </>,
  )
}

/** A single value's control alone, with no line of its own — for beside a picker. */
function Inline({
  schema,
  value,
  onChange,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  if ('const' in schema) return <span className={MUTED}>{JSON.stringify(schema.const)}</span>
  switch (typeOf(schema)) {
    case 'string':
      return (
        <input
          className={BARE}
          value={typeof value === 'string' ? value : ''}
          onChange={(e) => onChange(e.target.value)}
        />
      )
    case 'number':
    case 'integer':
      return (
        <NumberInput integer={typeOf(schema) === 'integer'} value={value} onChange={onChange} />
      )
    case 'boolean':
      return (
        <div className="px-2.5">
          <Switch checked={value === true} onChange={onChange} label="Toggle" />
        </div>
      )
    case 'null':
      return <span className={MUTED}>null</span>
    default:
      return (
        <div className="w-full py-1 pr-1.5 pl-1.5">
          <JsonEditor value={value} setValue={onChange} placeholder="Any JSON" />
        </div>
      )
  }
}

function Choice({
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
    <select
      className={cn(BARE, 'pr-2')}
      value={at < 0 ? '' : String(at)}
      onChange={(e) => onChange(options[Number(e.target.value)])}
    >
      {at < 0 && <option value="">Choose…</option>}
      {options.map((o, i) => (
        <option key={i} value={i}>
          {typeof o === 'string' ? o : JSON.stringify(o)}
        </option>
      ))}
    </select>
  )
}

/**
 * A number, typed as text so a half-typed `-` or `1.` isn't thrown away. Only a
 * value that reads as a number (an integer, for an integer field) is written; a
 * line that doesn't says so in its own tone.
 */
function NumberInput({
  integer,
  value,
  onChange,
}: {
  integer: boolean
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const [text, setText] = useState(typeof value === 'number' ? String(value) : '')
  useEffect(() => {
    if (typeof value === 'number' && Number(text) !== value) setText(String(value))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [value])
  const parsed = Number(text)
  const bad =
    text.trim() !== '' && (!Number.isFinite(parsed) || (integer && !Number.isInteger(parsed)))
  return (
    <input
      className={cn(BARE_MONO, bad && 'text-error-600')}
      inputMode={integer ? 'numeric' : 'decimal'}
      title={bad ? (integer ? 'Not a whole number' : 'Not a number') : undefined}
      value={text}
      onChange={(e) => {
        const next = e.target.value
        setText(next)
        const n = Number(next)
        if (next.trim() === '') onChange(undefined)
        else if (Number.isFinite(n) && (!integer || Number.isInteger(n))) onChange(n)
      }}
    />
  )
}
