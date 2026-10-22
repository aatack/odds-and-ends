import React, { useEffect, useRef, useState } from 'react'
import { ChevronDown, Plus, XClose } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { Dropdown } from '../ui/Dropdown'
import { Switch } from '../ui/Switch'
import { TextEditor } from '../ui/TextEditor'
import { JsonEditor } from './JsonEditor'
import { MenuChip, MenuItem } from './Menu'
import { SchemaBuilder } from './SchemaBuilder'
import {
  MONOSPACE_KEYWORD,
  MULTILINE_KEYWORD,
  defaultFor,
  isObject,
  isSchemaRef,
  matches,
  optionNames,
  problemOf,
  propertiesOf,
  requiredOf,
  typeOf,
  variantsOf,
  type JsonSchema,
} from './schema'

// A form for a value, drawn from a JSON Schema. It covers the shapes a value in
// this store actually takes — strings, numbers, booleans, null, literals, unions
// (a union of literals is a set of choices), objects of named fields, lists, and
// values that are themselves schemas — and falls back to a JSON box for anything
// else, so no value is ever uneditable.
//
// The whole form is one box of lines: a label on the left, a hairline, and the
// value on the right. A value that holds others — an object, a list, a schema —
// is still one line: its label on the left, and on the right everything inside
// it, as lines of their own. Depth is where the content sits, not a header above
// it.
//
// Every edit hands the whole value back through `setValue`. Keys the schema
// doesn't name are kept as they were: the form edits what it knows about and
// leaves the rest alone.

export function SchemaForm({
  value,
  setValue,
  schema,
  bare = false,
}: {
  value: unknown
  setValue: (value: unknown) => void
  schema: JsonSchema
  /**
   * Inside another surface rather than on its own: the lines with no box around
   * them, filling the space they are given. The schema builder draws its
   * options this way.
   */
  bare?: boolean
}): React.JSX.Element {
  // An object is its fields, with no line for itself: the box is the object.
  const fields = typeOf(schema) === 'object' && propertiesOf(schema).length > 0
  return (
    <div
      className={cn(
        LINES,
        !bare && 'overflow-hidden rounded-md border border-gray-200 bg-white shadow-xs',
      )}
    >
      {fields ? (
        <Fields schema={schema} value={value} onChange={setValue} />
      ) : (
        // A lone value has nothing to tell it apart from, so it has no label.
        <Entry schema={schema} value={value} onChange={setValue} />
      )}
    </div>
  )
}

/** A text box with no surface of its own: the line is the surface. */
const BARE =
  'h-8 w-full min-w-0 bg-transparent px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 focus:outline-none'
const BARE_MONO = cn(BARE, 'font-mono text-[12.5px]')
const MUTED = 'px-2.5 font-mono text-[12.5px] leading-8 text-gray-400'

/**
 * A run of sibling lines: a grid of two columns, the labels' and the values',
 * with each line a row of it (a subgrid). So the hairline between label and
 * value sits just right of the longest label among the siblings, rather than at
 * a width chosen for all of them — and each group finds its own.
 */
const LINES = 'grid w-full grid-cols-[max-content_minmax(0,1fr)] divide-y divide-gray-100'

/** Something in a run of lines that is not a line, across both columns. */
const SPAN = 'col-span-2'

/** Lines inside a line: the content of an object, a list or a union's option. */
function Inner({ children }: { children: React.ReactNode }): React.JSX.Element {
  return <div className={LINES}>{children}</div>
}

/**
 * One line: the label, a hairline, and the value — or the value alone, across the
 * whole line, where there is no label. Pressing the label puts the caret in its
 * control. A value that doesn't fit its schema tints the line in the
 * warning tone and says why at its end — and is written all the same.
 */
function Line({
  label,
  required,
  description,
  problem,
  tall,
  onRemove,
  children,
}: {
  label?: string
  required?: boolean
  description?: string
  /** Why the value doesn't fit, if it doesn't. */
  problem?: string | null
  /** The value takes more than one line, so the label sits at its top. */
  tall?: boolean
  /** Set for a list's item: a small cross beside its number takes it out. */
  onRemove?: () => void
  children?: React.ReactNode
}): React.JSX.Element {
  const control = useRef<HTMLDivElement>(null)
  return (
    <div
      className={cn(
        'group/line col-span-2 grid min-h-8 grid-cols-subgrid',
        problem ? 'bg-warning-50/60' : !tall && 'focus-within:bg-brand-50/40',
      )}
    >
      {label !== undefined && (
        <div
          className={cn(
            'flex max-w-56 min-w-0 cursor-default gap-1 border-r border-gray-100 py-1.5 pr-2.5 pl-2.5 text-[12px] leading-5 text-gray-500',
            tall ? 'items-start' : 'items-center',
          )}
          title={description}
          onClick={() =>
            control.current?.querySelector<HTMLElement>('input, select, textarea, button')?.focus()
          }
        >
          <span className="truncate">{label}</span>
          {required && <span className="shrink-0 text-gray-300">*</span>}
          {onRemove && (
            <button
              title="Remove"
              aria-label="Remove"
              className="-mr-1 flex h-5 w-4 shrink-0 items-center justify-center text-gray-300 group-hover/line:text-gray-400 hover:text-gray-700 focus:outline-none focus-visible:text-gray-700"
              onClick={(e) => {
                e.stopPropagation()
                onRemove()
              }}
            >
              <XClose size={12} />
            </button>
          )}
        </div>
      )}
      <div className={cn('flex min-w-0', label === undefined && SPAN)}>
        <div
          ref={control}
          className={cn('flex min-w-0 flex-1', tall ? 'items-stretch' : 'items-center')}
        >
          {children}
        </div>
        {problem && (
          <div className="flex shrink-0 items-start px-2 py-1.5 text-[11px] leading-5 text-warning-700">
            {problem}
          </div>
        )}
      </div>
    </div>
  )
}

const descriptionOf = (schema: JsonSchema): string | undefined =>
  typeof schema.description === 'string' ? schema.description : undefined

const titleOf = (schema: JsonSchema, fallback: string): string =>
  typeof schema.title === 'string' ? schema.title : fallback

/** Whether a schema's value takes more than one line. */
const compound = (schema: JsonSchema): boolean => {
  if (isSchemaRef(schema)) return true
  if (schema[MULTILINE_KEYWORD] === true) return true
  const variants = variantsOf(schema)
  if (variants) return !variants.every((v) => 'const' in v)
  const type = typeOf(schema)
  return type === 'array' || (type === 'object' && propertiesOf(schema).length > 0)
}

/** An object's fields, a line each. */
function Fields({
  schema,
  value,
  onChange,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
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

/** One value of the form: its line, with its control — however much is inside it. */
function Entry({
  label,
  schema,
  value,
  onChange,
  required,
  onRemove,
}: {
  label?: string
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
  required?: boolean
  /** Set for a list's item, which can be taken out. */
  onRemove?: () => void
}): React.JSX.Element {
  return (
    <Line
      label={label}
      required={required}
      description={descriptionOf(schema)}
      problem={problemOf(schema, value, required)}
      tall={compound(schema)}
      onRemove={onRemove}
    >
      <Control schema={schema} value={value} onChange={onChange} label={label} />
    </Line>
  )
}

/** A value's control alone: what goes on the right of its line. */
function Control({
  schema,
  value,
  onChange,
  label,
}: {
  schema: JsonSchema
  value: unknown
  onChange: (value: unknown) => void
  label?: string
}): React.JSX.Element {
  if ('const' in schema) return <span className={MUTED}>{JSON.stringify(schema.const)}</span>

  // A value that is itself a schema, written with the schema builder.
  if (isSchemaRef(schema)) {
    return <SchemaBuilder value={value} setValue={onChange} bare />
  }

  const variants = variantsOf(schema)
  if (variants) {
    // Every option a literal: a choice, drawn as one.
    if (variants.every((v) => 'const' in v)) {
      return <Choice variants={variants} value={value} onChange={onChange} />
    }
    return <Union variants={variants} value={value} onChange={onChange} label={label} />
  }

  const mono = schema[MONOSPACE_KEYWORD] === true
  switch (typeOf(schema)) {
    case 'string':
      if (schema[MULTILINE_KEYWORD] === true) {
        return (
          <div className="w-full px-2.5 py-1.5">
            <TextEditor
              multiline
              eager
              value={typeof value === 'string' ? value : ''}
              setValue={onChange}
              className={cn(
                // `block`: an inline textarea keeps room under its last line
                // for descenders, which reads as padding nobody asked for.
                'block leading-5 text-gray-900',
                mono ? 'font-mono text-[12.5px]' : 'text-[13px]',
              )}
            />
          </div>
        )
      }
      return (
        <input
          className={mono ? BARE_MONO : BARE}
          value={typeof value === 'string' ? value : ''}
          placeholder={typeof schema.default === 'string' ? schema.default : undefined}
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
          <Switch checked={value === true} onChange={onChange} label={label ?? 'Toggle'} />
        </div>
      )
    case 'null':
      return <span className={MUTED}>null</span>
    case 'object':
      if (!propertiesOf(schema).length) break
      return (
        <Inner>
          <Fields schema={schema} value={value} onChange={onChange} />
        </Inner>
      )
    case 'array':
      return <List schema={schema} value={value} onChange={onChange} />
  }
  // Anything the form can't draw a control for — no type, a bag of keys — is JSON.
  return (
    <div className="w-full p-1">
      <JsonEditor value={value} setValue={onChange} placeholder="Any JSON" />
    </div>
  )
}

/** A list: a line per item, numbered, and a last line for adding one. */
function List({
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
  return (
    <Inner>
      {list.map((item, i) => (
        <Entry
          key={i}
          label={String(i + 1)}
          schema={items}
          value={item}
          onChange={(next) => onChange(list.map((old, j) => (j === i ? next : old)))}
          onRemove={() => onChange(list.filter((_, j) => j !== i))}
        />
      ))}
      <button
        className="col-span-2 flex h-8 w-full items-center gap-1.5 px-2.5 text-[12px] text-gray-400 hover:bg-gray-50 hover:text-gray-700 focus:outline-none focus-visible:bg-gray-50"
        onClick={() => onChange([...list, defaultFor(items)])}
      >
        <Plus size={13} /> Add
      </button>
    </Inner>
  )
}

/**
 * One of several schemas, and the picker for which. The option shown is the
 * first the value fits, so nothing about the choice is stored but the value;
 * picking another starts it from that option's default. How the picker sits
 * depends on what the option needs besides being picked:
 *
 *   nothing (a literal, null)   the picker is the whole line — its name is the value
 *   one line (text, a number)   the option's control fills the line, the picker a
 *                               chip at its right end, as in the schema builder
 *   more (an object, a list)    the picker is a line, and the option's lines follow
 */
function Union({
  variants,
  value,
  onChange,
  label,
}: {
  variants: JsonSchema[]
  value: unknown
  onChange: (value: unknown) => void
  label?: string
}): React.JSX.Element {
  const fitted = variants.findIndex((v) => matches(v, value))
  const at = fitted < 0 ? 0 : fitted
  const variant = variants[at]
  const names = optionNames(variants)
  const pick = (i: number): void => onChange(defaultFor(variants[i]))

  if ('const' in variant || typeOf(variant) === 'null') {
    return <RowPicker names={names} at={at} onPick={pick} />
  }
  const control = <Control schema={variant} value={value} onChange={onChange} label={label} />
  if (compound(variant)) {
    return (
      <Inner>
        <div className={SPAN}>
          <RowPicker names={names} at={at} onPick={pick} />
        </div>
        <div className={cn(SPAN, 'flex min-w-0')}>{control}</div>
      </Inner>
    )
  }
  return (
    <div className="flex w-full min-w-0 items-center">
      <div className="flex min-w-0 flex-1 items-center">{control}</div>
      <div className="flex max-w-48 shrink-0 items-center pr-1">
        <Picker
          names={names}
          at={at}
          onPick={pick}
          trigger={(toggle) => <MenuChip onClick={toggle}>{names[at]}</MenuChip>}
        />
      </div>
    </div>
  )
}

/** A picker that is the whole line: the chosen name, and a chevron at the end. */
function RowPicker({
  names,
  at,
  onPick,
}: {
  names: string[]
  /** The option chosen now, or -1 for none. */
  at: number
  onPick: (index: number) => void
}): React.JSX.Element {
  return (
    <div className="w-full">
      <Picker
        names={names}
        at={at}
        onPick={onPick}
        trigger={(toggle) => (
          <button
            className="flex h-8 w-full items-center gap-1 px-2.5 text-left text-[13px] focus:outline-none"
            onClick={toggle}
          >
            <span
              className={cn('min-w-0 flex-1 truncate', at < 0 ? 'text-gray-400' : 'text-gray-900')}
            >
              {at < 0 ? 'Choose…' : names[at]}
            </span>
            <ChevronDown size={12} className="shrink-0 text-gray-400" />
          </button>
        )}
      />
    </div>
  )
}

/** A menu of named options, the same one the schema builder uses. */
function Picker({
  names,
  at,
  onPick,
  trigger,
}: {
  names: string[]
  /** The option chosen now, or -1 for none. */
  at: number
  onPick: (index: number) => void
  trigger: (toggle: () => void) => React.ReactNode
}): React.JSX.Element {
  return (
    <Dropdown trigger={({ toggle }) => trigger(toggle)}>
      {(close) =>
        names.map((name, i) => (
          <MenuItem
            key={i}
            on={i === at}
            onClick={() => {
              if (i !== at) onPick(i)
              close()
            }}
          >
            {name}
          </MenuItem>
        ))
      }
    </Dropdown>
  )
}

/** A union of literals: the line shows the one chosen, and opens the menu of all. */
function Choice({
  variants,
  value,
  onChange,
}: {
  variants: JsonSchema[]
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const at = variants.findIndex((v) => JSON.stringify(v.const) === JSON.stringify(value))
  return (
    <RowPicker names={optionNames(variants)} at={at} onPick={(i) => onChange(variants[i].const)} />
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
