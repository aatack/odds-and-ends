import React, { useEffect, useRef, useState } from 'react'
import { ChevronDown, Pencil01, XClose } from '@untitledui/icons'
import { Modal } from '../ui/Modal'
import { TextEditor } from '../ui/TextEditor'
import { EditorsProvider, useEditorFor, type Editors } from './editors'
import { JsonEditor } from './JsonEditor'
import { AddLine, BARE, BARE_MONO, MUTED, MenuChip, Picker, cx } from './parts'
import { SchemaBuilder } from './SchemaBuilder'
import {
  KIND_NAMES,
  MONOSPACE_KEYWORD,
  MULTILINE_KEYWORD,
  POPOUT_KEYWORD,
  defaultFor,
  isObject,
  kindOf,
  matches,
  optionNames,
  problemOf,
  propertiesOf,
  requiredOf,
  variantsOf,
  type JsonSchema,
} from './schema'

// A form for a value, drawn from a JSON Schema: strings, numbers, booleans,
// null, literals, unions, objects of named fields, lists, values that are
// themselves schemas, and any JSON for the rest — so no value is uneditable.
//
// The form is lines: a label on the left, a hairline, and the value on the
// right. A value that holds others is still one line, with everything inside it
// on the right as lines of its own. Only the outermost form draws a border; the
// rest fill the space they are given.
//
// Every edit hands the whole value back through `setValue`. Keys the schema
// doesn't name are kept as they were.

export function SchemaForm({
  value,
  setValue,
  schema,
  bare = false,
  editors,
}: {
  value: unknown
  setValue: (value: unknown) => void
  schema: JsonSchema
  /** Inside another surface: the lines with no border of their own. */
  bare?: boolean
  /**
   * Custom editors, by the id a schema's `x-editor` names — added to any a form
   * or builder further out registered. See `./editors`.
   */
  editors?: Editors
}): React.JSX.Element {
  return (
    <EditorsProvider editors={editors}>
      <div
        className={cx(
          LINES,
          !bare && 'overflow-hidden rounded-md border border-gray-200 bg-white shadow-xs',
        )}
      >
        {/* An object is its fields, a labelled line each; a lone value has nothing
          to tell it apart from, so no label. */}
        {hasFields(schema) ? (
          <Fields schema={schema} value={value} onChange={setValue} />
        ) : (
          <Entry schema={schema} value={value} onChange={setValue} />
        )}
      </div>
    </EditorsProvider>
  )
}

/**
 * A run of sibling lines: a grid of the labels' column and the values', each
 * line a row of it (a subgrid), so the hairline between them sits just past the
 * longest label among the siblings.
 */
const LINES = 'grid w-full grid-cols-[max-content_minmax(0,1fr)] divide-y divide-gray-100'

/** Something in a run of lines that is not a line, across both columns. */
const SPAN = 'col-span-2'

const hasFields = (schema: JsonSchema): boolean =>
  kindOf(schema) === 'object' && propertiesOf(schema).length > 0

const isLiteral = (schema: JsonSchema): boolean => 'const' in schema

/** Whether a schema's value takes more than one line, when drawn in place. */
function compound(schema: JsonSchema): boolean {
  switch (kindOf(schema)) {
    case 'schema':
    case 'array':
      return true
    case 'string':
      return schema[MULTILINE_KEYWORD] === true
    case 'union':
      return !(variantsOf(schema) ?? []).every(isLiteral)
    case 'object':
      return hasFields(schema)
    default:
      return false
  }
}

/**
 * One line: the label, a hairline, and the value — or the value alone across the
 * line, where there is no label. Pressing the label puts the caret in its
 * control. A value that doesn't fit its schema tints the line and says why at
 * its end, and is written all the same.
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
      className={cx(
        'group/line col-span-2 grid min-h-8 grid-cols-subgrid',
        problem ? 'bg-warning-50/60' : !tall && 'focus-within:bg-brand-50/40',
      )}
    >
      {label !== undefined && (
        <div
          className={cx(
            'flex max-w-56 min-w-0 cursor-default gap-1 border-r border-gray-100 px-2.5 py-1.5 text-[12px] leading-5 text-gray-500',
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
      <div className={cx('flex min-w-0', label === undefined && SPAN)}>
        <div
          ref={control}
          className={cx('flex min-w-0 flex-1', tall ? 'items-stretch' : 'items-center')}
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
          label={typeof property.title === 'string' ? property.title : key}
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

/** One value: its line, and its control — in place, or behind a pencil. */
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
  onRemove?: () => void
}): React.JSX.Element {
  const popped = schema[POPOUT_KEYWORD] === true
  const editor = useEditorFor(schema)
  // A custom editor says for itself whether it takes more than one line.
  const tall = popped ? false : editor ? editor.tall === true : compound(schema)
  return (
    <Line
      label={label}
      required={required}
      description={typeof schema.description === 'string' ? schema.description : undefined}
      problem={problemOf(schema, value, required)}
      tall={tall}
      onRemove={onRemove}
    >
      {popped ? (
        <PopOut schema={schema} value={value} onChange={onChange} label={label} />
      ) : (
        <Control schema={schema} value={value} onChange={onChange} label={label} />
      )}
    </Line>
  )
}

/** What a popped-out value holds, in a few words, for the line that opens it. */
function summaryOf(schema: JsonSchema, value: unknown): string {
  if (value === undefined) return 'Empty'
  if (kindOf(schema) === 'schema') {
    return isObject(value) ? `${KIND_NAMES[kindOf(value)]} schema` : 'Schema'
  }
  if (Array.isArray(value)) return `${value.length} item${value.length === 1 ? '' : 's'}`
  if (isObject(value)) {
    const keys = Object.keys(value)
    if (!keys.length) return 'No fields'
    const shown = keys.slice(0, 3).join(', ')
    return keys.length > 3 ? `${shown} and ${keys.length - 3} more` : shown
  }
  const text = JSON.stringify(value) ?? ''
  return text.length > 60 ? `${text.slice(0, 60)}…` : text
}

/**
 * A value edited in a dialog: one line saying what it holds, which opens the
 * whole form for it. The dialog edits the value in place, so closing it is all
 * there is.
 */
function PopOut({
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
  const [open, setOpen] = useState(false)
  // Without the keyword, so the dialog draws it in place.
  const { [POPOUT_KEYWORD]: _, ...inner } = schema
  return (
    <>
      <button
        className="flex h-8 w-full items-center gap-1.5 px-2.5 text-left text-[13px] focus:outline-none focus-visible:bg-brand-50/40"
        onClick={() => setOpen(true)}
      >
        <span className="min-w-0 flex-1 truncate text-gray-500">{summaryOf(schema, value)}</span>
        <Pencil01 size={12} className="shrink-0 text-gray-400" />
      </button>
      {open && (
        <Modal
          title={label ?? (typeof schema.title === 'string' ? schema.title : 'Edit')}
          size="wide"
          onClose={() => setOpen(false)}
        >
          <SchemaForm schema={inner} value={value} setValue={onChange} />
        </Modal>
      )}
    </>
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
  // A custom editor the schema names comes first: it is the host's say over the
  // form's. Inside a pop-out's dialog this is where it is drawn too, since the
  // dialog's form draws the same schema.
  const editor = useEditorFor(schema)
  if (editor) return <>{editor.render({ schema, value, onChange, label })}</>

  const mono = schema[MONOSPACE_KEYWORD] === true
  switch (kindOf(schema)) {
    case 'literal':
      return <span className={MUTED}>{JSON.stringify(schema.const)}</span>
    case 'null':
      return <span className={MUTED}>null</span>
    case 'schema':
      return <SchemaBuilder value={value} setValue={onChange} bare />
    case 'union':
      return (
        <Union
          variants={variantsOf(schema) ?? []}
          value={value}
          onChange={onChange}
          label={label}
        />
      )
    case 'string':
      if (schema[MULTILINE_KEYWORD] === true) {
        return (
          <div className="w-full px-2.5 py-1.5">
            <TextEditor
              multiline
              eager
              value={typeof value === 'string' ? value : ''}
              setValue={onChange}
              // `block`: an inline textarea keeps room under its last line for
              // descenders, which reads as padding nobody asked for.
              className={cx(
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
        <NumberInput integer={kindOf(schema) === 'integer'} value={value} onChange={onChange} />
      )
    case 'boolean':
      // Yes or no, written as a word: the whole line is the button that flips it.
      return (
        <button
          className={cx(BARE, 'text-left', value === true ? 'text-gray-900' : 'text-gray-500')}
          aria-pressed={value === true}
          onClick={() => onChange(value !== true)}
        >
          {value === true ? 'Yes' : 'No'}
        </button>
      )
    case 'array':
      return <List schema={schema} value={value} onChange={onChange} />
    case 'object':
      if (hasFields(schema)) {
        return (
          <div className={LINES}>
            <Fields schema={schema} value={value} onChange={onChange} />
          </div>
        )
      }
  }
  // Anything else — no type, an object that names no fields — is JSON.
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
    <div className={LINES}>
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
      <AddLine className={SPAN} onClick={() => onChange([...list, defaultFor(items)])}>
        Add
      </AddLine>
    </div>
  )
}

/**
 * One of several schemas, and the picker for which. The option shown is the
 * first the value fits, so nothing about the choice is stored but the value;
 * picking another starts it from that option's default. Where the picker sits
 * depends on what the option needs besides being picked:
 *
 *   nothing (a literal, null)   the picker is the whole line — its name is the value
 *   one line (text, a number)   the option's control fills the line, the picker a
 *                               chip at its right end, as in the schema builder
 *   more (an object, a list)    the picker is a line, and the option's lines follow
 *
 * A union of literals alone is a choice: a value that fits none is shown as no
 * choice made, rather than as the first.
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
  const at = fitted >= 0 || variants.every(isLiteral) ? fitted : 0
  const names = optionNames(variants)
  const pick = (i: number): void => onChange(defaultFor(variants[i]))
  const variant = variants[at]

  if (!variant || isLiteral(variant) || kindOf(variant) === 'null') {
    return <RowPicker names={names} at={at} onPick={pick} />
  }
  const control = <Control schema={variant} value={value} onChange={onChange} label={label} />
  if (compound(variant)) {
    return (
      <div className={LINES}>
        <div className={SPAN}>
          <RowPicker names={names} at={at} onPick={pick} />
        </div>
        <div className={cx(SPAN, 'flex min-w-0')}>{control}</div>
      </div>
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
              className={cx('min-w-0 flex-1 truncate', at < 0 ? 'text-gray-400' : 'text-gray-900')}
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

/**
 * A number, typed as text so a half-typed `-` or `1.` isn't thrown away. Only a
 * value that reads as a number (an integer, for an integer field) is written.
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
      className={cx(BARE_MONO, bad && 'text-error-600')}
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
