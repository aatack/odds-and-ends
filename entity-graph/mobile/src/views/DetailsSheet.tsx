import React, { useState } from 'react'
import { Button } from '../components/ui/Button'
import { Field, Input, Select } from '../components/ui/Field'
import { Sheet } from '../components/ui/Sheet'
import { TextEditor } from '../components/ui/TextEditor'
import { cn } from '../helpers/cn'
import { useEntityLabel, useGetEntities } from '../state/hooks'
import { closeSheet } from '../state/ui'
import { dispatch } from '../tools/dispatch'
import { TEXT } from './types'

// An entity's values as a form, drawn from its type's `schema`. The phone's small
// answer to the desktop's detail view (`../../src/renderer/src/layout/DetailView.tsx`)
// and schema form — read for behaviour, not ported: this is a handful of widgets, a
// read-only fallback for everything it can't edit on a phone, and one undoable save.
//
// Writing goes through a tool like everything else: the sheet works out which keys
// the draft changed and dispatches `entity.details.save` with just those, so one
// undo takes the whole save back.

type JsonSchema = Record<string, unknown>

const isObject = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v)

/** An entity of no type, or of a type with no schema, is edited as just its text. */
const TEXT_ONLY: JsonSchema = {
  type: 'object',
  properties: { text: { type: 'string', 'x-multiline': true } },
}

/** A schema worth drawing a form from: an object that names at least one property. */
const hasProperties = (schema: unknown): schema is JsonSchema =>
  isObject(schema) && isObject(schema.properties) && Object.keys(schema.properties).length > 0

// The widgets this phone form can edit. Anything else — an object, a list, a `$ref`,
// a union, a field with no `type` — is shown as read-only JSON, left to the desktop.
type Widget = 'string' | 'multiline' | 'number' | 'boolean' | 'enum' | 'json'

function widgetOf(schema: Record<string, unknown>): Widget {
  // The `enum` picker writes strings, so it is only right for an all-string enum;
  // one of numbers (or mixed) is left read-only rather than silently stringified.
  if (Array.isArray(schema.enum)) {
    return schema.enum.every((option) => typeof option === 'string') ? 'enum' : 'json'
  }
  const type = schema.type
  if (type === 'boolean') return 'boolean'
  if (type === 'number' || type === 'integer') return 'number'
  if (type === 'string') return schema['x-multiline'] === true ? 'multiline' : 'string'
  return 'json'
}

/** A stored value as the control for its widget wants it: a word for a number, a flag. */
function shownOf(w: Widget, raw: unknown): unknown {
  if (w === 'boolean') return raw === true
  if (w === 'number') return typeof raw === 'number' ? String(raw) : ''
  return typeof raw === 'string' ? raw : ''
}

/** A control's value as it is written: a number parsed, a blank cleared to null. */
function writtenOf(w: Widget, shown: unknown): unknown | undefined {
  if (w === 'boolean') return shown === true
  if (w === 'number') {
    const text = String(shown ?? '').trim()
    if (text === '') return null
    const n = Number(text)
    return Number.isNaN(n) ? undefined : n // undefined: leave a half-typed number be
  }
  const text = String(shown ?? '')
  return text === '' ? null : text
}

const BOX = 'rounded-xl bg-gray-100 px-3.5 py-3 text-gray-900'

export function DetailsSheet({ entityId }: { entityId: string }): React.JSX.Element {
  const get = useGetEntities()
  const label = useEntityLabel(entityId)
  const values = get([entityId])[entityId].values

  const typeId = typeof values.type === 'string' ? values.type : null
  const typeValues = typeId ? get([typeId])[typeId].values : null
  const typeName = typeId ? (typeof typeValues?.text === 'string' ? typeValues.text : typeId) : null
  const rawSchema = typeValues ? typeValues.schema : undefined
  const schema = hasProperties(rawSchema) ? rawSchema : TEXT_ONLY

  const properties = schema.properties as Record<string, Record<string, unknown>>
  const keys = Object.keys(properties)
  const required = Array.isArray(schema.required) ? (schema.required as string[]) : []

  // Only the keys the user touches, in control form — so the save is exactly what
  // changed, and an untouched entity is written back over by nothing.
  const [draft, setDraft] = useState<Record<string, unknown>>({})
  // A per-field problem, shown under the control: a number that won't parse keeps the
  // sheet open rather than being dropped on the floor.
  const [problems, setProblems] = useState<Record<string, string>>({})

  const numberProblem = (field: Record<string, unknown>, raw: unknown): string | null => {
    const text = String(raw ?? '').trim()
    if (text === '') return null
    const n = Number(text)
    const integer = field.type === 'integer'
    if (!Number.isFinite(n)) return 'Not a number'
    if (integer && !Number.isInteger(n)) return 'Not a whole number'
    return null
  }

  const save = (): void => {
    const changes: Record<string, unknown> = {}
    const found: Record<string, string> = {}
    for (const key of keys) {
      if (!(key in draft)) continue
      const field = properties[key]
      const w = widgetOf(field)
      if (w === 'number') {
        const problem = numberProblem(field, draft[key])
        if (problem) {
          found[key] = problem
          continue
        }
      }
      const next = writtenOf(w, draft[key])
      if (next === undefined) continue
      if (!Object.is(next, values[key] ?? null)) changes[key] = next
    }
    // A bad field blocks the whole save and writes nothing — the sheet stays up with
    // the problem on it, so no other edit is lost to the one that didn't parse.
    if (Object.keys(found).length) {
      setProblems(found)
      return
    }
    setProblems({})
    closeSheet()
    if (Object.keys(changes).length) dispatch('entity.details.save', { entityId, values: changes })
  }

  const formKeys = new Set(keys)
  const others = Object.keys(values)
    .filter((key) => !formKeys.has(key))
    .sort()

  return (
    <Sheet
      title={label}
      onClose={closeSheet}
      footer={
        <Button tone="primary" block onClick={save}>
          Save
        </Button>
      }
    >
      {typeName && <p className="pb-2 text-[12px] text-gray-400">{typeName}</p>}
      <div className="flex flex-col gap-3 pb-2">
        {keys.map((key) => {
          const field = properties[key]
          const w = widgetOf(field)
          const title = typeof field.title === 'string' ? field.title : key
          const description = typeof field.description === 'string' ? field.description : undefined
          const labelText = required.includes(key) ? `${title} *` : title
          const shown = key in draft ? draft[key] : shownOf(w, values[key])
          const problem = problems[key]
          // `text` is the entity's own words, so its editor reads as a row does — Lora,
          // not the chrome's Geist. Everything else stays in Geist.
          const own = key === 'text'
          const set = (next: unknown): void => {
            setDraft((d) => ({ ...d, [key]: next }))
            if (problem)
              setProblems((p) => {
                const rest = { ...p }
                delete rest[key]
                return rest
              })
          }
          const control =
            w === 'boolean' ? (
              // Yes or no as a word, the whole line the button that flips it.
              <button
                type="button"
                aria-pressed={shown === true}
                onClick={() => set(shown !== true)}
                className={cn(
                  'min-h-11 w-full rounded-xl bg-gray-100 px-3.5 text-left text-[16px] active:bg-gray-200',
                  shown === true ? 'text-gray-900' : 'text-gray-500',
                )}
              >
                {shown === true ? 'Yes' : 'No'}
              </button>
            ) : w === 'enum' ? (
              <Select value={typeof shown === 'string' ? shown : ''} onChange={(e) => set(e.target.value)}>
                <option value="">—</option>
                {(field.enum as unknown[]).map((option) => (
                  <option key={String(option)} value={String(option)}>
                    {String(option)}
                  </option>
                ))}
              </Select>
            ) : w === 'multiline' ? (
              <div className={BOX}>
                <TextEditor
                  value={String(shown ?? '')}
                  setValue={set}
                  className={own ? TEXT : cn('block', field['x-monospace'] === true && 'font-mono')}
                />
              </div>
            ) : w === 'number' ? (
              <Input
                value={String(shown ?? '')}
                inputMode={field.type === 'integer' ? 'numeric' : 'decimal'}
                onChange={(e) => set(e.target.value)}
              />
            ) : w === 'string' ? (
              <Input
                value={String(shown ?? '')}
                className={own ? 'font-serif' : undefined}
                onChange={(e) => set(e.target.value)}
              />
            ) : (
              // Not something a phone edits: shown whole, read-only, for the desktop.
              <div className="flex flex-col gap-1">
                <pre className="overflow-x-auto rounded-xl bg-gray-100 px-3.5 py-3 font-mono text-[13px] text-gray-800">
                  {JSON.stringify(values[key] ?? null, null, 2)}
                </pre>
                <span className="text-[12px] text-gray-400">Edited on the desktop app.</span>
              </div>
            )
          return (
            <Field key={key} label={labelText} hint={description}>
              {control}
              {problem && <span className="text-[13px] text-error-600">{problem}</span>}
            </Field>
          )
        })}

        {others.length > 0 && (
          <div className="pt-1">
            <h3 className="pb-1.5 text-[11px] font-medium tracking-wide text-gray-400 uppercase">
              Other values
            </h3>
            <div className="flex flex-col gap-1.5">
              {others.map((key) => (
                <div key={key} className="flex gap-2 text-[13px]">
                  <span className="shrink-0 font-medium text-gray-500">{key}</span>
                  <span className="min-w-0 flex-1 truncate font-mono text-gray-700">
                    {JSON.stringify(values[key])}
                  </span>
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </Sheet>
  )
}
