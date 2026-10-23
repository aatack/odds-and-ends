import { useLayoutEffect, useRef, useState, type ReactNode, type TextareaHTMLAttributes } from 'react'
import type { ObjectSchema, PropertySchema } from '../../shared/types'

/** Fields that take prose, and so a text box rather than a line. */
const LONG = new Set(['needed', 'instructions', 'message', 'text', 'body', 'source'])

const typeOf = (p: PropertySchema) => (Array.isArray(p.type) ? p.type.find((t) => t !== 'null') : p.type)

/**
 * A condensed form over a flat object its JSON Schema describes: one control a
 * field, named by its placeholder, with the description as its tooltip. Anything
 * deeper is edited as JSON. For the forms used all day (a new item, a handoff);
 * the schema components are for the rest.
 */
export function CompactForm(props: {
  schema: ObjectSchema
  value: Record<string, unknown>
  onChange: (value: Record<string, unknown>) => void
  /** Shown in place of `hinted`'s placeholder while it is empty; that field comes first. */
  hint?: { field: string; content: ReactNode }
}) {
  const { schema, value, onChange, hint } = props
  const fields = Object.entries(schema.properties ?? {})
  if (hint) fields.sort(([a], [b]) => Number(b === hint.field) - Number(a === hint.field))
  return (
    <div className="compact">
      {fields.map(([name, p]) => {
        const label = `${name}${schema.required?.includes(name) ? '' : ' (optional)'}`
        const field = (
          <Field
            key={name}
            name={name}
            label={label}
            placeholder={hint?.field === name ? '' : undefined}
            schema={p}
            value={value[name]}
            onChange={(v) => onChange({ ...value, [name]: v })}
          />
        )
        if (hint?.field !== name) return field
        const empty = value[name] === undefined || value[name] === '' || (Array.isArray(value[name]) && !(value[name] as unknown[]).length)
        return (
          <div key={name} className="hinted">
            {field}
            {empty && <span className="hint">{hint.content}</span>}
          </div>
        )
      })}
    </div>
  )
}

function Field(props: {
  name: string
  label: string
  /** In place of the label as placeholder; '' for none. */
  placeholder?: string
  schema: PropertySchema
  value: unknown
  onChange: (v: unknown) => void
}) {
  const { name, schema, value, onChange } = props
  const label = props.placeholder ?? props.label
  const type = typeOf(schema)
  const named = { title: schema.description ? `${props.label}: ${schema.description}` : props.label, 'aria-label': props.label }
  if (schema.enum)
    return (
      <select {...named} value={value === undefined ? '' : String(value)} onChange={(e) => onChange(e.target.value || undefined)}>
        <option value="">{label}</option>
        {schema.enum.map((o) => (
          <option key={o}>{o}</option>
        ))}
      </select>
    )
  if (type === 'boolean')
    return (
      <label className="check" title={named.title}>
        <input type="checkbox" checked={Boolean(value)} onChange={(e) => onChange(e.target.checked || undefined)} /> {label}
      </label>
    )
  if (type === 'number' || type === 'integer')
    return (
      <input
        {...named}
        placeholder={label}
        inputMode="numeric"
        value={value === undefined ? '' : String(value)}
        onChange={(e) => onChange(e.target.value === '' ? undefined : Number(e.target.value))}
      />
    )
  if (type === 'array' && typeOf(schema.items ?? {}) === 'string')
    return (
      <input
        {...named}
        placeholder={`${label}, comma separated`}
        value={Array.isArray(value) ? value.join(', ') : ''}
        onChange={(e) => onChange(e.target.value.split(',').map((s) => s.trim()).filter(Boolean))}
      />
    )
  if (type === 'string' || type === undefined) {
    const text = value === undefined ? '' : String(value)
    const set = (s: string) => onChange(s === '' ? undefined : s)
    return LONG.has(name) ? (
      <GrowingTextarea
        {...named}
        placeholder={label}
        className={name === 'source' ? 'mono' : ''}
        value={text}
        onChange={(e) => set(e.target.value)}
      />
    ) : (
      <input {...named} placeholder={label} value={text} onChange={(e) => set(e.target.value)} />
    )
  }
  return <JsonField label={label} title={named.title} value={value} onChange={onChange} />
}

function JsonField({ label, title, value, onChange }: { label: string; title: string; value: unknown; onChange: (v: unknown) => void }) {
  const [draft, setDraft] = useState(value === undefined ? '' : JSON.stringify(value, null, 2))
  const [bad, setBad] = useState(false)
  return (
    <textarea
      className={`mono ${bad ? 'bad' : ''}`}
      rows={2}
      title={title}
      value={draft}
      placeholder={`${label} (JSON)`}
      onChange={(e) => {
        setDraft(e.target.value)
        if (!e.target.value.trim()) return (setBad(false), onChange(undefined))
        try {
          onChange(JSON.parse(e.target.value))
          setBad(false)
        } catch {
          setBad(true)
        }
      }}
    />
  )
}

/** Most lines a text box grows to before it scrolls. */
const MAX_LINES = 8

/** A text box one line tall that grows with its text, up to MAX_LINES, with no handle to drag. */
export function GrowingTextarea(props: TextareaHTMLAttributes<HTMLTextAreaElement>) {
  const ref = useRef<HTMLTextAreaElement>(null)
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    const line = parseFloat(getComputedStyle(el).lineHeight) || 20
    const pad = el.offsetHeight - el.clientHeight + parseFloat(getComputedStyle(el).paddingTop) + parseFloat(getComputedStyle(el).paddingBottom)
    el.style.height = 'auto'
    el.style.height = `${Math.min(el.scrollHeight, line * MAX_LINES + pad)}px`
  }, [props.value])
  return <textarea {...props} ref={ref} rows={1} className={`growing ${props.className ?? ''}`} />
}
