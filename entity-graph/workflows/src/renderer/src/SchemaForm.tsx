import { useState } from 'react'
import type { ObjectSchema, PropertySchema } from '../../shared/types'

/** Fields that take prose, and so a text box rather than a line. */
const LONG = new Set(['needed', 'instructions', 'message', 'text', 'body', 'source'])

const typeOf = (p: PropertySchema) => (Array.isArray(p.type) ? p.type.find((t) => t !== 'null') : p.type)

/** A form over a flat object its JSON Schema describes. Anything deeper is edited as JSON. */
export function SchemaForm(props: {
  schema: ObjectSchema
  value: Record<string, unknown>
  onChange: (value: Record<string, unknown>) => void
}) {
  const { schema, value, onChange } = props
  return (
    <div className="schema-form">
      {Object.entries(schema.properties ?? {}).map(([name, p]) => (
        <label key={name}>
          <span>
            {name}
            {schema.required?.includes(name) ? '' : ' (optional)'}
          </span>
          <Field name={name} schema={p} value={value[name]} onChange={(v) => onChange({ ...value, [name]: v })} />
          {p.description && <small>{p.description}</small>}
        </label>
      ))}
    </div>
  )
}

function Field({ name, schema, value, onChange }: { name: string; schema: PropertySchema; value: unknown; onChange: (v: unknown) => void }) {
  const type = typeOf(schema)
  if (schema.enum)
    return (
      <select value={value === undefined ? '' : String(value)} onChange={(e) => onChange(e.target.value || undefined)}>
        <option value="" />
        {schema.enum.map((o) => (
          <option key={o}>{o}</option>
        ))}
      </select>
    )
  if (type === 'boolean') return <input type="checkbox" checked={Boolean(value)} onChange={(e) => onChange(e.target.checked || undefined)} />
  if (type === 'number' || type === 'integer')
    return (
      <input
        inputMode="numeric"
        value={value === undefined ? '' : String(value)}
        onChange={(e) => onChange(e.target.value === '' ? undefined : Number(e.target.value))}
      />
    )
  if (type === 'array' && typeOf(schema.items ?? {}) === 'string')
    return (
      <input
        placeholder="comma, separated"
        value={Array.isArray(value) ? value.join(', ') : ''}
        onChange={(e) => onChange(e.target.value.split(',').map((s) => s.trim()).filter(Boolean))}
      />
    )
  if (type === 'string' || type === undefined) {
    const text = value === undefined ? '' : String(value)
    const set = (s: string) => onChange(s === '' ? undefined : s)
    return LONG.has(name) ? (
      <textarea className={name === 'source' ? 'mono' : ''} rows={name === 'source' ? 8 : 3} value={text} onChange={(e) => set(e.target.value)} />
    ) : (
      <input value={text} onChange={(e) => set(e.target.value)} />
    )
  }
  return <JsonField value={value} onChange={onChange} />
}

function JsonField({ value, onChange }: { value: unknown; onChange: (v: unknown) => void }) {
  const [draft, setDraft] = useState(value === undefined ? '' : JSON.stringify(value, null, 2))
  const [bad, setBad] = useState(false)
  return (
    <textarea
      className={`mono ${bad ? 'bad' : ''}`}
      rows={4}
      value={draft}
      placeholder="JSON"
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
