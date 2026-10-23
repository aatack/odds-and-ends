import { useState } from 'react'
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
}) {
  const { schema, value, onChange } = props
  return (
    <div className="compact">
      {Object.entries(schema.properties ?? {}).map(([name, p]) => (
        <Field
          key={name}
          name={name}
          label={`${name}${schema.required?.includes(name) ? '' : ' (optional)'}`}
          schema={p}
          value={value[name]}
          onChange={(v) => onChange({ ...value, [name]: v })}
        />
      ))}
    </div>
  )
}

function Field(props: { name: string; label: string; schema: PropertySchema; value: unknown; onChange: (v: unknown) => void }) {
  const { name, label, schema, value, onChange } = props
  const type = typeOf(schema)
  const named = { title: schema.description ? `${label}: ${schema.description}` : label, 'aria-label': label }
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
      <textarea
        {...named}
        placeholder={label}
        className={name === 'source' ? 'mono' : ''}
        rows={name === 'source' ? 6 : 2}
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
