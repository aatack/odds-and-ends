import { SchemaForm, type Editors, type JsonSchema } from './components/schema'
import { isObject } from './components/schema'

/** Fields that hold prose, and so get a growing text box; `source` is code. */
const LONG = new Set(['needed', 'instructions', 'message', 'text', 'body', 'source'])

/**
 * A schema from the engine, with the form's keywords added by field name: the
 * engine's schemas say what a value is, not how it is typed in.
 */
export function withHints(schema: unknown, name?: string): JsonSchema {
  if (!isObject(schema)) return {}
  const out: JsonSchema = { ...schema }
  if (name && LONG.has(name) && (out.type === 'string' || out.type === undefined)) out['x-multiline'] = true
  if (name === 'source') out['x-monospace'] = true
  if (isObject(out.properties))
    out.properties = Object.fromEntries(Object.entries(out.properties).map(([k, v]) => [k, withHints(v, k)]))
  if (isObject(out.items)) out.items = withHints(out.items)
  if (Array.isArray(out.anyOf)) out.anyOf = out.anyOf.map((v) => withHints(v))
  return out
}

/** The app's own editors: a token is typed into a field that doesn't show it. */
export const EDITORS: Editors = {
  secret: {
    name: 'Secret',
    kinds: ['string'],
    render: ({ value, onChange, label }) => (
      <input
        type="password"
        aria-label={label}
        className="h-8 w-full bg-transparent px-2.5 text-[13px] text-gray-900 focus:outline-none"
        value={typeof value === 'string' ? value : ''}
        onChange={(e) => onChange(e.target.value === '' ? undefined : e.target.value)}
      />
    ),
  },
}

/** Every input in the app: a SchemaForm with the app's editors. */
export function Form(props: { schema: JsonSchema; value: unknown; setValue: (v: unknown) => void; bare?: boolean }) {
  return <SchemaForm {...props} editors={EDITORS} />
}

export const MULTILINE: JsonSchema = { type: 'string', 'x-multiline': true }
