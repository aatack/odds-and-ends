import { SchemaForm, type Editors, type JsonSchema } from './components/schema'
import { isObject, MenuItem } from './components/schema'
import { ChevronDown } from 'lucide-react'
import { ACTION_ICONS, actionIcon } from './actionIcons'
import { Dropdown } from './components/ui/Dropdown'

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
  /** A custom action's icon, chosen from a menu that shows each one beside its name. */
  'action-icon': {
    name: 'Action icon',
    kinds: ['string'],
    render: ({ value, onChange, label }) => {
      const at = typeof value === 'string' ? value : 'zap'
      const Icon = actionIcon(at)
      return (
        <div className="w-full">
          <Dropdown
            trigger={({ toggle }) => (
              <button aria-label={label} className="flex h-8 w-full items-center gap-1.5 px-2.5 text-left text-[13px] text-gray-900 focus:outline-none" onClick={toggle}>
                <Icon size={14} className="shrink-0" />
                <span className="min-w-0 flex-1 truncate">{at}</span>
                <ChevronDown size={12} className="shrink-0 text-gray-400" />
              </button>
            )}
          >
            {(close) =>
              Object.entries(ACTION_ICONS).map(([name, I]) => (
                <MenuItem
                  key={name}
                  on={name === at}
                  onClick={() => {
                    if (name !== at) onChange(name)
                    close()
                  }}
                >
                  <I size={14} className="mr-2 shrink-0" />
                  {name}
                </MenuItem>
              ))
            }
          </Dropdown>
        </div>
      )
    },
  },
}

/** Every input in the app: a SchemaForm with the app's editors. */
export function Form(props: { schema: JsonSchema; value: unknown; setValue: (v: unknown) => void; bare?: boolean }) {
  return <SchemaForm {...props} editors={EDITORS} />
}

export const MULTILINE: JsonSchema = { type: 'string', 'x-multiline': true }
