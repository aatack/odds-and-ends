import React, { useState } from 'react'
import { CopyButton } from './ui/CopyButton'
import { Modal } from './ui/Modal'
import {
  JsonEditor,
  SchemaBuilder,
  SchemaForm,
  isObject,
  pretty,
  type Editors,
  type JsonSchema,
} from './schema'

/**
 * Editors to try the hook with: a control drawn in place (colour), one built of
 * buttons (stars), and one that runs code and hands a value back (roll).
 */
const EDITORS: Editors = {
  colour: {
    name: 'Colour picker',
    kinds: ['string'],
    render: ({ value, onChange }) => (
      <div className="flex items-center gap-2 px-2.5">
        <input
          type="color"
          className="h-6 w-8 bg-transparent"
          value={typeof value === 'string' && /^#[0-9a-f]{6}$/i.test(value) ? value : '#000000'}
          onChange={(e) => onChange(e.target.value)}
        />
        <span className="font-mono text-[12.5px] text-gray-500">{String(value ?? '')}</span>
      </div>
    ),
  },
  stars: {
    name: 'Stars',
    kinds: ['integer', 'number'],
    render: ({ value, onChange }) => (
      <div className="flex px-1.5">
        {[1, 2, 3, 4, 5].map((n) => (
          <button
            key={n}
            className={`px-0.5 text-[15px] ${typeof value === 'number' && value >= n ? 'text-brand-600' : 'text-gray-300'}`}
            onClick={() => onChange(value === n ? 0 : n)}
          >
            ★
          </button>
        ))}
      </div>
    ),
  },
  roll: {
    name: 'Dice roll',
    kinds: ['integer', 'number'],
    render: ({ value, onChange }) => (
      <button
        className="px-2.5 text-left text-[13px] text-gray-700"
        // Stands in for any code that comes back with a value later.
        onClick={async () => {
          await new Promise((r) => setTimeout(r, 200))
          onChange(1 + Math.floor(Math.random() * 6))
        }}
      >
        {typeof value === 'number' ? `Rolled ${value} — roll again` : 'Roll'}
      </button>
    ),
  },
}

// Debug surfaces for the two schema components: each in a modal, with its input
// and output as JSON beside it, both editable and both copyable, so a case can be
// pasted in, poked at, and its result pasted back out.

const EXAMPLE_SCHEMA: JsonSchema = {
  type: 'object',
  properties: {
    title: { type: 'string', description: 'What it is called' },
    colour: { type: 'string', 'x-editor': 'colour' },
    spice: { type: 'integer', minimum: 0, maximum: 5, 'x-editor': 'stars' },
    servings: { type: 'integer', minimum: 1, maximum: 12 },
    method: { type: 'string', 'x-multiline': true, description: 'How to make it' },
    vegetarian: { type: 'boolean' },
    course: { anyOf: [{ const: 'starter' }, { const: 'main' }, { const: 'dessert' }] },
    rating: { anyOf: [{ type: 'integer', description: 'Out of five' }, { type: 'null' }] },
    ingredients: {
      type: 'array',
      items: {
        type: 'object',
        properties: { name: { type: 'string' }, grams: { type: 'number' } },
        required: ['name'],
      },
    },
    snippet: { type: 'string', 'x-multiline': true, 'x-monospace': true },
    shape: {
      $ref: 'https://json-schema.org/draft/2020-12/schema',
      'x-popout': true,
      description: 'A schema of its own',
    },
  },
  required: ['title'],
}

/** A JSON pane: a heading, a copy button, and the editable text. */
function Pane({
  label,
  value,
  setValue,
}: {
  label: string
  value: unknown
  setValue: (value: unknown) => void
}): React.JSX.Element {
  return (
    <div className="min-w-0 flex-1">
      <div className="flex items-center justify-between pb-1">
        <span className="text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
          {label}
        </span>
        <CopyButton value={pretty(value)} title={`Copy ${label.toLowerCase()}`} />
      </div>
      <JsonEditor value={value} setValue={setValue} />
    </div>
  )
}

export function SchemaFormDebug({ onClose }: { onClose: () => void }): React.JSX.Element {
  const [schema, setSchema] = useState<unknown>(EXAMPLE_SCHEMA)
  const [value, setValue] = useState<unknown>({ title: 'Soup' })
  return (
    <Modal title="Schema form" size="large" onClose={onClose}>
      <div className="flex min-h-0 flex-1 gap-5 overflow-auto">
        <div className="flex w-80 shrink-0 flex-col gap-4">
          <Pane label="Schema" value={schema} setValue={setSchema} />
          <Pane label="Value" value={value} setValue={setValue} />
        </div>
        <div className="min-w-0 flex-1">
          <SchemaForm
            schema={isObject(schema) ? schema : {}}
            value={value}
            setValue={setValue}
            editors={EDITORS}
          />
        </div>
      </div>
    </Modal>
  )
}

export function SchemaBuilderDebug({ onClose }: { onClose: () => void }): React.JSX.Element {
  const [schema, setSchema] = useState<unknown>(EXAMPLE_SCHEMA)
  return (
    <Modal title="Schema builder" size="large" onClose={onClose}>
      <div className="flex min-h-0 flex-1 gap-5 overflow-auto">
        <div className="min-w-0 flex-1">
          <SchemaBuilder value={schema} setValue={setSchema} editors={EDITORS} />
        </div>
        <div className="w-80 shrink-0">
          <Pane label="Schema" value={schema} setValue={setSchema} />
        </div>
      </div>
    </Modal>
  )
}
