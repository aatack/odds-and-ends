import React, { useState } from 'react'
import { CopyButton } from '../ui/CopyButton'
import { Modal } from '../ui/Modal'
import { JsonEditor } from './JsonEditor'
import { SchemaBuilder } from './SchemaBuilder'
import { SchemaForm } from './SchemaForm'
import { isObject, pretty, type JsonSchema } from './schema'

// Debug surfaces for the two schema components: each in a modal, with its input
// and output as JSON beside it, both editable and both copyable, so a case can be
// pasted in, poked at, and its result pasted back out.

const EXAMPLE_SCHEMA: JsonSchema = {
  type: 'object',
  properties: {
    title: { type: 'string', description: 'What it is called' },
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
    notes: {},
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
        <span className="text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">{label}</span>
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
          <SchemaForm schema={isObject(schema) ? schema : {}} value={value} setValue={setValue} />
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
          <SchemaBuilder value={schema} setValue={setSchema} />
        </div>
        <div className="w-80 shrink-0">
          <Pane label="Schema" value={schema} setValue={setSchema} />
        </div>
      </div>
    </Modal>
  )
}
