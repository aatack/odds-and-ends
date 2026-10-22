import React from 'react'
import { CodeBlock } from '../ui/CodeBlock'
import { ResourceView } from '../Resource'
import { fieldsOf, schemaOf } from '../../../../core/schema'
import { useGetEntities } from '../../state/hooks'
import { Aside, EMPTY, RowText } from './text'
import type { CardProps } from './types'

// Every kind's card, and the focus of the kinds that have one. A card is a row's
// worth: the gist, and the one action a kind can't do without. A focus is the
// same entity with a page to itself, so it says the rest.

// --- Note -------------------------------------------------------------------

/** What the user wrote, and nothing else. Also a note's focus. */
export function NoteCard({ row, highlight }: CardProps): React.JSX.Element {
  return row.text ? <RowText row={row} highlight={highlight} /> : EMPTY
}

// --- File -------------------------------------------------------------------

/** A thumbnail or a chip, and whatever the user typed as a caption under it. */
export function FileCard({ row, highlight }: CardProps): React.JSX.Element {
  return (
    <>
      <ResourceView id={row.id} mimeType={row.mimeType} alt={row.text} />
      <RowText row={row} highlight={highlight} muted />
    </>
  )
}

/** The whole file: an image as large as the page allows, text read out, media playing. */
export function FileFocus({ row, highlight }: CardProps): React.JSX.Element {
  return (
    <>
      <ResourceView id={row.id} mimeType={row.mimeType} alt={row.text} size="focus" />
      <RowText row={row} highlight={highlight} muted />
    </>
  )
}

// --- Type -------------------------------------------------------------------

/** The type's name, and the fields an instance of it holds. */
export function TypeCard({ row, highlight }: CardProps): React.JSX.Element {
  const values = useGetEntities()([row.id])[row.id].values
  const fields = fieldsOf(schemaOf(values))
  return (
    <>
      <NoteCard row={row} highlight={highlight} />
      {fields.length > 0 && <Aside>{fields.map((f) => f.key).join(' · ')}</Aside>}
    </>
  )
}

/** Every field, with its shape and what it is for. */
export function TypeFocus({ row, highlight }: CardProps): React.JSX.Element {
  const values = useGetEntities()([row.id])[row.id].values
  const schema = schemaOf(values)
  const fields = fieldsOf(schema)
  const description = typeof schema?.description === 'string' ? schema.description : null
  return (
    <>
      <NoteCard row={row} highlight={highlight} />
      {description && <p className="mt-1 text-[12.5px] leading-5 text-gray-500">{description}</p>}
      {fields.length > 0 ? (
        <dl className="mt-2 space-y-1.5">
          {fields.map((f) => (
            <div key={f.key} className="text-[12.5px] leading-5">
              <dt className="flex items-baseline gap-2">
                <span className="font-mono text-gray-900">{f.key}</span>
                <span className="font-mono text-gray-400">{f.label}</span>
                {f.required && <span className="text-gray-400">required</span>}
              </dt>
              {f.description && <dd className="text-gray-500">{f.description}</dd>}
            </div>
          ))}
        </dl>
      ) : (
        <Aside>No schema yet — its instances hold whatever is written to them.</Aside>
      )}
    </>
  )
}

// --- Tool -------------------------------------------------------------------

const toolFacts = (values: Record<string, unknown>): string[] =>
  [
    typeof values.key === 'string' && values.key ? values.key : null,
    typeof values.id === 'string' && values.id ? values.id : null,
  ].filter((v): v is string => v !== null)

/** What the tool is called, what it is for, and the key it answers to. */
export function ToolCard({ row, highlight }: CardProps): React.JSX.Element {
  const values = useGetEntities()([row.id])[row.id].values
  const description = typeof values.description === 'string' ? values.description : null
  const facts = toolFacts(values)
  return (
    <>
      <NoteCard row={row} highlight={highlight} />
      {(description || facts.length > 0) && (
        <Aside>{[description, ...facts].filter(Boolean).join(' · ')}</Aside>
      )}
    </>
  )
}

/** The card, then the body it runs — read-only here; the inspector edits it. */
export function ToolFocus(props: CardProps): React.JSX.Element {
  const values = useGetEntities()([props.row.id])[props.row.id].values
  const body = typeof values.execute === 'string' ? values.execute : typeof values.script === 'string' ? values.script : null
  const args = Array.isArray(values.arguments) ? values.arguments : []
  const names = args
    .map((a) => (typeof a === 'string' ? a : a && typeof a === 'object' ? (a as { name?: unknown }).name : null))
    .filter((n): n is string => typeof n === 'string' && !!n)
  return (
    <>
      <ToolCard {...props} />
      {names.length > 0 && <Aside>Takes {names.join(', ')}</Aside>}
      {body && (
        <div className="mt-2">
          <CodeBlock code={body} />
        </div>
      )}
    </>
  )
}
