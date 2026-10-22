import React from 'react'
import { CodeBlock } from '../ui/CodeBlock'
import { ResourceView } from '../Resource'
import { EMPTY, RowText } from './text'
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
