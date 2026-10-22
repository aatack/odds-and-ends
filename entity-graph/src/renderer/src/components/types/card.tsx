import React from 'react'
import { viewsFor } from './index'
import { TextCardEdit } from './text'
import type { CardProps, EditProps } from './types'

/**
 * A row's card, as its type draws it — or, while the row is being edited, the
 * type's edit box, which is a text box over `text` for a type that has none of
 * its own. Editing is the card's business: the outline only says whether the row
 * is being edited and hands over the draft.
 */
export function TypeCard({
  row,
  highlight,
  onDraft,
  onCommit,
}: CardProps & Omit<EditProps, 'draft'>): React.JSX.Element {
  const { Card, Edit = TextCardEdit } = viewsFor(row.type)
  if (row.editing) {
    return <Edit row={row} draft={row.draft ?? ''} onDraft={onDraft} onCommit={onCommit} />
  }
  return <Card row={row} highlight={highlight} />
}
