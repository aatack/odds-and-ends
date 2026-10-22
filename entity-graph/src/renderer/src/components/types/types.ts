import type React from 'react'
import type { EntityRow } from '../../state/derive'

// What a type of entity looks like, at the three sizes it is ever drawn at. A
// view of an entity has as many dimensions as it has room for:
//
//   Pill   0-d  inline, inside prose or beside other things: a name and a mark
//   Card   1-d  a row of its own: the gist and its key actions — and, while the
//               row is being edited, the box it is edited in (`Edit`)
//   Focus  2-d  a page to itself — a frame — with room for all of it
//
// Every type has a pill and a card. The rest are optional:
//
//   Edit   defaults to a text box over the entity's `text`, so a type with a
//          card of its own still edits the way every row does
//   Focus  defaults to the outline: the frame's query, a card per row it
//          returns, indented, with the frame's find, filters, folding and
//          selection (`layout/Outline.tsx`). A type with a focus of its own — a
//          file's preview, a page's — takes the whole frame instead
//
// The type is the entity's `type` value (`viewsFor` in `./index`). An entity of
// a type nothing is registered for — a custom type, say — is drawn as a note.

export interface PillProps {
  id: string
  className?: string
  /**
   * Capped at a pill's maximum width ({@link PILL_MAX_WIDTH}) — for a pill in
   * a row of others or a line of prose, where one long name would crowd the rest.
   * Unbounded, it still truncates, but only to the room it is given.
   */
  bounded?: boolean
}

/** How wide a bounded pill may get before it ellipses. */
export const PILL_MAX_WIDTH = 'max-w-[140px]'

/** What a card is drawn from. */
export interface CardProps {
  row: EntityRow
  /** The frame's find text, marked wherever the entity says it. */
  highlight?: string
}

/** What a card is edited with, while its row is being edited in place. */
export interface EditProps {
  row: EntityRow
  /** The text being typed, as the frame holds it. */
  draft: string
  /** Every keystroke: the draft is the frame's, so it survives a redraw. */
  onDraft: (text: string) => void
  /** Write the draft: on Enter, or when the box loses focus. */
  onCommit: () => void
}

/** What a focus is drawn into: the frame, and the entity at its root. */
export interface FocusProps {
  frameId: string
  entityId: string
}

export interface TypeViews {
  Pill: React.ComponentType<PillProps>
  Card: React.ComponentType<CardProps>
  /** The card while its row is being edited. Absent: a text box over `text`. */
  Edit?: React.ComponentType<EditProps>
  /** The whole frame, when this entity is its root. Absent: the outline. */
  Focus?: React.ComponentType<FocusProps>
}
