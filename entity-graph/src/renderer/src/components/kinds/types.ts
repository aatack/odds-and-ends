import type React from 'react'
import type { EntityRow } from '../../state/derive'

// What a kind of entity looks like, at the three sizes it is ever drawn at. A
// view of an entity has as many dimensions as it has room for:
//
//   Pill   0-d  inline, inside prose or beside other things: a name and a mark
//   Card   1-d  a row of its own: the gist and its one or two key actions
//   Focus  2-d  a page to itself — the root of a frame — with room for all of it
//
// Every kind has the first two. A focus is optional and defaults to the card,
// which is right for a note: its page is its text with its children beneath, and
// the children are the frame's rows rather than anything the kind draws. A file
// is the case for one — a row shows a thumbnail, and its own page the whole thing.
//
// The kind is the entity's `type` value (`kindOf` in `./index`); an entity of a
// type no kind is registered for is drawn as a note. A row draws its kind's card
// and nothing else: there is no type pill in front of it.

export interface PillProps {
  id: string
  className?: string
}

/** What a card or a focus is drawn from. */
export interface CardProps {
  row: EntityRow
  /** The frame's find text, marked wherever the entity says it. */
  highlight?: string
}

export interface Kind {
  Pill: React.ComponentType<PillProps>
  Card: React.ComponentType<CardProps>
  /** The root of a frame. Absent: the card again, which is what a note wants. */
  Focus?: React.ComponentType<CardProps>
}
