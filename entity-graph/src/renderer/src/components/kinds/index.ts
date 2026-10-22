import { FileCard, FileFocus, NoteCard } from './cards'
import { FilePill, NotePill } from './pills'
import type { Kind } from './types'

export type { CardProps, Kind, PillProps } from './types'

// The kinds the app knows how to draw, by `type` value. Every type is defined
// here, in code: an entity whose `type` names nothing here is drawn as a note.
//
// This is the one place a kind's views are chosen. A row, a pill and a frame's
// root all ask here, so adding a kind is adding an entry and not a branch in
// each of them.

const NOTE: Kind = { Pill: NotePill, Card: NoteCard }

const KINDS: Record<string, Kind> = {
  file: { Pill: FilePill, Card: FileCard, Focus: FileFocus },
}

/** How an entity of this type is drawn. A type nothing here knows is a note. */
export const kindOf = (type: string | undefined): Kind => (type && KINDS[type]) || NOTE
