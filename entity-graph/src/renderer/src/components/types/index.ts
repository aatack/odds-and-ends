import { FileCard, FileFocus, NoteCard, UrlCard, UrlFocus } from './cards'
import { FilePill, NotePill, UrlPill } from './pills'
import type { TypeViews } from './types'

export type { CardProps, EditProps, FocusProps, PillProps, TypeViews } from './types'
export { TypeCard } from './card'

// The types the app knows how to draw, by `type` value — `file` and `url` — and
// the note, which is every other entity: one of no type, or of a custom type
// (a `type: type` entity), which has no views of its own.
//
// This is the one place a type's views are chosen. A row, a pill and a frame
// all ask here, so adding a type is adding an entry and not a branch in each.

const NOTE: TypeViews = { Pill: NotePill, Card: NoteCard }

const TYPES: Record<string, TypeViews> = {
  file: { Pill: FilePill, Card: FileCard, Focus: FileFocus },
  url: { Pill: UrlPill, Card: UrlCard, Focus: UrlFocus },
}

/** How an entity of this type is drawn. A type nothing here knows is a note. */
export const viewsFor = (type: string | undefined): TypeViews => (type && TYPES[type]) || NOTE
