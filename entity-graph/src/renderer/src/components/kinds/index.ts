import { TOOL_ID, TYPE_ID } from '../../../../core/builtins'
import {
  CodeCard,
  CodeMark,
  FileCard,
  FileFocus,
  NoteCard,
  ToolCard,
  ToolFocus,
  TypeCard,
  TypeFocus,
} from './cards'
import { CodePill, FilePill, NotePill, ToolPill, TypeKindPill } from './pills'
import type { Kind } from './types'

export type { CardProps, Kind, PillProps } from './types'

// The kinds the app knows how to draw, by `type` value: the types the store
// supplies (`type`, `tool`) and the two the app itself gives a body
// to (`code`, `file`). Anything else — a type the user wrote — is drawn as a
// note, wearing its type as a pill.
//
// This is the one place a kind's views are chosen. A row, a pill and a frame's
// root all ask here, so adding a kind is adding an entry and not a branch in
// each of them.

const NOTE: Kind = { Pill: NotePill, Card: NoteCard, prose: true }

const KINDS: Record<string, Kind> = {
  code: { Pill: CodePill, Card: CodeCard, Mark: CodeMark, prose: false },
  file: { Pill: FilePill, Card: FileCard, Focus: FileFocus, prose: false },
  [TYPE_ID]: { Pill: TypeKindPill, Card: TypeCard, Focus: TypeFocus, prose: true },
  [TOOL_ID]: { Pill: ToolPill, Card: ToolCard, Focus: ToolFocus, prose: true },
}

/** How an entity of this type is drawn. A type nothing here knows is a note. */
export const kindOf = (type: string | undefined): Kind => (type && KINDS[type]) || NOTE
