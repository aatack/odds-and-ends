import type { LinkDirection } from '../core/types'
import type { EntityRow } from '../state/derive'

// What a tool is. "Tool" here means anything the user can invoke — tapping a
// button in the bottom bar, picking a line out of the action sheet, following a
// crumb. Each is declared once, and every gesture dispatches through that
// declaration, so a button and its sheet entry cannot drift.
//
// The desktop app's tools carry a scope (frame / group / app) and a set of key
// bindings, because there the keyboard is the primary instrument and the same key
// means different things at different depths. Neither survives the move to a
// phone: there is one view, so there is one scope, and there are no keys — the
// bottom bar and the action sheet are what a hotkey was.

export type ArgKind =
  | 'string'
  /** Multi-line. */
  | 'text'
  | 'number'
  | 'json'
  | 'select'
  /** An entity id, which can be typed or (with `pick`) tapped. */
  | 'entity'

export interface ArgSpec {
  /** The key this value reaches `run` under. */
  name: string
  label: string
  /** Defaults to `string`. */
  kind?: ArgKind
  /** Choices, for a `select`. */
  options?: readonly string[]
  /**
   * Context key that fills this argument. Omitted means never auto-filled —
   * opt-in, so an argument like `text` isn't silently inherited from the row the
   * user happens to be on and then skipped past.
   */
  fromContext?: string
  /** May be left empty. Empty optional arguments never reach `run`. */
  optional?: boolean
  placeholder?: string
  /**
   * Filled by tapping a row rather than typing it. The outline stays on screen
   * with a banner naming what is being chosen — the phone's version of "press x,
   * move the selection, press x again".
   */
  pick?: boolean
}

/** What a tool hands back. All optional; `message` becomes a toast. */
export interface ToolOutcome {
  message?: string
  /** Overrides {@link ToolSpec.mutates} for this run — e.g. "nothing to write". */
  mutated?: boolean
}

export interface ToolSpec {
  id: string
  label: string
  /** Extra terms the action sheet's search matches. */
  aliases?: string[]
  /** Group heading in the action sheet. */
  hint?: string
  /** Arguments prompted for, in order. Absent for tools that need none. */
  args?: ArgSpec[]
  /** Set when running it changes the store, so the outline must refetch. */
  mutates?: boolean
  /**
   * Set for the tools that work *on* the undo stack. Any other write clears it,
   * since events written back after newer edits would land out of order.
   */
  preservesUndo?: boolean
  /** False keeps it out of the action sheet; it stays reachable by id. */
  listed?: boolean
  /** Whether the tool applies right now. */
  enabled?: (ctx: ToolContext) => boolean
  run: (
    args: Record<string, unknown>,
    ctx: ToolContext,
  ) => void | ToolOutcome | Promise<void | ToolOutcome>
}

/**
 * Where a tool was invoked. Assembled once when the call starts and then fixed.
 *
 * Deliberately thinner than the desktop's: there, the context folds every entity
 * value along the path to the selection, because the integration tools (GitHub,
 * Slack) take arguments like `channel` that an ancestor is expected to have
 * declared. This app has no integrations, so the context is positional — which
 * row, which parent, which level — plus the sibling order that inserting and
 * reordering need.
 */
export interface ToolContext {
  rootId: string
  direction: LinkDirection
  /** The resolved selection: its leaf, its parent, and the path to it. */
  entityId: string | null
  parentId: string | null
  path: string[]
  /** The selected row, when the selection is on one. */
  row: EntityRow | null
  rows: EntityRow[]
  /** Values arguments may be filled from, by `fromContext`. */
  values: Record<string, unknown>
}

export const argsOf = (tool: ToolSpec): ArgSpec[] => tool.args ?? []

export const kindOf = (arg: ArgSpec): ArgKind => arg.kind ?? 'string'

/** A tool that only makes sense with something selected. */
export const needsRow = (ctx: ToolContext): boolean => ctx.row != null

/** A tool that edits the link between a row and the row above it. */
export const needsParent = (ctx: ToolContext): boolean => ctx.row != null && ctx.parentId != null

/**
 * A tool that writes structure. A reversed level draws the same tree upside down —
 * a row hangs off the one above it by a link running the other way — so the tools
 * that reorder or re-parent are simply switched off there rather than quietly
 * doing the opposite of what they say.
 */
export const needsForward = (ctx: ToolContext): boolean =>
  ctx.direction === 'out' && ctx.row != null
