import type { KeyBinding } from './keys'
import type { CallContext } from '../state/types'

// What a tool is. "Tool" here means anything the user can invoke through the
// command palette — moving the selection, opening a tab and toggling the theme
// all count, alongside the handful of actions the source itself exposes. Each is
// declared once, and both the key router and the palette dispatch through that
// declaration, so a hotkey and its palette entry cannot drift.

/** How a value is entered, and how it's parsed out of what the user types. */
export type ArgKind = 'string' | 'number' | 'boolean' | 'entity' | 'select' | 'json'

export interface ArgSpec {
  /** The key this value is passed to `run` under. */
  name: string
  label: string
  /** Defaults to "string". */
  kind?: ArgKind
  /** Choices, for a `select`. */
  options?: readonly string[]
  /**
   * Context key that fills this argument. Omitted means never auto-filled —
   * opt-in, so an argument like `text` isn't silently inherited from an ancestor
   * and then skipped past.
   */
  fromContext?: string
  /**
   * The tool supplies its own value when this is left as `default`. Tab still
   * lands on it (unlike a context-filled argument), so it can be overridden.
   */
  hasDefault?: boolean
  /** May be left empty. Empty optional arguments are dropped before `run`. */
  optional?: boolean
  placeholder?: string
  /**
   * What the argument is for, at whatever length its author wrote it. Shown on
   * hover rather than in the field, which belongs to {@link label}.
   */
  description?: string
  /**
   * Filled by pointing at something rather than typing: with the palette hidden,
   * pressing the tool's own hotkey again takes this from the live selection.
   */
  pick?: 'entity'
}

/**
 * How far a tool reaches. Only `external` calls are kept in the log — source
 * reads and writes are far too frequent, and their results far too large, to
 * persist.
 */
export type ToolReach = 'ui' | 'source' | 'external'

/**
 * Which part of the focus chain a tool belongs to. The key router tries the
 * focused frame first, then its tab group, then the app, so the same key can
 * mean different things at different depths.
 */
export type ToolScope = 'frame' | 'group' | 'app'

/** What a tool hands back. All fields optional; `message` becomes a toast. */
export interface ToolOutcome {
  data?: unknown
  message?: string
  /** Overrides {@link ToolSpec.mutates} for this run — e.g. "nothing to write". */
  mutated?: boolean
}

export interface ToolSpec {
  id: string
  label: string
  /** Extra terms the palette's fuzzy search matches, e.g. synonyms. */
  aliases?: string[]
  /** Faint text on the right of the palette row (a category, say). */
  hint?: string
  scope: ToolScope
  reach: ToolReach
  /** Arguments prompted for, in order. Absent for tools that need none. */
  args?: ArgSpec[]
  /** Keys that invoke it. Absent for palette-only tools. */
  keys?: KeyBinding[]
  /**
   * Ids of other tools whose keys also fill this one's outstanding `pick`
   * argument while it waits. `r` starts a link one way and `shift+r` the other,
   * but by the time the far end is being chosen the direction is settled, so
   * either key should finish it.
   */
  pickAlso?: string[]
  /**
   * Set when running it changes the entity store, which is what strands the undo
   * stack. It says nothing about refetching: a write made through `source/entity`
   * tells the cache what it changed on its way out, so there is nothing left for
   * the call machine to invalidate.
   */
  mutates?: boolean
  /**
   * Set when the store may change where this side cannot see it — a Claude
   * session writing notes over MCP, an integration acting on the graph through
   * the server. Nothing in the cache accounts for those, so the only honest
   * answer is to read everything again. Separate from {@link mutates}, since a
   * tool can be one without the other: a pull request listed changes nothing and
   * still deserves a look afterwards, and a rename changes everything about a row
   * without a single unaccounted event.
   */
  writesUnseen?: boolean
  /**
   * Set for the tools that work *on* the undo stack. Any other write clears it,
   * since events written back after newer edits would come back out of order.
   */
  preservesUndo?: boolean
  /** False hides it from the tool list; it stays reachable by key or by id. */
  listed?: boolean
  /**
   * Whether the tool applies right now. The key router skips tools that don't,
   * which is how Escape can mean "cancel this edit" in a frame and "cancel this
   * call" at the app level without either knowing about the other.
   */
  enabled?: () => boolean
  /** Argument values, keyed by name, with defaults and empties already resolved. */
  run: (args: Record<string, unknown>, call: CallInfo) => void | ToolOutcome | Promise<void | ToolOutcome>
}

/** What a tool is told about the invocation it's running under. */
export interface CallInfo {
  callId: string
  context: CallContext
}

export const argsOf = (tool: ToolSpec): ArgSpec[] => tool.args ?? []

export const kindOf = (arg: ArgSpec): ArgKind => arg.kind ?? 'string'
