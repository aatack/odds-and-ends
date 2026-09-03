import { v4 as uuid } from 'uuid'
import type { LinkDirection } from '../../../core/entity'

export type { LinkDirection }

// Every piece of latent, serialisable state the renderer keeps. "Latent" is the
// operative word: nothing derivable lives here (no row lists, no resolved
// selections, no entity text), and nothing cached lives here either (query
// results and code output are runtime-only, in ./query and ./code).
//
//   layout                        one persisted blob
//     └─ groups[]                 columns dividing the screen
//          └─ tabs[]              one visible at a time per group
//               └─ frames[]       a stack; only the top one renders
//
// Calls (the command palette's pending invocation and the log of finished ones)
// are persisted separately, in ./store.

/** The well-known root entity every source tree starts from. */
export const ROOT_ID = '@index'

// --- Argument values --------------------------------------------------------

/**
 * One argument of a call. "Not supplied yet" and "use the tool's default" are
 * genuinely different states, and the source's tool contract already spends
 * `null` on the latter (see `stripNulls` in core/pensive/tool.ts), so the two
 * are distinguished here rather than both collapsing onto null.
 */
export type ArgValue =
  | { kind: 'empty' }
  | { kind: 'default' }
  | { kind: 'value'; value: unknown }

export type ArgValues = Record<string, ArgValue>

export const EMPTY_ARG: ArgValue = { kind: 'empty' }
export const DEFAULT_ARG: ArgValue = { kind: 'default' }
export const argValue = (value: unknown): ArgValue => ({ kind: 'value', value })

export const isFilled = (v: ArgValue | undefined): boolean =>
  v != null && v.kind !== 'empty'

// --- Calls -----------------------------------------------------------------

/**
 * Where and when a call was started. Captured once, then immutable — a call
 * resumed an hour later still records the context it was born in.
 */
export interface CallContext {
  /** Folded entity values plus the positional keys (entityId, parentId, …). */
  values: Record<string, unknown>
  /**
   * Whether those values fill arguments by themselves. False for the palette
   * opened cold, where the tool about to be picked may well be meant for some
   * *other* entity than the selected one: an argument filled silently is then
   * skipped past, with no way to say otherwise. Such a context is offered rather
   * than applied — the palette shows it as a value that can be taken with a
   * click. Absent means true, which is what a hotkey and a right-click both want.
   */
  autofill?: boolean
  /**
   * The entity path the values were folded along: each frame root in the tab's
   * stack, outermost first, then the selection path inside the top frame. Kept
   * for the record — the fold is what tools actually read.
   */
  path: string[]
  groupId: string | null
  tabId: string | null
  frameId: string | null
  startedAt: number
}

/**
 * How the pending call is shown. `palette` with an anchor reads as a context
 * menu at the cursor; without one it is the centred launcher. `hidden` means
 * only the corner toast names what is being waited on.
 */
export type CallDisplay =
  | { kind: 'hidden' }
  | { kind: 'palette'; anchor: { x: number; y: number } | null }

/** The one call currently being built up. */
export interface PendingCall {
  /** Identifies this invocation — not the tool. */
  callId: string
  /** null while the tool is still being chosen. */
  toolId: string | null
  args: ArgValues
  /** The argument being entered; null while browsing the tool list. */
  activeArg: string | null
  display: CallDisplay
  context: CallContext
  /** Search text typed while browsing the tool list. */
  query: string
  /** The recorded call this one was resumed or rerun from. */
  fromCallId?: string
}

export type CallOutcome =
  /**
   * Still going. Only calls that reach outside the app are recorded this early —
   * a Claude session runs for minutes, and a log it appears in only once it is
   * over is no use while you are waiting for it.
   */
  | { kind: 'running' }
  | { kind: 'cancelled' }
  | { kind: 'success'; data?: unknown; message?: string }
  | { kind: 'error'; message: string }

/** A call in the log: running, or cancelled, succeeded or failed. */
export interface RecordedCall {
  callId: string
  /** Non-null: a call cannot be recorded without a tool. */
  toolId: string
  args: ArgValues
  context: CallContext
  /** When the outcome was written — so, for a running call, when it began. */
  settledAt: number
  outcome: CallOutcome
  fromCallId?: string
}

// --- Layout ----------------------------------------------------------------

/** In-place text entry against a frame. The draft is persisted with it. */
export type EditState =
  | { mode: 'edit'; path: string[]; draft: string }
  | { mode: 'create'; path: string[]; draft: string; values: Record<string, unknown> }

export interface FrameState {
  id: string
  tabId: string
  /** Passed to the query as a single-item array and expanded into rows. */
  rootId: string
  /**
   * Which way the query follows links: `out` for the ordinary tree of children,
   * `in` to grow towards whatever references the root instead. Read through
   * {@link directionOf}, since layouts persisted before this existed have none.
   */
  direction: LinkDirection
  /**
   * Path of entity ids to the selection. A path, not an id, because the graph
   * isn't a tree: the same entity can appear several times in one frame. Latent
   * — the *resolved* path is derived and never written back over this.
   */
  selectedPath: string[]
  /** Free-text filter over the rows; null when not filtering. */
  find: string | null
  /**
   * Show only section rows (plus the frame's root, which anchors the view) —
   * the tree read as an outline. A filter over the rows like `find`, applied
   * after it, not a different query.
   */
  sectionsOnly: boolean
  /**
   * Show only rows left open (plus the frame's root), and stop at the ones that
   * have been ticked — the tree read as what is left to do. Alone among the
   * filters it prunes the walk as well as the rows; see `core/query`.
   */
  openOnly: boolean
  /**
   * entity id → depth cap below it (null = uncapped, missing = uncapped). The
   * root's entry is the frame's own depth limit, which the pill shows and
   * ⇧←/⇧→ move; the rest is provisioned for a later change to the `query` tool.
   */
  maxDepth: Record<string, number | null>
  edit: EditState | null
}

export interface TabState {
  id: string
  /** Stack of frame ids; the last is the visible ("top") frame. */
  frameIds: string[]
  /** Frames popped off the stack, kept so a pop can be undone. Last = most recent. */
  history: string[]
  /**
   * Entity ids folded shut, per tab rather than per frame. Keyed by id, so an
   * entity appearing twice in one frame folds in both places — unlike
   * selection, which is keyed by path.
   */
  collapsed: string[]
}

export interface GroupState {
  id: string
  tabIds: string[]
  activeTabId: string | null
}

export interface LayoutState {
  /** Id-keyed, with the column order kept separately since a record has none. */
  groups: Record<string, GroupState>
  groupOrder: string[]
  tabs: Record<string, TabState>
  frames: Record<string, FrameState>
  selectedGroupId: string | null
  /** When true only the selected group shows, full-width. One flag for all groups. */
  expanded: boolean
}

// --- Constructors ----------------------------------------------------------

/**
 * A frame's traversal direction. Tolerates a frame from a layout persisted
 * before the field existed, where "the ordinary way round" is the only answer.
 */
export const directionOf = (frame: FrameState | null | undefined): LinkDirection =>
  frame?.direction ?? 'out'

/**
 * How far below its root a frame walks, or null when it walks all the way. The
 * cap is the root's entry in the frame's depth map — no longer something the
 * sections filter implies, so that the two can be moved apart.
 *
 * Zero is not a cap but the absence of one: a frame showing its root and nothing
 * under it is what folding is for, and reading zero as "no limit" is what lets
 * ⇧← walk the cap off the bottom of the scale.
 */
export const frameDepth = (frame: FrameState | null | undefined): number | null => {
  const cap = frame ? frame.maxDepth[frame.rootId] : null
  return cap != null && cap > 0 ? cap : null
}

/**
 * How far below its root a frame reading only sections walks, unless it already
 * has a limit of its own. An outline is a table of contents rather than a
 * search: a heading buried six levels down is not part of the shape of the
 * thing, and walking to it costs a whole subtree of rows that are then thrown
 * away. Three levels is what fits on a screen and what the walk can finish.
 */
export const SECTION_DEPTH = 3

/** How a depth limit is said, in a pill and anywhere else it is named. */
export const depthLabel = (depth: number): string =>
  `${depth} level${depth === 1 ? '' : 's'} deep`

/**
 * The cap ⇧← and ⇧→ move to. No limit is the *bottom* of the scale rather than
 * something off the end of it: ⇧→ from nothing caps at one level and works up,
 * ⇧← works back down until the cap comes off, and ⇧← with no cap does nothing.
 * So holding ⇧← always arrives at the whole tree, rather than walking past it
 * and starting again.
 */
export const nudgeDepth = (depth: number | null, by: 1 | -1): number | null => {
  if (depth == null) return by > 0 ? 1 : null
  const next = depth + by
  return next > 0 ? next : null
}

export function newFrame(tabId: string, rootId: string): FrameState {
  return {
    id: uuid(),
    tabId,
    rootId,
    // Not inherited from the frame this was pushed from: whether a query type
    // should carry into a new frame is still an open question.
    direction: 'out',
    selectedPath: [rootId],
    find: null,
    sectionsOnly: false,
    openOnly: false,
    maxDepth: {},
    edit: null,
  }
}

/** A fresh tab holding one frame rooted at `rootId`. */
export function newTab(rootId: string = ROOT_ID): { tab: TabState; frame: FrameState } {
  const tabId = uuid()
  const frame = newFrame(tabId, rootId)
  return {
    tab: { id: tabId, frameIds: [frame.id], history: [], collapsed: [] },
    frame,
  }
}

/** The initial layout: one group, one tab, one frame at the root. */
export function defaultLayout(rootId: string = ROOT_ID): LayoutState {
  const { tab, frame } = newTab(rootId)
  const groupId = uuid()
  return {
    groups: { [groupId]: { id: groupId, tabIds: [tab.id], activeTabId: tab.id } },
    groupOrder: [groupId],
    tabs: { [tab.id]: tab },
    frames: { [frame.id]: frame },
    selectedGroupId: groupId,
    expanded: false,
  }
}

export function isLayoutState(v: unknown): boolean {
  const s = v as LayoutState | null
  return !!s && !!s.groups && Array.isArray(s.groupOrder) && !!s.tabs && !!s.frames
}

// --- Small shared helpers --------------------------------------------------

/** The last element of an array, or undefined when empty. */
export function last<T>(xs: readonly T[]): T | undefined {
  return xs.length ? xs[xs.length - 1] : undefined
}

export const samePath = (a: readonly string[], b: readonly string[]): boolean =>
  a.length === b.length && a.every((x, i) => x === b[i])

/**
 * A tab's collapse set as one of its frames sees it: a frame's own root always
 * expands, whatever the tab says. Collapse is what you did to that entity where
 * it sits in its parent, and opening a frame on it is how you look inside without
 * disturbing that — a frame showing one folded row and nothing else would be a
 * dead end. Used by both the query and the rows, so the chevron agrees with what
 * is on screen.
 */
export const collapsedBelow = (collapsed: readonly string[], rootId: string): string[] =>
  collapsed.filter((id) => id !== rootId)
