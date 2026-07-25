import { v4 as uuid } from 'uuid'

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
 * `null` on the latter (see `stripNulls` in core/source/types.ts), so the two
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
  | { kind: 'cancelled' }
  | { kind: 'success'; data?: unknown; message?: string }
  | { kind: 'error'; message: string }

/** A call that has finished — cancelled, succeeded or failed. */
export interface RecordedCall {
  callId: string
  /** Non-null: a call cannot finish without a tool. */
  toolId: string
  args: ArgValues
  context: CallContext
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
   * Path of entity ids to the selection. A path, not an id, because the graph
   * isn't a tree: the same entity can appear several times in one frame. Latent
   * — the *resolved* path is derived and never written back over this.
   */
  selectedPath: string[]
  /** Free-text filter over the rows; null when not filtering. */
  find: string | null
  /**
   * entity id → depth cap below it (null = uncapped, missing = uncapped). Only
   * the root's entry reaches the server so far; the rest is provisioned for a
   * later change to the `query` tool.
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

export function newFrame(tabId: string, rootId: string): FrameState {
  return {
    id: uuid(),
    tabId,
    rootId,
    selectedPath: [rootId],
    find: null,
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
