import { v4 as uuid } from 'uuid'

// The layout state model. The app is a VS Code-style arrangement:
//
//   layout                       (one global tree, persisted to localStorage)
//     └─ groups[]                (columns dividing the screen evenly)
//          └─ tabs[]             (one visible at a time per group)
//               └─ frames[]      (a stack; only the top one renders)
//                    └─ view     (what a frame actually shows)
//
// Everything is stored normalised — groups hold tab ids, tabs hold frame ids —
// so a tab can move between groups by id without its frames coming along in the
// blob, and a frame carries a back-pointer to its tab (per the design notes).

/** The well-known root entity every source tree starts from. */
export const ROOT_ID = '@index'

/** Default rendered width of a canvas node, in px, when none is stored. */
export const CANVAS_DEFAULT_WIDTH = 360

// --- Views ------------------------------------------------------------------

export interface EntityView {
  kind: 'entity'
  rootId: string
  /** null = no depth limit. */
  maxDepth: number | null
}

export interface CanvasNode {
  x: number
  y: number
  /** Rendered width in px; falls back to CANVAS_DEFAULT_WIDTH when omitted. */
  width?: number
  /** When set, the sub-view is confined to this height and scrolls within it. */
  height?: number
}

export interface CanvasView {
  kind: 'canvas'
  /** entity id → where its sub-view sits on the board. */
  nodes: Record<string, CanvasNode>
}

export type View = EntityView | CanvasView

// --- Structure --------------------------------------------------------------

export interface Frame {
  id: string
  tabId: string
  view: View
}

export interface Tab {
  id: string
  /** Stack of frame ids; the last is the visible ("top") frame. */
  frameIds: string[]
  /** Frames popped off the stack, kept so a pop can be undone. Last = most recent. */
  history: string[]
}

export interface TabGroup {
  id: string
  tabIds: string[]
  activeTabId: string | null
}

export interface LayoutState {
  groups: TabGroup[]
  tabs: Record<string, Tab>
  frames: Record<string, Frame>
  focusedGroupId: string | null
  /** When true, only the focused group shows, full-width (the `m` action). */
  solo: boolean
}

// --- Constructors -----------------------------------------------------------

export function entityView(rootId: string, maxDepth: number | null = null): EntityView {
  return { kind: 'entity', rootId, maxDepth }
}

export function canvasView(seedEntityId: string): CanvasView {
  return { kind: 'canvas', nodes: { [seedEntityId]: { x: 40, y: 40 } } }
}

/** The initial layout: a single group / tab / entity frame at the root. */
export function defaultLayout(rootId: string = ROOT_ID): LayoutState {
  const frameId = uuid()
  const tabId = uuid()
  const groupId = uuid()
  return {
    groups: [{ id: groupId, tabIds: [tabId], activeTabId: tabId }],
    tabs: { [tabId]: { id: tabId, frameIds: [frameId], history: [] } },
    frames: { [frameId]: { id: frameId, tabId, view: entityView(rootId) } },
    focusedGroupId: groupId,
    solo: false,
  }
}

/** The last element of an array, or undefined when empty. */
export function last<T>(xs: readonly T[]): T | undefined {
  return xs.length ? xs[xs.length - 1] : undefined
}

/** A short label for a view, shown on its tab. */
export function viewTitle(view: View): string {
  if (view.kind === 'canvas') return 'Canvas'
  return view.rootId === ROOT_ID ? 'Index' : view.rootId
}

/**
 * A tab's label: an entity view shows its root entity's text (from the resolved
 * `names` map), falling back to "Index"/the id until that text is known.
 */
export function tabTitle(view: View, names: Record<string, string>): string {
  if (view.kind === 'canvas') return 'Canvas'
  return names[view.rootId] || (view.rootId === ROOT_ID ? 'Index' : view.rootId)
}
