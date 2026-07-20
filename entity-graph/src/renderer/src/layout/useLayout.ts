import { useCallback, useMemo, useRef, useState } from 'react'
import { v4 as uuid } from 'uuid'
import { persistentAtom, useAtom } from './atom'
import type { LayoutController } from './layoutActions'
import {
  ROOT_ID,
  canvasView,
  defaultLayout,
  entityView,
  last,
  type Frame,
  type LayoutState,
  type Tab,
  type TabGroup,
  type View,
} from './types'

// ---------------------------------------------------------------------------
// The global, persisted store
// ---------------------------------------------------------------------------

// One layout for the whole app. NOTE: it isn't scoped per source yet, so frames
// rooted at a specific entity from source A will look broken under source B.
// (Flagged in the follow-up notes.)
const layoutAtom = persistentAtom<LayoutState>(
  'entity-graph.layout',
  defaultLayout(),
  (v): boolean => !!v && Array.isArray((v as LayoutState).groups),
)

// ---------------------------------------------------------------------------
// The imperative handle a rendered view publishes so layout actions can reach
// into it (read its selection, run one of its editor actions).
// ---------------------------------------------------------------------------

export interface ViewHandle {
  getSelectedEntityId: () => string | null
  /** The selected entity's text, if any — used to name a freshly-opened tab. */
  getSelectedText?: () => string | null
  /** Canvas only: the entity id of the active panel (for "Close panel"). */
  getActiveNodeId?: () => string | null
  /** Present only for entity views; runs one of the EDITOR_ACTIONS by id. */
  runAction?: (id: string) => void
}

// ---------------------------------------------------------------------------
// Reducers — pure LayoutState → LayoutState transforms. Kept out of the hook so
// they're easy to read and test, and so the controller callbacks stay stable.
// ---------------------------------------------------------------------------

const withTab = (s: LayoutState, id: string, tab: Tab): LayoutState => ({
  ...s,
  tabs: { ...s.tabs, [id]: tab },
})

// Delete a frame and everything it references from the frames map.
const dropFrames = (frames: Record<string, Frame>, ids: string[]): Record<string, Frame> => {
  const next = { ...frames }
  for (const id of ids) delete next[id]
  return next
}

function pushFrame(s: LayoutState, tabId: string, view: View): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab) return s
  const frameId = uuid()
  const frame: Frame = { id: frameId, tabId, view }
  // A new frame clears the redo history — its previously-undone frames are now
  // unreachable, so drop them from the frames map too.
  const frames = dropFrames({ ...s.frames, [frameId]: frame }, tab.history)
  return {
    ...withTab(s, tabId, { ...tab, frameIds: [...tab.frameIds, frameId], history: [] }),
    frames,
  }
}

function popFrame(s: LayoutState, tabId: string): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab || tab.frameIds.length <= 1) return s
  const top = tab.frameIds[tab.frameIds.length - 1]
  return withTab(s, tabId, {
    ...tab,
    frameIds: tab.frameIds.slice(0, -1),
    history: [...tab.history, top],
  })
}

function undoPop(s: LayoutState, tabId: string): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab || tab.history.length === 0) return s
  const restored = tab.history[tab.history.length - 1]
  return withTab(s, tabId, {
    ...tab,
    frameIds: [...tab.frameIds, restored],
    history: tab.history.slice(0, -1),
  })
}

// Insert `tabId` into `group` immediately after `afterTabId` and make it active.
function insertTabAfter(group: TabGroup, tabId: string, afterTabId: string): TabGroup {
  const at = group.tabIds.indexOf(afterTabId)
  const tabIds = [...group.tabIds]
  tabIds.splice(at < 0 ? tabIds.length : at + 1, 0, tabId)
  return { ...group, tabIds, activeTabId: tabId }
}

function popIntoNewTab(s: LayoutState, groupId: string, tabId: string): LayoutState {
  const tab = s.tabs[tabId]
  const group = s.groups.find((g) => g.id === groupId)
  if (!tab || !group || tab.frameIds.length <= 1) return s
  const moved = tab.frameIds[tab.frameIds.length - 1]
  const newTabId = uuid()
  return {
    ...s,
    groups: s.groups.map((g) => (g.id === groupId ? insertTabAfter(g, newTabId, tabId) : g)),
    tabs: {
      ...s.tabs,
      [tabId]: { ...tab, frameIds: tab.frameIds.slice(0, -1) },
      [newTabId]: { id: newTabId, frameIds: [moved], history: [] },
    },
    // The moved frame now lives in the new tab.
    frames: { ...s.frames, [moved]: { ...s.frames[moved], tabId: newTabId } },
  }
}

// Add an entity as a node on a canvas frame (the "Add selected to canvas" action).
function addCanvasNode(s: LayoutState, frameId: string, entityId: string): LayoutState {
  const frame = s.frames[frameId]
  if (!frame || frame.view.kind !== 'canvas' || frame.view.nodes[entityId]) return s
  const offset = Object.keys(frame.view.nodes).length * 28
  const nodes = { ...frame.view.nodes, [entityId]: { x: 60 + offset, y: 60 + offset } }
  return { ...s, frames: { ...s.frames, [frameId]: { ...frame, view: { ...frame.view, nodes } } } }
}

// Remove a node from a canvas frame (the "Close panel" action).
function removeCanvasNode(s: LayoutState, frameId: string, entityId: string): LayoutState {
  const frame = s.frames[frameId]
  if (!frame || frame.view.kind !== 'canvas' || !frame.view.nodes[entityId]) return s
  const nodes = { ...frame.view.nodes }
  delete nodes[entityId]
  return { ...s, frames: { ...s.frames, [frameId]: { ...frame, view: { ...frame.view, nodes } } } }
}

function newTab(s: LayoutState, groupId: string): LayoutState {
  const group = s.groups.find((g) => g.id === groupId)
  if (!group) return s
  const tabId = uuid()
  const frameId = uuid()
  return {
    ...s,
    groups: s.groups.map((g) =>
      g.id === groupId ? { ...g, tabIds: [...g.tabIds, tabId], activeTabId: tabId } : g,
    ),
    tabs: { ...s.tabs, [tabId]: { id: tabId, frameIds: [frameId], history: [] } },
    frames: { ...s.frames, [frameId]: { id: frameId, tabId, view: entityView(ROOT_ID) } },
  }
}

function closeTab(s: LayoutState, groupId: string, tabId: string): LayoutState {
  const group = s.groups.find((g) => g.id === groupId)
  const tab = s.tabs[tabId]
  if (!group || !tab) return s

  const tabIds = group.tabIds.filter((t) => t !== tabId)
  const tabs = { ...s.tabs }
  delete tabs[tabId]
  const frames = dropFrames(s.frames, [...tab.frameIds, ...tab.history])

  let groups = s.groups.map((g) =>
    g.id === groupId
      ? { ...g, tabIds, activeTabId: g.activeTabId === tabId ? (last(tabIds) ?? null) : g.activeTabId }
      : g,
  )

  // An emptied group is removed — unless it's the last one, which keeps a fresh
  // default tab so the app never goes blank.
  if (tabIds.length === 0) {
    if (groups.length > 1) {
      groups = groups.filter((g) => g.id !== groupId)
    } else {
      const seedTab = uuid()
      const seedFrame = uuid()
      groups = groups.map((g) => ({ ...g, tabIds: [seedTab], activeTabId: seedTab }))
      tabs[seedTab] = { id: seedTab, frameIds: [seedFrame], history: [] }
      frames[seedFrame] = { id: seedFrame, tabId: seedTab, view: entityView(ROOT_ID) }
    }
  }

  const focusedGroupId = groups.some((g) => g.id === s.focusedGroupId)
    ? s.focusedGroupId
    : (groups[0]?.id ?? null)
  return { ...s, groups, tabs, frames, focusedGroupId }
}

function moveTab(s: LayoutState, groupId: string, tabId: string, dir: -1 | 1): LayoutState {
  let groups = s.groups.map((g) => ({ ...g, tabIds: [...g.tabIds] }))
  const gi = groups.findIndex((g) => g.id === groupId)
  if (gi < 0 || !groups[gi].tabIds.includes(tabId)) return s

  // Detach from the source group.
  groups[gi].tabIds = groups[gi].tabIds.filter((t) => t !== tabId)
  if (groups[gi].activeTabId === tabId) groups[gi].activeTabId = last(groups[gi].tabIds) ?? null

  const target = gi + dir
  let focusedGroupId: string
  if (target < 0 || target >= groups.length) {
    // Off the edge: spin up a new group to hold it.
    const ng: TabGroup = { id: uuid(), tabIds: [tabId], activeTabId: tabId }
    if (target < 0) groups.unshift(ng)
    else groups.push(ng)
    focusedGroupId = ng.id
  } else {
    groups[target].tabIds.push(tabId)
    groups[target].activeTabId = tabId
    focusedGroupId = groups[target].id
  }

  // Drop the source group if the move emptied it (but never the last group).
  groups = groups.filter((g) => g.tabIds.length > 0 || groups.length === 1)
  return { ...s, groups, focusedGroupId }
}

function focusAdjacentGroup(s: LayoutState, dir: -1 | 1): LayoutState {
  const i = s.groups.findIndex((g) => g.id === s.focusedGroupId)
  const from = i < 0 ? 0 : i
  const next = Math.min(s.groups.length - 1, Math.max(0, from + dir))
  return { ...s, focusedGroupId: s.groups[next]?.id ?? s.focusedGroupId }
}

// Cycle the focused group's active tab (wrapping) — ctrl+tab / ctrl+shift+tab.
function cycleTab(s: LayoutState, groupId: string, dir: -1 | 1): LayoutState {
  const group = s.groups.find((g) => g.id === groupId)
  if (!group || group.tabIds.length === 0) return s
  const n = group.tabIds.length
  const cur = group.activeTabId ? group.tabIds.indexOf(group.activeTabId) : 0
  const nextId = group.tabIds[(((cur + dir) % n) + n) % n]
  return { ...s, groups: s.groups.map((g) => (g.id === groupId ? { ...g, activeTabId: nextId } : g)) }
}

// ---------------------------------------------------------------------------
// Derived, render-ready shapes
// ---------------------------------------------------------------------------

export interface ResolvedGroup {
  group: TabGroup
  tabs: Tab[]
  activeTab: Tab | null
  topFrame: Frame | null
}

export interface UseLayoutResult {
  state: LayoutState
  groups: ResolvedGroup[]
  /** Entity-id → display name, populated as views mount. */
  names: Record<string, string>
  /** A view reports the display text it has learned for an entity id. */
  reportName: (id: string, text: string | undefined) => void
  /** The one frame keyboard input is routed to (focused group → active tab → top). */
  focusedFrameId: string | null
  controller: LayoutController
  /** Dispatch an editor action id to the focused frame's view, if it has one. */
  runFocusedEditorAction: (id: string) => void
  /** A view registers/deregisters (null) its imperative handle by frame id. */
  registerHandle: (frameId: string, handle: ViewHandle | null) => void
  // Targeted operations the chrome uses directly (clicks, drags).
  selectTab: (groupId: string, tabId: string) => void
  focusGroup: (groupId: string) => void
  closeTab: (groupId: string, tabId: string) => void
  newTab: (groupId: string) => void
  /** Push an entity frame onto a specific tab (used by double-click-to-open). */
  pushEntityFrame: (tabId: string, entityId: string) => void
  /** Replace a frame's view (canvas drag/resize persistence). */
  updateView: (frameId: string, view: View) => void
}

/**
 * Owns the whole layout tree. Holds only the latent, persisted state (in the
 * atom) and derives the render-ready group list plus the imperative controller
 * the action registry dispatches through — mirroring how useEditor relates to
 * the editor registry, one level up.
 */
export function useLayout(): UseLayoutResult {
  const [state, setState] = useAtom(layoutAtom)

  // View handles live in a ref (no re-render on register) keyed by frame id.
  const handles = useRef<Map<string, ViewHandle>>(new Map())
  const registerHandle = useCallback((frameId: string, handle: ViewHandle | null) => {
    if (handle) handles.current.set(frameId, handle)
    else handles.current.delete(frameId)
  }, [])

  // Entity-id → display name, learned from mounted views. Ephemeral (not
  // persisted): a tab shows its root's id until its frame reports the text.
  const [names, setNames] = useState<Record<string, string>>({})
  const reportName = useCallback((id: string, text: string | undefined) => {
    setNames((prev) => {
      const v = text ?? ''
      if ((prev[id] ?? '') === v) return prev
      return { ...prev, [id]: v }
    })
  }, [])

  // Derived focus chain -------------------------------------------------------
  const focusedGroup =
    state.groups.find((g) => g.id === state.focusedGroupId) ?? state.groups[0] ?? null
  const focusedTabId = focusedGroup?.activeTabId ?? null
  const focusedTab = focusedTabId ? state.tabs[focusedTabId] : null
  const focusedFrameId = focusedTab ? last(focusedTab.frameIds) ?? null : null

  // Latest focus + state, read by the (stable) controller callbacks.
  const ctx = useRef({ groupId: null as string | null, tabId: null as string | null, frameId: null as string | null, state })
  ctx.current = {
    groupId: focusedGroup?.id ?? null,
    tabId: focusedTabId,
    frameId: focusedFrameId,
    state,
  }

  const groups = useMemo<ResolvedGroup[]>(
    () =>
      state.groups.map((group) => {
        const tabs = group.tabIds.map((id) => state.tabs[id]).filter(Boolean) as Tab[]
        const activeTab = group.activeTabId ? (state.tabs[group.activeTabId] ?? null) : null
        const topFrame = activeTab ? (state.frames[last(activeTab.frameIds) ?? ''] ?? null) : null
        return { group, tabs, activeTab, topFrame }
      }),
    [state],
  )

  // Targeted ops --------------------------------------------------------------
  const selectTab = useCallback(
    (groupId: string, tabId: string) =>
      setState((s) => ({
        ...s,
        focusedGroupId: groupId,
        groups: s.groups.map((g) => (g.id === groupId ? { ...g, activeTabId: tabId } : g)),
      })),
    [setState],
  )
  const focusGroup = useCallback(
    (groupId: string) => setState((s) => (s.focusedGroupId === groupId ? s : { ...s, focusedGroupId: groupId })),
    [setState],
  )
  const closeTabAt = useCallback(
    (groupId: string, tabId: string) => setState((s) => closeTab(s, groupId, tabId)),
    [setState],
  )
  const newTabAt = useCallback((groupId: string) => setState((s) => newTab(s, groupId)), [setState])
  const pushEntityFrame = useCallback(
    (tabId: string, entityId: string) => setState((s) => pushFrame(s, tabId, entityView(entityId))),
    [setState],
  )
  const updateView = useCallback(
    (frameId: string, view: View) =>
      setState((s) =>
        s.frames[frameId] ? { ...s, frames: { ...s.frames, [frameId]: { ...s.frames[frameId], view } } } : s,
      ),
    [setState],
  )

  const runFocusedEditorAction = useCallback((id: string) => {
    const fid = ctx.current.frameId
    if (fid) handles.current.get(fid)?.runAction?.(id)
  }, [])

  // The controller the action registry dispatches through — built once, reading
  // the latest focus through the ref so its identity stays stable.
  const controller = useMemo<LayoutController>(
    () => ({
      toggleSolo: () => setState((s) => ({ ...s, solo: !s.solo })),
      focusAdjacentGroup: (dir) => setState((s) => focusAdjacentGroup(s, dir)),
      moveTab: (dir) => {
        const { groupId, tabId } = ctx.current
        if (groupId && tabId) setState((s) => moveTab(s, groupId, tabId, dir))
      },
      focusSelectedEntity: () => {
        const { tabId, frameId } = ctx.current
        if (!tabId || !frameId) return
        const handle = handles.current.get(frameId)
        const id = handle?.getSelectedEntityId() ?? null
        if (!id) return
        // Don't stack a frame whose root is already the current view's root —
        // that just makes a duplicate you'd have to pop straight back off.
        const cur = ctx.current.state.frames[frameId]
        if (cur?.view.kind === 'entity' && cur.view.rootId === id) return
        // Seed the new tab's name from the selected row so it's labelled at once.
        const text = handle?.getSelectedText?.() ?? undefined
        if (text) reportName(id, text)
        setState((s) => pushFrame(s, tabId, entityView(id)))
      },
      cycleTab: (dir) => {
        const { groupId } = ctx.current
        if (groupId) setState((s) => cycleTab(s, groupId, dir))
      },
      addSelectedToCanvas: () => {
        const { frameId } = ctx.current
        if (!frameId) return
        const frame = ctx.current.state.frames[frameId]
        if (frame?.view.kind !== 'canvas') return
        const id = handles.current.get(frameId)?.getSelectedEntityId() ?? null
        if (id) setState((s) => addCanvasNode(s, frameId, id))
      },
      closePanel: () => {
        const { frameId } = ctx.current
        if (!frameId) return
        const frame = ctx.current.state.frames[frameId]
        if (frame?.view.kind !== 'canvas') return
        const id = handles.current.get(frameId)?.getActiveNodeId?.() ?? null
        if (id) setState((s) => removeCanvasNode(s, frameId, id))
      },
      popFrame: () => {
        const { tabId } = ctx.current
        if (tabId) setState((s) => popFrame(s, tabId))
      },
      undoPop: () => {
        const { tabId } = ctx.current
        if (tabId) setState((s) => undoPop(s, tabId))
      },
      popIntoNewTab: () => {
        const { groupId, tabId } = ctx.current
        if (groupId && tabId) setState((s) => popIntoNewTab(s, groupId, tabId))
      },
      openCanvas: () => {
        const { tabId, frameId } = ctx.current
        if (!tabId) return
        // Seed the board with the focused view's selection, else its own root.
        const selected = frameId ? handles.current.get(frameId)?.getSelectedEntityId() ?? null : null
        const frame = frameId ? ctx.current.state.frames[frameId] : null
        const seed =
          selected ?? (frame?.view.kind === 'entity' ? frame.view.rootId : ROOT_ID)
        setState((s) => pushFrame(s, tabId, canvasView(seed)))
      },
      newTab: () => {
        const { groupId } = ctx.current
        if (groupId) setState((s) => newTab(s, groupId))
      },
      closeTab: () => {
        const { groupId, tabId } = ctx.current
        if (groupId && tabId) setState((s) => closeTab(s, groupId, tabId))
      },
    }),
    [setState, reportName],
  )

  return {
    state,
    groups,
    names,
    reportName,
    focusedFrameId,
    controller,
    runFocusedEditorAction,
    registerHandle,
    selectTab,
    focusGroup,
    closeTab: closeTabAt,
    newTab: newTabAt,
    pushEntityFrame,
    updateView,
  }
}
