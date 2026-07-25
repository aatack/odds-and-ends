import {
  ROOT_ID,
  last,
  newFrame,
  newTab,
  type EditState,
  type FrameState,
  type GroupState,
  type LayoutState,
  type TabState,
} from './types'
import { v4 as uuid } from 'uuid'

// Pure LayoutState → LayoutState transforms. No IO, no React, no atoms: the
// store applies these, tools call the store. Kept separate so the interesting
// structural rules (what happens to a group when its last tab closes) are
// readable and testable on their own.

// --- Small setters ----------------------------------------------------------

const withGroup = (s: LayoutState, group: GroupState): LayoutState => ({
  ...s,
  groups: { ...s.groups, [group.id]: group },
})

const withTab = (s: LayoutState, tab: TabState): LayoutState => ({
  ...s,
  tabs: { ...s.tabs, [tab.id]: tab },
})

export const withFrame = (s: LayoutState, frame: FrameState): LayoutState => ({
  ...s,
  frames: { ...s.frames, [frame.id]: frame },
})

export function updateFrame(
  s: LayoutState,
  frameId: string,
  patch: (f: FrameState) => FrameState,
): LayoutState {
  const frame = s.frames[frameId]
  if (!frame) return s
  const next = patch(frame)
  return next === frame ? s : withFrame(s, next)
}

export function updateTab(
  s: LayoutState,
  tabId: string,
  patch: (t: TabState) => TabState,
): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab) return s
  const next = patch(tab)
  return next === tab ? s : withTab(s, next)
}

/** Delete frames from the map, e.g. when their tab closes or their branch is cut. */
const dropFrames = (frames: Record<string, FrameState>, ids: string[]): Record<string, FrameState> => {
  const next = { ...frames }
  for (const id of ids) delete next[id]
  return next
}

// --- Selection within a frame ----------------------------------------------

export const setSelectedPath = (s: LayoutState, frameId: string, path: string[]): LayoutState =>
  updateFrame(s, frameId, (f) => ({ ...f, selectedPath: path }))

export const setEdit = (s: LayoutState, frameId: string, edit: EditState | null): LayoutState =>
  updateFrame(s, frameId, (f) => ({ ...f, edit }))

export const setFind = (s: LayoutState, frameId: string, find: string | null): LayoutState =>
  updateFrame(s, frameId, (f) => ({ ...f, find }))

export const setSectionsOnly = (s: LayoutState, frameId: string, on: boolean): LayoutState =>
  updateFrame(s, frameId, (f) => ({ ...f, sectionsOnly: on }))

export const setMaxDepth = (
  s: LayoutState,
  frameId: string,
  entityId: string,
  depth: number | null,
): LayoutState =>
  updateFrame(s, frameId, (f) => ({ ...f, maxDepth: { ...f.maxDepth, [entityId]: depth } }))

// --- Collapse (per tab) -----------------------------------------------------

export function setCollapsed(
  s: LayoutState,
  tabId: string,
  entityId: string,
  collapsed: boolean,
): LayoutState {
  return updateTab(s, tabId, (t) => {
    const has = t.collapsed.includes(entityId)
    if (has === collapsed) return t
    return {
      ...t,
      collapsed: collapsed ? [...t.collapsed, entityId] : t.collapsed.filter((id) => id !== entityId),
    }
  })
}

export const toggleCollapsed = (s: LayoutState, tabId: string, entityId: string): LayoutState =>
  setCollapsed(s, tabId, entityId, !(s.tabs[tabId]?.collapsed.includes(entityId) ?? false))

// --- Frame stack ------------------------------------------------------------

export function pushFrame(s: LayoutState, tabId: string, rootId: string): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab) return s
  const frame = newFrame(tabId, rootId)
  // A new frame makes the redo history unreachable, so drop those frames too.
  return {
    ...s,
    tabs: { ...s.tabs, [tabId]: { ...tab, frameIds: [...tab.frameIds, frame.id], history: [] } },
    frames: { ...dropFrames(s.frames, tab.history), [frame.id]: frame },
  }
}

export function popFrame(s: LayoutState, tabId: string): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab || tab.frameIds.length <= 1) return s
  const top = last(tab.frameIds)!
  return withTab(s, { ...tab, frameIds: tab.frameIds.slice(0, -1), history: [...tab.history, top] })
}

export function undoPop(s: LayoutState, tabId: string): LayoutState {
  const tab = s.tabs[tabId]
  if (!tab || tab.history.length === 0) return s
  const restored = last(tab.history)!
  return withTab(s, {
    ...tab,
    frameIds: [...tab.frameIds, restored],
    history: tab.history.slice(0, -1),
  })
}

/** Insert `tabId` into a group immediately after `afterTabId` and make it active. */
function insertTabAfter(group: GroupState, tabId: string, afterTabId: string): GroupState {
  const at = group.tabIds.indexOf(afterTabId)
  const tabIds = [...group.tabIds]
  tabIds.splice(at < 0 ? tabIds.length : at + 1, 0, tabId)
  return { ...group, tabIds, activeTabId: tabId }
}

/** Move the top frame of `tabId` into a fresh tab beside it. */
export function popIntoNewTab(s: LayoutState, groupId: string, tabId: string): LayoutState {
  const tab = s.tabs[tabId]
  const group = s.groups[groupId]
  if (!tab || !group || tab.frameIds.length <= 1) return s
  const moved = last(tab.frameIds)!
  const newTabId = uuid()
  return {
    ...s,
    groups: { ...s.groups, [groupId]: insertTabAfter(group, newTabId, tabId) },
    tabs: {
      ...s.tabs,
      [tabId]: { ...tab, frameIds: tab.frameIds.slice(0, -1) },
      // The new tab inherits the collapse state it was looking at.
      [newTabId]: { id: newTabId, frameIds: [moved], history: [], collapsed: [...tab.collapsed] },
    },
    frames: { ...s.frames, [moved]: { ...s.frames[moved], tabId: newTabId } },
  }
}

// --- Tabs -------------------------------------------------------------------

export function addTab(s: LayoutState, groupId: string, rootId: string = ROOT_ID): LayoutState {
  const group = s.groups[groupId]
  if (!group) return s
  const { tab, frame } = newTab(rootId)
  return {
    ...s,
    groups: { ...s.groups, [groupId]: { ...group, tabIds: [...group.tabIds, tab.id], activeTabId: tab.id } },
    tabs: { ...s.tabs, [tab.id]: tab },
    frames: { ...s.frames, [frame.id]: frame },
    selectedGroupId: groupId,
  }
}

export function closeTab(s: LayoutState, groupId: string, tabId: string): LayoutState {
  const group = s.groups[groupId]
  const tab = s.tabs[tabId]
  if (!group || !tab) return s

  const tabIds = group.tabIds.filter((t) => t !== tabId)
  const tabs = { ...s.tabs }
  delete tabs[tabId]
  const frames = dropFrames(s.frames, [...tab.frameIds, ...tab.history])

  let groups = {
    ...s.groups,
    [groupId]: {
      ...group,
      tabIds,
      activeTabId: group.activeTabId === tabId ? (last(tabIds) ?? null) : group.activeTabId,
    },
  }
  let groupOrder = s.groupOrder

  // An emptied group goes away — unless it's the last one, which keeps a fresh
  // tab so the app never goes blank.
  if (tabIds.length === 0) {
    if (groupOrder.length > 1) {
      groups = { ...groups }
      delete groups[groupId]
      groupOrder = groupOrder.filter((g) => g !== groupId)
    } else {
      const seed = newTab()
      groups = { ...groups, [groupId]: { ...group, tabIds: [seed.tab.id], activeTabId: seed.tab.id } }
      tabs[seed.tab.id] = seed.tab
      frames[seed.frame.id] = seed.frame
    }
  }

  const selectedGroupId = groups[s.selectedGroupId ?? ''] ? s.selectedGroupId : (groupOrder[0] ?? null)
  return { ...s, groups, groupOrder, tabs, frames, selectedGroupId }
}

export function selectTab(s: LayoutState, groupId: string, tabId: string): LayoutState {
  const group = s.groups[groupId]
  if (!group) return s
  return { ...withGroup(s, { ...group, activeTabId: tabId }), selectedGroupId: groupId }
}

/** Cycle a group's active tab, wrapping. */
export function cycleTab(s: LayoutState, groupId: string, dir: -1 | 1): LayoutState {
  const group = s.groups[groupId]
  if (!group || group.tabIds.length === 0) return s
  const n = group.tabIds.length
  const cur = group.activeTabId ? group.tabIds.indexOf(group.activeTabId) : 0
  return withGroup(s, { ...group, activeTabId: group.tabIds[(((cur + dir) % n) + n) % n] })
}

/** Move a tab into the neighbouring group, spinning up a new one off either edge. */
export function moveTab(s: LayoutState, groupId: string, tabId: string, dir: -1 | 1): LayoutState {
  const from = s.groups[groupId]
  if (!from || !from.tabIds.includes(tabId)) return s

  const groups: Record<string, GroupState> = { ...s.groups }
  let groupOrder = [...s.groupOrder]
  const remaining = from.tabIds.filter((t) => t !== tabId)
  groups[groupId] = {
    ...from,
    tabIds: remaining,
    activeTabId: from.activeTabId === tabId ? (last(remaining) ?? null) : from.activeTabId,
  }

  const at = groupOrder.indexOf(groupId)
  const target = at + dir
  let selectedGroupId: string
  if (target < 0 || target >= groupOrder.length) {
    const group: GroupState = { id: uuid(), tabIds: [tabId], activeTabId: tabId }
    groups[group.id] = group
    groupOrder = target < 0 ? [group.id, ...groupOrder] : [...groupOrder, group.id]
    selectedGroupId = group.id
  } else {
    const to = groups[groupOrder[target]]
    groups[to.id] = { ...to, tabIds: [...to.tabIds, tabId], activeTabId: tabId }
    selectedGroupId = to.id
  }

  // Drop the source group if the move emptied it (never the last one).
  if (remaining.length === 0 && groupOrder.length > 1) {
    delete groups[groupId]
    groupOrder = groupOrder.filter((g) => g !== groupId)
  }
  return { ...s, groups, groupOrder, selectedGroupId }
}

// --- Groups -----------------------------------------------------------------

export const selectGroup = (s: LayoutState, groupId: string): LayoutState =>
  s.groups[groupId] ? { ...s, selectedGroupId: groupId } : s

export function selectAdjacentGroup(s: LayoutState, dir: -1 | 1): LayoutState {
  const at = s.groupOrder.indexOf(s.selectedGroupId ?? '')
  const from = at < 0 ? 0 : at
  const next = Math.min(s.groupOrder.length - 1, Math.max(0, from + dir))
  return { ...s, selectedGroupId: s.groupOrder[next] ?? s.selectedGroupId }
}

export const toggleExpanded = (s: LayoutState): LayoutState => ({ ...s, expanded: !s.expanded })
