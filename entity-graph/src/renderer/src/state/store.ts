import { atom, persistentAtom } from './atom'
import { settlePeeks } from './peeks'
import {
  defaultLayout,
  isLayoutState,
  peeksOf,
  type FrameState,
  type GroupState,
  type LayoutState,
  type PendingCall,
  type RecordedCall,
  type TabState,
} from './types'

// The persisted atoms, plus the focus chain every tool resolves against. This is
// the whole of the app's latent state; anything not reachable from here is either
// derived (./derive) or a runtime cache (./query, ./code, ./ui).

/**
 * `.v3` because the shape changed (frames typed and chained through `below`,
 * peeks always a tab). An older blob simply fails validation and the default
 * layout is used.
 */
export const layoutAtom = persistentAtom<LayoutState>(
  'entity-graph.layout.v3',
  defaultLayout(),
  isLayoutState,
)

// Only pinned peeks outlive the window: a transient one was being hovered, and
// nothing is hovering it now. Handed back untouched when there is nothing to drop.
layoutAtom.set(settlePeeks)

/** The one call currently being built up, if any. */
export const pendingAtom = persistentAtom<PendingCall | null>('entity-graph.pending', null)

/** Recorded calls — running, cancelled, succeeded, failed — newest first. */
export const callsAtom = persistentAtom<RecordedCall[]>('entity-graph.calls', [], (v) =>
  Array.isArray(v),
)

// A call still marked running in the persisted log was running when the window
// closed, and nothing is waiting on it now: whatever the tool went on to do, this
// window will never hear the answer. Left alone it would say "Running" forever, so
// it is settled here, on the way in. The list is handed back untouched when there
// is nothing to fix, which is every load but the rare one — an atom compares by
// identity, and a needless write here would rewrite the whole log at startup.
callsAtom.set((list) =>
  list.some((call) => call.outcome.kind === 'running')
    ? list.map((call) =>
        call.outcome.kind === 'running'
          ? { ...call, outcome: { kind: 'error', message: 'Interrupted — the app restarted while it ran' } }
          : call,
      )
    : list,
)

/**
 * The ids of the calls running right now. Apart from the log, and runtime only,
 * because it answers a different question: the log keeps what is *worth keeping*,
 * which is almost none of the calls a gesture makes, while anything that pressed
 * a button wants to know whether that press is still going — however unremarkable
 * the call behind it. A window that closes mid-call takes this with it, which is
 * right: nothing is waiting on those calls any more.
 */
export const runningCallsAtom = atom<readonly string[]>([])

export const getLayout = (): LayoutState => layoutAtom.get()

/** Apply a pure reducer from ./reducers to the layout. */
export const updateLayout = (fn: (s: LayoutState) => LayoutState): void => layoutAtom.set(fn)

// --- Focus ------------------------------------------------------------------

/**
 * What the user is acting on: the selected group, its active tab, and that
 * tab's top frame. Derived, never stored — the selected group's *id* is the only
 * part that is latent.
 *
 * Unless a peek has the keys: then it is that peek's tab and frame, and no group
 * at all, since a peek belongs to none — so the group's tools (the next tab, a
 * frame popped into a new one) have nothing to act on rather than acting on a
 * group the keys are not in.
 */
export interface Focus {
  groupId: string | null
  tabId: string | null
  frameId: string | null
}

export function focusOf(s: LayoutState): Focus {
  const peek = s.peekFocus ? peeksOf(s).find((p) => p.key === s.peekFocus) : null
  const peekTab = peek?.tabId ? s.tabs[peek.tabId] : null
  if (peekTab) return { groupId: null, tabId: peekTab.id, frameId: peekTab.top }
  const groupId = mainGroupId(s)
  const group = groupId ? s.groups[groupId] : null
  const tabId = group?.activeTabId ?? null
  const tab = tabId ? s.tabs[tabId] : null
  const frameId = tab?.top ?? null
  return { groupId, tabId, frameId }
}

export const focus = (): Focus => focusOf(getLayout())

/**
 * The group the main view's keys go to, whether or not a peek has them now —
 * where something opened from a peek lands.
 */
export const mainGroupId = (s: LayoutState): string | null =>
  (s.selectedGroupId && s.groups[s.selectedGroupId] ? s.selectedGroupId : s.groupOrder[0]) ?? null

export function focusedFrame(s: LayoutState = getLayout()): FrameState | null {
  const { frameId } = focusOf(s)
  return frameId ? (s.frames[frameId] ?? null) : null
}

export function focusedTab(s: LayoutState = getLayout()): TabState | null {
  const { tabId } = focusOf(s)
  return tabId ? (s.tabs[tabId] ?? null) : null
}

/** Groups in column order, skipping any dangling id. */
export const orderedGroups = (s: LayoutState): GroupState[] =>
  s.groupOrder.map((id) => s.groups[id]).filter(Boolean)

/** The selected group's index — the derived counterpart to its latent id. */
export const selectedGroupIndex = (s: LayoutState): number =>
  s.groupOrder.indexOf(focusOf(s).groupId ?? '')
