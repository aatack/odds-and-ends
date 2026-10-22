import { v4 as uuid } from 'uuid'
import {
  last,
  newTab,
  peeksOf,
  stackOf,
  type LayoutState,
  type PeekState,
  type Rect,
} from './types'

// Peeks, as pure LayoutState → LayoutState transforms. A peek is opened by
// hovering, so most of what happens to one is about which of them the pointer is
// still in or under: a transient peek lasts exactly as long as something keeps it
// — the pointer, the keys, or a peek stacked on it — and a pinned one until its ×.
//
// A peek's frames live in `tabs` and `frames` like anyone's, under a tab that
// belongs to no group, so closing a peek has to take them with it.

/** A peek's size until it is resized. */
export const PEEK_SIZE = { width: 720, height: 480 }

/**
 * Open a peek on purpose rather than by hovering: pinned at `rect`, since no
 * pointer is keeping it, stacked on the peek holding the keys, and given them.
 */
export function openPeek(s: LayoutState, entityId: string, rect: Rect): LayoutState {
  const shown = showPeek(s, entityId, rect, s.peekFocus ?? null)
  const peek = peeksOf(shown)[peeksOf(shown).length - 1]
  return focusPeek(placePeek(shown, peek.key, rect), peek.key)
}

/** The newest peek that isn't pinned — the one a hover would replace. */
export const transientPeek = (s: LayoutState): PeekState | null =>
  [...peeksOf(s)].reverse().find((p) => !p.pinned) ?? null

/** A peek and every peek it was opened from, by key. */
export function peekChain(s: LayoutState, key: string | null | undefined): Set<string> {
  const chain = new Set<string>()
  const peeks = peeksOf(s)
  let current = key ?? null
  while (current && !chain.has(current)) {
    chain.add(current)
    current = peeks.find((p) => p.key === current)?.parent ?? null
  }
  return chain
}

/** Keep only the peeks `keep` says, dropping the tabs and frames of the rest. */
function keepPeeks(s: LayoutState, keep: (peek: PeekState) => boolean): LayoutState {
  const peeks = peeksOf(s)
  const gone = peeks.filter((p) => !keep(p))
  if (!gone.length) return s
  const tabs = { ...s.tabs }
  const frames = { ...s.frames }
  for (const peek of gone) {
    const tab = tabs[peek.tabId]
    if (!tab) continue
    for (const id of [...stackOf(s, tab.id), ...tab.history]) delete frames[id]
    delete tabs[tab.id]
  }
  const kept = peeks.filter(keep)
  const peekFocus = kept.some((p) => p.key === s.peekFocus) ? s.peekFocus : null
  return { ...s, tabs, frames, peeks: kept, peekFocus }
}

/**
 * Open a transient peek, stacked on the one it came from. Every other transient
 * peek goes, unless it is under this one, or under the peek holding the keys.
 */
export function showPeek(
  s: LayoutState,
  entityId: string,
  anchor: Rect,
  parent: string | null,
): LayoutState {
  const keep = peekChain(s, parent)
  for (const key of peekChain(s, s.peekFocus)) keep.add(key)
  const pruned = keepPeeks(s, (p) => p.pinned || keep.has(p.key))
  const key = uuid()
  const { tab, frame } = newTab(entityId)
  const peek: PeekState = { key, entityId, tabId: tab.id, anchor, rect: null, pinned: false, parent }
  return {
    ...pruned,
    tabs: { ...pruned.tabs, [tab.id]: tab },
    frames: { ...pruned.frames, [frame.id]: frame },
    peeks: [...peeksOf(pruned), peek],
  }
}

/**
 * Close the transient peeks the pointer is no longer in or under (`hovering` is
 * the peek it is in, or null for the main view). One holding the keys stays
 * until they leave it.
 */
export function keepHovered(s: LayoutState, hovering: string | null): LayoutState {
  const keep = peekChain(s, hovering)
  for (const key of peekChain(s, s.peekFocus)) keep.add(key)
  return keepPeeks(s, (p) => p.pinned || keep.has(p.key))
}

/** Close one peek, and the transient ones stacked on it. Null closes the top transient one. */
export function closePeek(s: LayoutState, key: string | null): LayoutState {
  const target = key ?? transientPeek(s)?.key
  if (!target) return s
  return keepPeeks(s, (p) => p.key !== target && !(!p.pinned && peekChain(s, p.key).has(target)))
}

/** Moving or resizing pins a peek and brings it to the front. */
export function placePeek(s: LayoutState, key: string, rect: Rect): LayoutState {
  const peek = peeksOf(s).find((p) => p.key === key)
  if (!peek) return s
  return { ...s, peeks: [...peeksOf(s).filter((p) => p !== peek), { ...peek, rect, pinned: true }] }
}

export function raisePeek(s: LayoutState, key: string): LayoutState {
  const peeks = peeksOf(s)
  const peek = peeks.find((p) => p.key === key)
  if (!peek || last(peeks) === peek) return s
  return { ...s, peeks: [...peeks.filter((p) => p !== peek), peek] }
}

/** Give the keys to a peek, or back to the main view (null). */
export function focusPeek(s: LayoutState, key: string | null): LayoutState {
  const next = key && peeksOf(s).some((p) => p.key === key) ? key : null
  return (s.peekFocus ?? null) === next ? s : { ...s, peekFocus: next }
}

/** What survives a reload: the pinned peeks, with the keys back in the main view. */
export const settlePeeks = (s: LayoutState): LayoutState => {
  const settled = keepPeeks(s, (p) => p.pinned)
  return settled.peekFocus ? { ...settled, peekFocus: null } : settled
}
