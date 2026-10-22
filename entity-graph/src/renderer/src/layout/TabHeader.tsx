import React from 'react'
import { ChevronRight, X } from '@untitledui/icons'
import { EntityPill, PillContent, PillWrapper } from '../components/EntityPill'
import { Button } from '../components/ui/Button'
import * as A from '../state/actions'
import { URL_PREFIX } from '../../../core/builtins'
import { useCrumbs, useLayoutState } from '../state/hooks'
import type { PeekState } from '../state/types'
import { runTool } from '../tools/call'
import { EntityFrame } from './EntityFrame'

// A tab, drawn: its title in a group's strip, and its content — bar and frame —
// the same in a group and in a peek.

/**
 * A tab is the entity it is showing: a pill on a background of its own — the
 * header's own two buttons, the filled one for the tab you are in and the quiet
 * one for the rest — with a way to close it. The pill brings the entity gestures
 * with it, so a tab can be right-clicked for the tool list or middle-clicked to
 * open the same entity again elsewhere.
 */
export function TabTitle({
  rootId,
  active,
  onSelect,
  onClose,
}: {
  rootId: string | undefined
  active: boolean
  onSelect?: () => void
  /** Absent: no × (a peek has its own). */
  onClose?: () => void
}): React.JSX.Element {
  const tab = (
    <Button
      variant={active ? 'secondary' : 'tertiary'}
      size="sm"
      className="group shrink-0"
      onClick={onSelect}
    >
      {rootId ? <PillContent id={rootId} bounded /> : <span className="text-gray-400">Empty</span>}
      {onClose && (
        <span
          role="button"
          tabIndex={-1}
          className="opacity-0 hover:text-gray-700 group-hover:opacity-100"
          onClick={(e) => {
            e.stopPropagation()
            onClose()
          }}
        >
          <X size={11} />
        </span>
      )}
    </Button>
  )
  return rootId ? (
    <PillWrapper id={rootId} className="shrink-0">
      {tab}
    </PillWrapper>
  ) : (
    tab
  )
}

/**
 * What a tab shows — its bar and its top frame — drawn the same wherever a tab
 * is: under a group's strip, and as the whole of a peek. `peek` is the one
 * difference: a peek's bar also carries Open and its ×, top right.
 *
 * The bar is the way back out: the frames the tab drilled through to get here,
 * each of which pops everything above it when clicked — which the frame history
 * can then undo. Only the ones you came through; where you are now is the frame
 * below it, and the trailing chevron is what says you are inside. Each crumb is
 * a bare pill — no surface, capped in width, and no preview on hover. With
 * nothing to put in it, no bar.
 */
export function TabContent({
  tabId,
  peek,
}: {
  tabId: string
  /** Set for a peek's tab: the bar gains Open and, once pinned, ×. */
  peek?: PeekState
}): React.JSX.Element {
  const layout = useLayoutState()
  const top = layout.tabs[tabId]?.top
  const crumbs = useCrumbs(tabId).slice(0, -1)
  return (
    // Its own background, so the bar is the same grey wherever it is drawn rather
    // than whatever happens to be behind it.
    <div className="flex h-full min-h-0 flex-col bg-gray-50">
      {(crumbs.length > 0 || peek) && (
        <div
          {...{ [TAB_BAR_ATTRIBUTE]: '' }}
          className="flex shrink-0 items-center gap-1 px-3 py-1.5 text-[11px] text-gray-400 select-none"
        >
          <nav className="flex min-w-0 flex-1 items-center gap-1 overflow-x-auto">
            {crumbs.map((crumb) => (
              <React.Fragment key={crumb.frameId}>
                <EntityPill
                  id={crumb.rootId}
                  bare
                  bounded
                  className="shrink-0"
                  onClick={() => A.popToFrame(tabId, crumb.frameId)}
                />
                <ChevronRight size={11} className="shrink-0 text-gray-300" />
              </React.Fragment>
            ))}
          </nav>
          {peek && <PeekControls peek={peek} rootId={top ? layout.frames[top]?.rootId : undefined} />}
        </div>
      )}
      <div className="relative min-h-0 flex-1">
        {top ? (
          <EntityFrame frameId={top} />
        ) : (
          <div className="p-8 text-center text-[13px] text-gray-400">No tab open.</div>
        )}
      </div>
    </div>
  )
}

/** Marks a tab's bar: what a peek is dragged by. */
export const TAB_BAR_ATTRIBUTE = 'data-tab-bar'

/** Open, and × once pinned. Pressing either never starts a drag. */
function PeekControls({ peek, rootId }: { peek: PeekState; rootId: string | undefined }): React.JSX.Element {
  const open = (): void => {
    const entityId = rootId ?? peek.entityId
    // A web page nobody has written anything about opens where pages open.
    if (entityId.startsWith(URL_PREFIX)) {
      runTool('link.open', { extra: { url: entityId.slice(URL_PREFIX.length) } })
    } else runTool('entity.openInTab', { extra: { entityId } })
  }
  return (
    <span className="flex shrink-0 items-center gap-2" onPointerDown={(e) => e.stopPropagation()}>
      <button className="hover:text-gray-700 focus:outline-none focus-visible:text-gray-700" onClick={open}>
        Open
      </button>
      {peek.pinned && (
        <button
          className="hover:text-gray-700 focus:outline-none focus-visible:text-gray-700"
          title="Close"
          aria-label="Close"
          onClick={() => A.closePeek(peek.key)}
        >
          <X size={12} />
        </button>
      )}
    </span>
  )
}
