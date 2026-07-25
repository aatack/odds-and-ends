import React from 'react'
import { ChevronRight, Plus, X } from '@untitledui/icons'
import { EntityPill, PillContent, PillWrapper } from '../components/EntityPill'
import { Button } from '../components/ui/Button'
import { cn } from '../helpers/cn'
import * as A from '../state/actions'
import { tabRootId } from '../state/derive'
import { useCrumbs, useLayoutState } from '../state/hooks'
import { last, type GroupState } from '../state/types'
import { EntityFrame } from './EntityFrame'

/**
 * One column of the layout: a tab strip plus the active tab's top frame. Groups
 * split the screen evenly; the selected one carries a subtle ring.
 */
export function TabGroupView({
  group,
  selected,
}: {
  group: GroupState
  selected: boolean
}): React.JSX.Element {
  const layout = useLayoutState()
  const activeTab = group.activeTabId ? layout.tabs[group.activeTabId] : null
  const topFrameId = activeTab ? last(activeTab.frameIds) : undefined

  return (
    <section
      className={cn(
        'flex min-w-0 flex-1 flex-col border-r border-gray-100',
        selected && 'ring-1 ring-inset ring-brand-200',
      )}
      onMouseDown={() => A.selectGroup(group.id)}
    >
      <div className="flex items-center gap-0.5 overflow-x-auto border-b border-gray-100 bg-white px-1 py-1">
        {group.tabIds
          .filter((id) => layout.tabs[id])
          .map((tabId) => (
            <TabButton
              key={tabId}
              rootId={tabRootId(layout, tabId)}
              active={group.activeTabId === tabId}
              onSelect={() => A.selectTab(group.id, tabId)}
              onClose={() => A.closeTab(group.id, tabId)}
            />
          ))}
        <button
          className="shrink-0 rounded p-1 text-gray-400 hover:bg-gray-50 hover:text-gray-700 focus:outline-none"
          onClick={() => A.addTab(group.id)}
          title="New tab"
        >
          <Plus size={13} />
        </button>
      </div>

      {activeTab && <Breadcrumb tabId={activeTab.id} />}

      <div className="min-h-0 flex-1">
        {topFrameId ? (
          <EntityFrame frameId={topFrameId} />
        ) : (
          <div className="p-8 text-center text-[13px] text-gray-400">No tab open.</div>
        )}
      </div>
    </section>
  )
}

/**
 * The way back out: the frames the tab drilled through to get here, each of which
 * pops everything above it when clicked — which the frame history can then undo.
 * Only the ones you came through. Where you are now is the tree below it and the
 * tab above it, so a crumb for it would say the same thing a third time and be the
 * one crumb with nothing to click; the trailing chevron is what says you are
 * inside. Absent with a single frame on the stack, where there is no way back.
 */
function Breadcrumb({ tabId }: { tabId: string }): React.JSX.Element | null {
  const crumbs = useCrumbs(tabId).slice(0, -1)
  if (!crumbs.length) return null
  return (
    <nav className="flex items-center gap-1 overflow-x-auto px-3 py-1.5 text-[11px] text-gray-400">
      {crumbs.map((crumb) => (
        <React.Fragment key={crumb.frameId}>
          <EntityPill
            id={crumb.rootId}
            className="shrink-0"
            onClick={() => A.popToFrame(tabId, crumb.frameId)}
            title={`Back to ${crumb.label}`}
          />
          <ChevronRight size={11} className="shrink-0 text-gray-300" />
        </React.Fragment>
      ))}
    </nav>
  )
}

/**
 * A tab is the entity it is showing: a pill on a background of its own — the
 * header's own two buttons, the filled one for the tab you are in and the quiet
 * one for the rest — with a way to close it. The pill brings the entity gestures
 * with it, so a tab can be right-clicked for the tool list or middle-clicked to
 * open the same entity again elsewhere.
 */
function TabButton({
  rootId,
  active,
  onSelect,
  onClose,
}: {
  rootId: string | undefined
  active: boolean
  onSelect: () => void
  onClose: () => void
}): React.JSX.Element {
  const tab = (
    <Button
      variant={active ? 'secondary' : 'tertiary'}
      size="sm"
      className="group shrink-0"
      onClick={onSelect}
    >
      {rootId ? <PillContent id={rootId} /> : <span className="text-gray-400">Empty</span>}
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
