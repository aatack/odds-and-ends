import React from 'react'
import { Plus, X } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import * as A from '../state/actions'
import { useLayoutState, useTabLabel } from '../state/hooks'
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
              tabId={tabId}
              active={group.activeTabId === tabId}
              frameCount={layout.tabs[tabId].frameIds.length}
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

function TabButton({
  tabId,
  active,
  frameCount,
  onSelect,
  onClose,
}: {
  tabId: string
  active: boolean
  frameCount: number
  onSelect: () => void
  onClose: () => void
}): React.JSX.Element {
  const label = useTabLabel(tabId)
  return (
    <button
      className={cn(
        'group flex shrink-0 items-center gap-1 rounded px-2 py-1 text-[12px]',
        active ? 'bg-gray-100 text-gray-900' : 'text-gray-500 hover:bg-gray-50',
      )}
      onClick={onSelect}
    >
      <span className="max-w-[80px] truncate">{label}</span>
      {frameCount > 1 && (
        <span className="text-[10px] text-gray-400" title={`${frameCount} frames on the stack`}>
          {frameCount}
        </span>
      )}
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
    </button>
  )
}
