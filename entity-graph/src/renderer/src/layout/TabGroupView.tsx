import React from 'react'
import { Plus } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import * as A from '../state/actions'
import { tabRootId } from '../state/derive'
import { useLayoutState } from '../state/hooks'
import type { GroupState } from '../state/types'
import { TabContent, TabTitle } from './TabHeader'

/**
 * One column of the layout: a tab strip plus the active tab's content. Groups
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
            <TabTitle
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

      <div className="min-h-0 flex-1">
        {activeTab ? (
          <TabContent tabId={activeTab.id} />
        ) : (
          <div className="p-8 text-center text-[13px] text-gray-400">No tab open.</div>
        )}
      </div>
    </section>
  )
}
