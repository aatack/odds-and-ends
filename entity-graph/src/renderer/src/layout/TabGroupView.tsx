import React from 'react'
import { Plus, X } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import type { EditorActions } from '../views/useEditor'
import { FrameView } from './Frame'
import { last, tabTitle, type Frame, type View } from './types'
import type { ResolvedGroup, ViewHandle } from './useLayout'

export interface TabGroupViewProps {
  rg: ResolvedGroup
  frames: Record<string, Frame>
  names: Record<string, string>
  focused: boolean
  actions: EditorActions
  onDebugEntity: (entityId: string) => void
  onSelectTab: (tabId: string) => void
  onFocus: () => void
  onCloseTab: (tabId: string) => void
  onNewTab: () => void
  registerHandle: (frameId: string, handle: ViewHandle | null) => void
  pushEntityFrame: (tabId: string, entityId: string) => void
  updateView: (frameId: string, view: View) => void
  reportName: (id: string, text: string | undefined) => void
}

/**
 * One column of the layout: a tab strip plus the active tab's top frame. Groups
 * split the screen evenly (flex-1); the focused one carries a subtle ring.
 */
export function TabGroupView({
  rg,
  frames,
  names,
  focused,
  actions,
  onDebugEntity,
  onSelectTab,
  onFocus,
  onCloseTab,
  onNewTab,
  registerHandle,
  pushEntityFrame,
  updateView,
  reportName,
}: TabGroupViewProps): React.JSX.Element {
  return (
    <section
      className={cn(
        'flex min-w-0 flex-1 flex-col border-r border-gray-100',
        focused && 'ring-1 ring-inset ring-brand-200',
      )}
      onMouseDown={onFocus}
    >
      <div className="flex items-center gap-0.5 overflow-x-auto border-b border-gray-100 bg-white px-1 py-1">
        {rg.tabs.map((tab) => {
          const top = frames[last(tab.frameIds) ?? '']
          const active = rg.group.activeTabId === tab.id
          return (
            <button
              key={tab.id}
              className={cn(
                'group flex shrink-0 items-center gap-1 rounded px-2 py-1 text-[12px]',
                active ? 'bg-gray-100 text-gray-900' : 'text-gray-500 hover:bg-gray-50',
              )}
              onClick={() => onSelectTab(tab.id)}
            >
              <span className="max-w-[80px] truncate">
                {top ? tabTitle(top.view, names) : 'Empty'}
              </span>
              {tab.frameIds.length > 1 && (
                <span
                  className="text-[10px] text-gray-400"
                  title={`${tab.frameIds.length} frames on the stack`}
                >
                  {tab.frameIds.length}
                </span>
              )}
              <span
                role="button"
                tabIndex={-1}
                className="opacity-0 hover:text-gray-700 group-hover:opacity-100"
                onClick={(e) => {
                  e.stopPropagation()
                  onCloseTab(tab.id)
                }}
              >
                <X size={11} />
              </span>
            </button>
          )
        })}
        <button
          className="shrink-0 rounded p-1 text-gray-400 hover:bg-gray-50 hover:text-gray-700 focus:outline-none"
          onClick={onNewTab}
          title="New tab"
        >
          <Plus size={13} />
        </button>
      </div>

      <div className="min-h-0 flex-1">
        {rg.topFrame ? (
          <FrameView
            frame={rg.topFrame}
            actions={actions}
            onDebugEntity={onDebugEntity}
            registerHandle={registerHandle}
            pushEntityFrame={pushEntityFrame}
            updateView={updateView}
            reportName={reportName}
          />
        ) : (
          <div className="p-8 text-center text-[13px] text-gray-400">No tab open.</div>
        )}
      </div>
    </section>
  )
}
