import React from 'react'
import { X } from '@untitledui/icons'
import { Editor } from '../views/Editor'
import { codeRunsAtom, runCode, stopCode } from '../helpers/codeRunner'
import * as A from '../state/actions'
import { useAtomValue, useFrameRows, useLayoutState } from '../state/hooks'
import { loadMore } from '../state/query'
import { runTool } from '../tools/call'

/**
 * One entity view: the tree rooted at the frame's root entity. It owns no state
 * — rows are derived from the frame plus the query cache, and every gesture goes
 * to a state action or a tool.
 */
export function EntityFrame({ frameId }: { frameId: string }): React.JSX.Element {
  const layout = useLayoutState()
  const { rows, loading } = useFrameRows(frameId)
  const codeRuns = useAtomValue(codeRunsAtom)
  const frame = layout.frames[frameId]

  if (!frame) return <div className="p-8 text-center text-[13px] text-gray-400">No frame.</div>

  return (
    <div className="flex h-full min-h-0 flex-col">
      {frame.find != null && (
        <div className="flex items-center gap-2 border-b border-gray-100 bg-gray-50 px-3 py-1.5">
          <span className="text-[11px] font-medium uppercase tracking-wide text-gray-400">Find</span>
          <span className="min-w-0 flex-1 truncate font-serif text-[13px] text-gray-700">
            {frame.find || '—'}
          </span>
          <button
            className="text-gray-400 hover:text-gray-700 focus:outline-none"
            onClick={() => A.setFind(frameId, null)}
            aria-label="Clear find"
          >
            <X size={13} />
          </button>
        </div>
      )}
      <div className="min-h-0 flex-1">
        <Editor
          rows={rows}
          loading={loading}
          onSelectRow={(path) => A.selectPath(frameId, path)}
          onToggleCollapse={(row) => {
            A.selectPath(frameId, row.path)
            A.toggleCollapse(frame.tabId, row.id)
          }}
          onDraft={(text) => A.setDraft(frameId, text)}
          // Explicitly against *this* frame: a blur can arrive after focus has
          // already moved to another group.
          onCommit={() => runTool('edit.commit', { extra: { frameId } })}
          onCancel={() => A.setEdit(frameId, null)}
          onNearEnd={() => loadMore(frameId)}
          codeRuns={codeRuns}
          onRunCode={runCode}
          onStopCode={stopCode}
        />
      </div>
    </div>
  )
}
