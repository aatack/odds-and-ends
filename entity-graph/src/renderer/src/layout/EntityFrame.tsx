import React, { useRef } from 'react'
import { SearchSm, X } from '@untitledui/icons'
import { Editor } from '../views/Editor'
import { codeRunsAtom, runCode, stopCode } from '../helpers/codeRunner'
import * as A from '../state/actions'
import { findField } from '../state/focusRequest'
import { useAtomValue, useFocusRequest, useFrameRows, useLayoutState } from '../state/hooks'
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
    <div className="relative flex h-full min-h-0 flex-col">
      {frame.find != null && (
        <FindBox
          frameId={frameId}
          text={frame.find}
          onChange={(text) => A.setFind(frameId, text)}
          onClear={() => A.setFind(frameId, null)}
        />
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

/**
 * The find field, floating over the frame's top-right corner the way the toasts
 * float over its bottom-right — the rows keep their full height, and the field
 * doesn't push them down the moment it opens.
 *
 * It holds no state: `frame.find` is the box, so it is on screen exactly while
 * the frame is filtering, empty string and all. Escape isn't handled here — it
 * routes like every other key, so it only reaches `frame.find.clear` when there
 * is nothing more pressing for it to do.
 */
function FindBox({
  frameId,
  text,
  onChange,
  onClear,
}: {
  frameId: string
  text: string
  onChange: (text: string) => void
  onClear: () => void
}): React.JSX.Element {
  const inputRef = useRef<HTMLInputElement>(null)

  // Taken on mount when the tool has just opened the field, and again each time
  // the tool is run against a field already open. Selecting rather than merely
  // focusing means a second ⌘F types over the old filter, as it would in a
  // browser, while leaving it there to be kept.
  useFocusRequest(findField(frameId), () => inputRef.current?.select())

  return (
    <div className="absolute right-3 top-3 z-20 flex w-64 items-center gap-2 rounded-lg bg-white px-3 py-2 shadow-lg">
      <SearchSm size={13} className="shrink-0 text-gray-400" />
      <input
        ref={inputRef}
        value={text}
        onChange={(e) => onChange(e.target.value)}
        // Enter hands the keyboard back: the field owns bare keys while it has
        // focus, so navigation is dead until something lets go of them. The
        // filter stays — it is written through as you type, not on submit.
        onKeyDown={(e) => {
          if (e.key === 'Enter') {
            e.preventDefault()
            inputRef.current?.blur()
          }
        }}
        placeholder="Filter rows…"
        // Serif, like the rows it is matched against.
        className="min-w-0 flex-1 bg-transparent font-serif text-[13px] text-gray-900 outline-none placeholder:font-sans placeholder:text-gray-400"
      />
      <button
        className="shrink-0 text-gray-400 hover:text-gray-700 focus:outline-none"
        onClick={onClear}
        aria-label="Clear find"
      >
        <X size={13} />
      </button>
    </div>
  )
}
