import React, { useRef } from 'react'
import { ChevronLeft, ChevronRight, SearchSm, X } from '@untitledui/icons'
import { Editor } from '../views/Editor'
import { cn } from '../helpers/cn'
import { codeRunsAtom, runCode, stopCode } from '../helpers/codeRunner'
import * as A from '../state/actions'
import { findField } from '../state/focusRequest'
import { useAtomValue, useFocusRequest, useFrameRows, useLayoutState } from '../state/hooks'
import { loadMore } from '../state/query'
import { depthLabel, directionOf, frameDepth, nudgeDepth } from '../state/types'
import { contextWithin, runTool } from '../tools/call'

/**
 * One entity view: the tree rooted at the frame's root entity. It owns no state
 * — rows are derived from the frame plus the query cache, and every gesture goes
 * to a state action or a tool.
 */
export function EntityFrame({ frameId }: { frameId: string }): React.JSX.Element {
  const layout = useLayoutState()
  const { rows, keys, selectedIndex, editIndex, loading, complete } = useFrameRows(frameId)
  const codeRuns = useAtomValue(codeRunsAtom)
  const frame = layout.frames[frameId]

  if (!frame) return <div className="p-8 text-center text-[13px] text-gray-400">No frame.</div>

  const depth = frameDepth(frame)

  return (
    <div className="relative flex h-full min-h-0 flex-col">
      {/* Whatever the frame is doing to its rows, floating over its top-right
          corner the way the toasts float over its bottom-right: the rows keep
          their full height and nothing is pushed down when a filter opens. */}
      <div className="absolute right-3 top-3 z-20 flex flex-col items-end gap-2">
        {frame.find != null && (
          <FindBox
            frameId={frameId}
            text={frame.find}
            onChange={(text) => A.setFind(frameId, text)}
            onClear={() => A.setFind(frameId, null)}
          />
        )}
        {frame.sectionsOnly && (
          <FramePill label="Sections only" onClear={() => A.setSectionsOnly(frameId, false)} />
        )}
        {frame.openOnly && (
          <FramePill label="Open items only" onClear={() => A.setOpenOnly(frameId, false)} />
        )}
        {depth != null && (
          // A depth is said out loud: a row below it is simply absent, and a
          // limit that hides things without saying so reads as the tree having
          // nothing there. Its own arrows, since a limit is the one thing up
          // here that is a number rather than a state to be left.
          <FramePill
            label={depthLabel(depth)}
            onClear={() => A.setFrameDepth(frameId, null)}
            onLess={() => A.setFrameDepth(frameId, nudgeDepth(depth, -1))}
            onMore={() => A.setFrameDepth(frameId, nudgeDepth(depth, 1))}
          />
        )}
        {directionOf(frame) === 'in' && (
          <FramePill label="Inbound links" onClear={() => A.setDirection(frameId, 'out')} />
        )}
      </div>
      <div className="min-h-0 flex-1">
        <Editor
          rows={rows}
          keys={keys}
          selectedIndex={selectedIndex}
          editIndex={editIndex}
          loading={loading}
          // What the frame is filtering on, marked in the rows it kept. Null while
          // there is no field open, which is most of the time.
          highlight={frame.find ?? undefined}
          onSelectRow={(path) => A.selectPath(frameId, path)}
          onToggleCollapse={(row) => {
            A.selectPath(frameId, row.path)
            A.toggleCollapse(frame.tabId, row.id)
          }}
          onDraft={(text) => A.setDraft(frameId, text)}
          // Explicitly against *this* frame: a blur can arrive after focus has
          // already moved to another group.
          onCommit={() => runTool('edit.commit', { extra: { frameId } })}
          onNearEnd={() => loadMore(frameId)}
          complete={complete}
          codeRuns={codeRuns}
          // The run button is aimed at the row it sits on, which needn't be the
          // selected one, so the script's context is folded along that row's path.
          onRunCode={(id, code, path) => runCode(id, code, contextWithin(path))}
          onStopCode={stopCode}
        />
      </div>
    </div>
  )
}

/**
 * Something the frame is doing that has little or nothing to configure — a
 * filter with no text, the query running backwards, a depth: it says what is
 * going on and offers to stop, and where there is a dial it wears the two
 * arrows for it. Like the find field's own clear button, working one is direct
 * manipulation — a state action, not a call through the tool machine.
 */
function FramePill({
  label,
  onClear,
  onLess,
  onMore,
}: {
  label: string
  onClear: () => void
  onLess?: () => void
  onMore?: () => void
}): React.JSX.Element {
  return (
    <div
      className={cn(
        'flex items-center gap-1.5 rounded-full bg-white py-1 pr-2 text-[12px] text-gray-500 shadow-lg',
        onLess ? 'pl-1' : 'pl-3',
      )}
    >
      {onLess && <PillButton icon={ChevronLeft} label={`Less: ${label}`} onClick={onLess} />}
      <span>{label}</span>
      {onMore && <PillButton icon={ChevronRight} label={`More: ${label}`} onClick={onMore} />}
      <PillButton icon={X} label={`Stop: ${label}`} onClick={onClear} />
    </div>
  )
}

function PillButton({
  icon: Icon,
  label,
  onClick,
}: {
  icon: typeof X
  label: string
  onClick: () => void
}): React.JSX.Element {
  return (
    <button
      className="text-gray-400 hover:text-gray-700 focus:outline-none"
      onClick={onClick}
      aria-label={label}
    >
      <Icon size={11} />
    </button>
  )
}

/**
 * The find field. It holds no state: `frame.find` is the box, so it is on screen
 * exactly while the frame is filtering, empty string and all. Escape isn't
 * handled here — it routes like every other key, so it only reaches
 * `frame.find.clear` when there is nothing more pressing for it to do.
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
    <div className="flex w-64 items-center gap-2 rounded-lg bg-white px-3 py-2 shadow-lg">
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
