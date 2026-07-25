import React, { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'
import { Check, ChevronDown, ChevronRight, Play, Square, Stop } from '@untitledui/icons'
import { TextEditor } from '../components/ui/TextEditor'
import { CodeBlock } from '../components/ui/CodeBlock'
import { cn } from '../helpers/cn'
import type { CodeRunState } from '../helpers/codeRunner'
import type { EntityRow, Row } from '../state/derive'

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const INDENT = 20 // px per depth level
const OVERSCAN = 8 // extra rows rendered above/below the viewport
const ESTIMATE = 24 // assumed height of a not-yet-measured row, in px

// User-written entity text — serif, matching the orchestrator's prose voice. It
// wraps to show the whole value rather than truncating. `block` keeps the
// editing textarea the same height as the static line (an inline-block textarea
// leaves a descender gap below it) so pressing `e` doesn't shift the layout.
// Colour is applied per-use so collapsed rows can read as muted.
const TEXT = 'block font-serif text-[14px] leading-5'
// Monospace counterpart for editing `type: code` entities in place.
const MONO = 'block font-mono text-[12.5px] leading-5'

const keyOf = (row: Row, index: number): string =>
  row.kind === 'entity' ? row.path.join('/') : `input-${index}`

// Section headings grow with prominence: ~1.5x at the top level, easing toward
// ~1.1x as they nest deeper. Per-depth, so it can't be a static utility class.
// Applied to the editing box as well as the static line, so a section reads as
// one while it is being typed.
const sectionStyle = (depth: number): React.CSSProperties => ({
  fontSize: `${14 * Math.max(1.1, 1.5 - depth * 0.1)}px`,
  lineHeight: 1.3,
})

/**
 * The glyph in front of a row: a checkbox when the row has one, otherwise a
 * plain bullet. Shared with the create input, which shows the same mark as the
 * row it is about to become.
 */
function Mark({ open }: { open?: boolean }): React.JSX.Element {
  if (open === true) return <Square size={13} />
  if (open === false) return <Check size={13} />
  return <span className="size-1 rounded-full bg-gray-300" />
}

// ---------------------------------------------------------------------------
// Dumb rendering component
// ---------------------------------------------------------------------------

export interface EditorProps {
  rows: Row[]
  loading: boolean
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  /** Every keystroke of an in-place edit; the draft is part of the frame's state. */
  onDraft: (text: string) => void
  /** Write the draft (Enter or blur). */
  onCommit: () => void
  /** Abandon the edit (Escape). */
  onCancel: () => void
  onNearEnd: () => void
  /** Run state for code entities, keyed by entity id. */
  codeRuns: Record<string, CodeRunState>
  onRunCode: (id: string, code: string) => void
  onStopCode: () => void
}

/**
 * Pure presentation: renders the flat row list as an indented, windowed set of
 * wrapping bullets and forwards interaction to the callbacks it is given. Rows
 * are variable-height — each measures itself so multi-line entities show in
 * full — and only the slice around the viewport is mounted.
 */
export function Editor(props: EditorProps): React.JSX.Element {
  const {
    rows,
    loading,
    onSelectRow,
    onToggleCollapse,
    onDraft,
    onCommit,
    onCancel,
    onNearEnd,
    codeRuns,
    onRunCode,
    onStopCode,
  } = props

  const containerRef = useRef<HTMLDivElement>(null)
  const [scrollTop, setScrollTop] = useState(0)
  // The scroll container fills its parent, so its height is measured rather
  // than fixed; it drives how many rows the window renders.
  const [viewportH, setViewportH] = useState(0)
  // Measured heights keyed by row identity, so offsets survive reloads without
  // re-measuring and unknown rows fall back to ESTIMATE.
  const [heights, setHeights] = useState<Map<string, number>>(new Map())

  const setHeight = useCallback((key: string, h: number) => {
    setHeights((prev) => {
      if (prev.get(key) === h) return prev
      const next = new Map(prev)
      next.set(key, h)
      return next
    })
  }, [])

  useEffect(() => {
    const el = containerRef.current
    if (!el) return
    const update = (): void => setViewportH(el.clientHeight)
    update()
    const ro = new ResizeObserver(update)
    ro.observe(el)
    return () => ro.disconnect()
  }, [])

  // Cumulative offsets (offsets[i] = top of row i; offsets[n] = total height).
  const offsets = useMemo(() => {
    const out = new Array<number>(rows.length + 1)
    let acc = 0
    for (let i = 0; i < rows.length; i++) {
      out[i] = acc
      acc += heights.get(keyOf(rows[i], i)) ?? ESTIMATE
    }
    out[rows.length] = acc
    return out
  }, [rows, heights])
  const total = offsets[rows.length]

  // Keep the selected row within the viewport as selection moves. Reads the
  // latest offsets via a ref so height changes don't fight the user's scroll.
  const offsetsRef = useRef(offsets)
  offsetsRef.current = offsets
  const selectedIndex = useMemo(
    () => rows.findIndex((r) => r.kind === 'entity' && r.selected),
    [rows],
  )
  useEffect(() => {
    const el = containerRef.current
    if (!el || selectedIndex < 0) return
    const o = offsetsRef.current
    const top = o[selectedIndex]
    const bottom = o[selectedIndex + 1]
    if (top < el.scrollTop) el.scrollTop = top
    else if (bottom > el.scrollTop + el.clientHeight) el.scrollTop = bottom - el.clientHeight
  }, [selectedIndex])

  // The windowed slice: walk offsets to the first row reaching the viewport top
  // and the first past its bottom, padded by OVERSCAN.
  const bottomEdge = scrollTop + (viewportH || 600)
  let firstIndex = 0
  while (firstIndex < rows.length && offsets[firstIndex + 1] <= scrollTop) firstIndex++
  firstIndex = Math.max(0, firstIndex - OVERSCAN)
  let lastIndex = firstIndex
  while (lastIndex < rows.length && offsets[lastIndex] < bottomEdge) lastIndex++
  lastIndex = Math.min(rows.length, lastIndex + OVERSCAN)
  const slice = rows.slice(firstIndex, lastIndex)

  const handleScroll = (e: React.UIEvent<HTMLDivElement>): void => {
    const el = e.currentTarget
    setScrollTop(el.scrollTop)
    if (el.scrollTop + el.clientHeight >= el.scrollHeight - ESTIMATE * OVERSCAN) onNearEnd()
  }

  return (
    <div className="flex h-full flex-col overflow-hidden bg-white">
      <div
        ref={containerRef}
        onScroll={handleScroll}
        className="relative min-h-0 flex-1 overflow-y-auto py-1"
        // Room to scroll past the last row when the tree overflows, so appending
        // at the bottom isn't jammed against the edge of the screen.
        style={{ paddingBottom: total > viewportH ? '40vh' : undefined }}
      >
        {rows.length === 0 ? (
          <div className="px-4 py-8 text-center text-[13px] text-gray-400">
            {loading ? 'Loading…' : 'No entities.'}
          </div>
        ) : (
          <>
            <div style={{ height: offsets[firstIndex] }} />
            {slice.map((row, i) => {
              const index = firstIndex + i
              const key = keyOf(row, index)
              // Pass this row's run state directly (not the whole map) so the
              // memoised Row only re-renders when *its own* run changes.
              const run = row.kind === 'entity' ? codeRuns[row.id] : undefined
              return (
                <RowView
                  key={key}
                  row={row}
                  measureKey={key}
                  run={run}
                  onMeasure={setHeight}
                  onSelectRow={onSelectRow}
                  onToggleCollapse={onToggleCollapse}
                  onDraft={onDraft}
                  onCommit={onCommit}
                  onCancel={onCancel}
                  onRunCode={onRunCode}
                  onStopCode={onStopCode}
                />
              )
            })}
            <div style={{ height: total - offsets[lastIndex] }} />
          </>
        )}
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Row — memoised so scrolling only re-renders rows that actually change
// ---------------------------------------------------------------------------

interface RowProps {
  row: Row
  measureKey: string
  run?: CodeRunState
  onMeasure: (key: string, height: number) => void
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  onDraft: (text: string) => void
  onCommit: () => void
  onCancel: () => void
  onRunCode: (id: string, code: string) => void
  onStopCode: () => void
}

const RowView = React.memo(function RowView({
  row,
  measureKey,
  run,
  onMeasure,
  onSelectRow,
  onToggleCollapse,
  onDraft,
  onCommit,
  onCancel,
  onRunCode,
  onStopCode,
}: RowProps): React.JSX.Element {
  const ref = useRef<HTMLDivElement>(null)

  // Report our height so the parent can lay out the window; a ResizeObserver
  // keeps it current as text wraps or the in-place editor grows.
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    const report = (): void => onMeasure(measureKey, el.offsetHeight)
    report()
    const ro = new ResizeObserver(report)
    ro.observe(el)
    return () => ro.disconnect()
  }, [measureKey, onMeasure])

  // Enter commits, Escape abandons. Both are handled here rather than left to
  // the key router, since the textarea owns bare keys while it has focus.
  const onKeyDown = (e: React.KeyboardEvent<HTMLTextAreaElement>): void => {
    if (e.key === 'Escape') {
      e.preventDefault()
      onCancel()
    } else if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault()
      onCommit()
    }
  }

  if (row.kind === 'input') {
    return (
      <div ref={ref} className="flex">
        <div
          className="flex items-start py-0.5 mx-2 pr-2 flex-1 min-w-0"
          style={{ paddingLeft: row.depth * INDENT + 4 }}
        >
          <span className="flex h-5 w-5 shrink-0 items-center justify-center text-gray-400 select-none">
            <Mark open={row.open} />
          </span>
          <div className="flex-1 min-w-0">
            <TextEditor
              autoFocus
              eager
              value={row.draft}
              setValue={onDraft}
              onBlur={onCommit}
              placeholder={row.section ? 'New section…' : 'New entity…'}
              onKeyDown={onKeyDown}
              className={cn(
                row.type === 'code' ? MONO : TEXT,
                'text-gray-900',
                row.section && 'font-semibold',
              )}
              style={row.section ? sectionStyle(row.depth) : undefined}
            />
          </div>
        </div>
      </div>
    )
  }

  const isCode = row.type === 'code'
  const running = run?.status === 'running'
  const heading = row.section ? sectionStyle(row.depth) : undefined

  return (
    <div
      ref={ref}
      className="flex"
      // The data attributes let the global right-click handler seed a call's
      // context; selecting on right-click also lets the selection-based tools
      // act on the row under the cursor.
      data-entity-id={row.id}
      data-parent-id={row.path.length > 1 ? row.path[row.path.length - 2] : undefined}
      onClick={() => onSelectRow(row.path)}
      onContextMenu={() => onSelectRow(row.path)}
    >
      <div
        className={cn(
          'flex items-start my-px py-0.5 mx-2 pr-2 rounded-md flex-1 min-w-0 cursor-default',
          row.selected ? 'bg-blue-100' : 'hover:bg-gray-100/70',
        )}
        style={{ paddingLeft: row.depth * INDENT + 4 }}
      >
        {isCode ? (
          // Code rows swap the chevron/bullet for a run/stop control.
          <span
            className="flex h-5 w-5 shrink-0 items-center justify-center text-gray-400 select-none hover:text-gray-700"
            title={running ? 'Stop' : 'Run'}
            onClick={(e) => {
              e.stopPropagation()
              if (running) onStopCode()
              else onRunCode(row.id, row.text ?? '')
            }}
          >
            {running ? <Stop size={13} /> : <Play size={13} />}
          </span>
        ) : (
          <span
            className="flex h-5 w-5 shrink-0 items-center justify-center text-gray-400 select-none"
            onClick={(e) => {
              e.stopPropagation()
              if (row.hasChildren) onToggleCollapse(row)
            }}
          >
            {/* A checkbox keeps its box even with children; only a plain row
                trades its bullet for a chevron. */}
            {row.open !== undefined || !row.hasChildren ? (
              <Mark open={row.open} />
            ) : row.collapsed ? (
              <ChevronRight size={14} />
            ) : (
              <ChevronDown size={14} />
            )}
          </span>
        )}
        <div className="flex-1 min-w-0">
          {row.editing ? (
            <TextEditor
              autoFocus
              eager
              value={row.draft ?? ''}
              setValue={onDraft}
              onBlur={onCommit}
              onKeyDown={onKeyDown}
              className={cn(isCode ? MONO : TEXT, 'text-gray-900', row.section && 'font-semibold')}
              style={heading}
            />
          ) : isCode ? (
            <CodeBlock code={row.text ?? ''} run={run} />
          ) : row.text ? (
            <span
              className={cn(
                'whitespace-pre-wrap break-words',
                TEXT,
                row.section && 'font-semibold',
                row.hasChildren && row.collapsed ? 'text-gray-400' : 'text-gray-900',
              )}
              style={heading}
            >
              {row.text}
            </span>
          ) : (
            <span className={`${TEXT} italic text-gray-400`}>Empty</span>
          )}
        </div>
      </div>
    </div>
  )
})
