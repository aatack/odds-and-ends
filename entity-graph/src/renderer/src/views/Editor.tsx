import React, { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'
import { Check, ChevronDown, ChevronRight, Play, Square, Stop } from '@untitledui/icons'
import { TextEditor } from '../components/ui/TextEditor'
import { CodeBlock } from '../components/ui/CodeBlock'
import { EntityMarkdown } from '../components/EntityMarkdown'
import { ResourceView } from '../components/Resource'
import { TypePill } from '../components/TypePill'
import { cn } from '../helpers/cn'
import type { CodeRunState } from '../helpers/codeRunner'
import type { EntityRow, Row } from '../state/derive'

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const INDENT = 20 // px per depth level
const OVERSCAN = 8 // extra rows rendered above/below the viewport
const ESTIMATE = 24 // assumed height of a not-yet-measured row, in px
// How far in from the edge a row scrolled into view lands, as a fraction of the
// viewport — so the row that was just selected has context above or below it
// rather than sitting on the boundary.
const MARGIN = 0.3

// User-written entity text — serif, matching the orchestrator's prose voice. It
// wraps to show the whole value rather than truncating. `block` keeps the
// editing textarea the same height as the static line (an inline-block textarea
// leaves a descender gap below it) so pressing `e` doesn't shift the layout.
// Colour is applied per-use so collapsed rows can read as muted.
const TEXT = 'block font-serif text-[14px] leading-5'
// Monospace counterpart for editing `type: code` entities in place.
const MONO = 'block font-mono text-[12.5px] leading-5'

// Row keys are not computed here. They come with the rows, index for index, and
// stay the same while the tree does — which is what lets the measured heights and
// the offsets built from them survive a cursor move untouched.

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
  /** Row keys, index-aligned with `rows`; unchanged while the tree is unchanged. */
  keys: string[]
  /** Which row is selected, and which is being typed into. -1 for neither. */
  selectedIndex: number
  editIndex: number
  loading: boolean
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  /** Every keystroke of an in-place edit; the draft is part of the frame's state. */
  onDraft: (text: string) => void
  /** Write the draft (Enter or blur). */
  onCommit: () => void
  /**
   * There is more of the tree to unroll: the view has scrolled near the end of
   * what it has, or what it has does not reach the bottom of the viewport — the
   * latter being how a filter that keeps three rows out of a page of two hundred
   * still fills the screen.
   */
  onNearEnd: () => void
  /** Run state for code entities, keyed by entity id. */
  codeRuns: Record<string, CodeRunState>
  /** Run a code entity: its id and source, plus the path of the row it sits at. */
  onRunCode: (id: string, code: string, path: string[]) => void
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
    keys,
    selectedIndex,
    editIndex,
    loading,
    onSelectRow,
    onToggleCollapse,
    onDraft,
    onCommit,
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
  // Keyed on `keys` rather than on `rows`: the rows array is new whenever the
  // cursor moves, while the keys are the same until the tree itself changes, so
  // this walk happens when the shape of the list changes and not on every press.
  const offsets = useMemo(() => {
    const out = new Array<number>(keys.length + 1)
    let acc = 0
    for (let i = 0; i < keys.length; i++) {
      out[i] = acc
      acc += heights.get(keys[i]) ?? ESTIMATE
    }
    out[keys.length] = acc
    return out
  }, [keys, heights])
  const total = offsets[keys.length]

  // Keep the selected row within the viewport as selection moves. Reads the
  // latest offsets via a ref so height changes don't fight the user's scroll.
  const offsetsRef = useRef(offsets)
  offsetsRef.current = offsets
  const reveal = useCallback((index: number): void => {
    const el = containerRef.current
    if (!el || index < 0) return
    const o = offsetsRef.current
    const top = o[index]
    const bottom = o[index + 1]
    const height = el.clientHeight
    if (!height) return
    // A row scrolled to isn't pushed flush against the edge: it lands MARGIN in,
    // so there is context on the side it came from. Capped at what's left over
    // once the row itself is accounted for, so a tall row can't overshoot.
    const margin = Math.max(0, Math.min(height * MARGIN, (height - (bottom - top)) / 2))
    if (top < el.scrollTop + margin) el.scrollTop = Math.max(0, top - margin)
    else if (bottom > el.scrollTop + height - margin) el.scrollTop = bottom - height + margin
  }, [])

  // Both indices arrive with the rows rather than being searched for here: the
  // derivation looked them up in its own index, and scanning the whole list twice
  // per keystroke to find what it already knew was most of the cost of a press.
  useEffect(() => reveal(selectedIndex), [selectedIndex, reveal])

  // The row being typed into is kept mounted wherever it is (see the pin below)
  // and brought into view — creating a child of a very tall entity puts the box
  // right at the end of that entity's subtree, pages away.
  //
  // Also on its offset, not just its index: the rows between here and there are
  // mostly unmeasured guesses, so the first scroll lands approximately and the
  // real offset arrives once they mount.
  const editTop = editIndex < 0 ? -1 : offsets[editIndex]
  useEffect(() => {
    if (editIndex >= 0) reveal(editIndex)
  }, [editIndex, editTop, reveal])

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

  // The edited row is pinned: rendered exactly once, positioned at its own
  // offset, for as long as the edit lasts — whether or not the window reaches
  // it. Unmounting it would take the caret with it (autofocus never fires for a
  // box that was never rendered, and scrolling away mid-edit would drop focus
  // and the selection inside the box), and overscan can't help, since the row can
  // be arbitrarily far from the viewport. Moving it between the flow and the pin
  // as it crosses the window edge would remount it just the same, so it stays
  // pinned throughout and the flow leaves a spacer of its height in its place.
  const editKey = editIndex < 0 ? null : keys[editIndex]
  const editHeight = editKey == null ? 0 : (heights.get(editKey) ?? ESTIMATE)

  const handleScroll = (e: React.UIEvent<HTMLDivElement>): void => {
    const el = e.currentTarget
    setScrollTop(el.scrollTop)
    if (el.scrollTop + el.clientHeight >= el.scrollHeight - ESTIMATE * OVERSCAN) onNearEnd()
  }

  // Rows that don't reach the bottom of the viewport ask for more, since there is
  // no scroll to ask on their behalf. This is what a filter needs: the limit is on
  // the *walk*, so a page of two hundred entities can leave three rows standing —
  // a third of a screen with nothing below it and nothing to scroll.
  //
  // Asked at most once per measurement, and `onNearEnd` is deliberately not a
  // dependency: it is a fresh closure on every render, so keying on it fired this
  // on every render, and each firing unrolled another page. A short list and a
  // tree with more to give became a loop that raised the ceiling until it had
  // walked the whole store — leaving the frame permanently resolving thousands of
  // rows, which is far slower than the empty space it was trying to fill.
  const askedAt = useRef('')
  useEffect(() => {
    if (!viewportH || total >= viewportH) return
    const at = `${keys.length}:${viewportH}`
    if (askedAt.current === at) return
    askedAt.current = at
    onNearEnd()
  })

  const renderRow = (row: Row, index: number): React.JSX.Element => {
    const key = keys[index]
    // Pass this row's run state directly (not the whole map) so the memoised
    // RowView only re-renders when *its own* run changes.
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
        onRunCode={onRunCode}
        onStopCode={onStopCode}
      />
    )
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
          // Positioning context for the pinned row, whose offset is measured from
          // the top of the rows rather than the padded scroll container.
          <div className="relative">
            <div style={{ height: offsets[firstIndex] }} />
            {slice.map((row, i) =>
              firstIndex + i === editIndex ? (
                <div key="edit-slot" style={{ height: editHeight }} />
              ) : (
                renderRow(row, firstIndex + i)
              ),
            )}
            <div style={{ height: total - offsets[lastIndex] }} />
            {editIndex >= 0 && (
              <div className="absolute inset-x-0" style={{ top: offsets[editIndex] }}>
                {renderRow(rows[editIndex], editIndex)}
              </div>
            )}
          </div>
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
  /** Run a code entity: its id and source, plus the path of the row it sits at. */
  onRunCode: (id: string, code: string, path: string[]) => void
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

  // Enter commits, and is handled here rather than left to the key router since
  // the textarea owns bare keys while it has focus.
  //
  // Escape is *not*, though it used to be. The router lets Escape through even in
  // a text field, so cancelling here as well meant it was handled twice: this
  // dropped the edit, and then `edit.cancel` found nothing left to cancel and the
  // press fell through to the next Escape tool in the frame — which cleared the
  // find text. One listener, at the top, as everywhere else.
  const onKeyDown = (e: React.KeyboardEvent<HTMLTextAreaElement>): void => {
    if (e.key === 'Enter' && !e.shiftKey) {
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
          {/* The row it is about to become already wears its type. */}
          {row.type && <TypePill type={row.type} className="mr-1" />}
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
              else onRunCode(row.id, row.text ?? '', row.path)
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
        {/* A typed row says so, whatever the type turns out to mean. It is a
            sibling of the mark rather than something floated into the text: the
            row draws its text four different ways — prose, a code block, a
            file's bytes, a box being typed into — and only the first of those is
            something a float can flow around. So the type takes a column of its
            own, aligned with the mark, and the content keeps the rest. */}
        {row.type && <TypePill type={row.type} className="mr-1" />}
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
          ) : row.type === 'file' ? (
            // The bytes, and whatever the user typed as a caption under them.
            <>
              <ResourceView id={row.id} mimeType={row.mimeType} alt={row.text} />
              {row.text && (
                <EntityMarkdown
                  entityId={row.id}
                  path={row.path}
                  text={row.text}
                  className={cn(TEXT, 'text-gray-500')}
                />
              )}
            </>
          ) : row.text ? (
            // Rendered, not printed: a line with no markup in it comes out
            // exactly as the plain text did, and the blocks are there for the
            // rows that use them.
            <EntityMarkdown
              entityId={row.id}
              path={row.path}
              text={row.text}
              className={cn(
                TEXT,
                row.section && 'font-semibold',
                row.hasChildren && row.collapsed ? 'text-gray-400' : 'text-gray-900',
              )}
              style={heading}
            />
          ) : (
            <span className={`${TEXT} italic text-gray-400`}>Empty</span>
          )}
        </div>
      </div>
    </div>
  )
})
