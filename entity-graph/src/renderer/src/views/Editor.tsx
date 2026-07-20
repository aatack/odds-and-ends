import React, { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'
import { ChevronDown, ChevronRight } from '@untitledui/icons'
import { TextEditor } from '../components/ui/TextEditor'
import { ContextMenu, type ContextMenuItem } from '../components/ui/ContextMenu'
import type { EditorRow, EntityRow } from './useEditor'

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

const keyOf = (row: EditorRow, index: number): string =>
  row.kind === 'entity' ? row.path.join('/') : `input-${index}`

// ---------------------------------------------------------------------------
// Dumb rendering component
// ---------------------------------------------------------------------------

export interface EditorProps {
  rows: EditorRow[]
  loading: boolean
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  onCommitEdit: (value: string) => void
  onCancelEdit: () => void
  onExport: () => void
  onDebug: () => void
  onNearEnd: () => void
  /** Double-clicking a row "activates" it — the layout opens it in a new frame. */
  onActivateRow?: (path: string[]) => void
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
    onCommitEdit,
    onCancelEdit,
    onExport,
    onDebug,
    onNearEnd,
    onActivateRow,
  } = props

  const containerRef = useRef<HTMLDivElement>(null)
  // Right-click menu target: a row index + screen position.
  const [menu, setMenu] = useState<{ index: number; x: number; y: number } | null>(null)
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

  const openMenuFor = (index: number, e: React.MouseEvent): void => {
    e.preventDefault()
    const row = rows[index]
    if (row.kind === 'entity') onSelectRow(row.path)
    setMenu({ index, x: e.clientX, y: e.clientY })
  }

  // The menu acts on the row it opened over, which openMenuFor has selected, so
  // the export/debug actions (which operate on the selection) hit the right one.
  const menuRow = menu ? rows[menu.index] : null
  const menuItems: ContextMenuItem[] =
    menu && menuRow?.kind === 'entity'
      ? [
          { label: 'Export', onClick: () => { onExport(); setMenu(null) } },
          { label: 'Debug entity', onClick: () => { onDebug(); setMenu(null) } },
        ]
      : []

  return (
    <div className="h-full flex flex-col overflow-hidden bg-white">
      <div
        ref={containerRef}
        onScroll={handleScroll}
        className="relative flex-1 min-h-0 overflow-y-auto py-1"
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
              return (
                <Row
                  key={key}
                  row={row}
                  measureKey={key}
                  onMeasure={setHeight}
                  onSelectRow={onSelectRow}
                  onToggleCollapse={onToggleCollapse}
                  onCommitEdit={onCommitEdit}
                  onCancelEdit={onCancelEdit}
                  onActivateRow={onActivateRow}
                  onContextMenu={
                    row.kind === 'entity' ? (e) => openMenuFor(index, e) : undefined
                  }
                />
              )
            })}
            <div style={{ height: total - offsets[lastIndex] }} />
          </>
        )}
      </div>

      {menu && menuItems.length > 0 && (
        <ContextMenu x={menu.x} y={menu.y} items={menuItems} onClose={() => setMenu(null)} />
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Row — memoised so scrolling only re-renders rows that actually change
// ---------------------------------------------------------------------------

interface RowProps {
  row: EditorRow
  measureKey: string
  onMeasure: (key: string, height: number) => void
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  onCommitEdit: (value: string) => void
  onCancelEdit: () => void
  onActivateRow?: (path: string[]) => void
  onContextMenu?: (e: React.MouseEvent) => void
}

// Escape abandons the edit; everything else (Enter to commit, autosize, blur to
// commit) is the TextEditor's own behaviour.
const escapeCancels =
  (onCancelEdit: () => void) =>
  (e: React.KeyboardEvent<HTMLTextAreaElement>): void => {
    if (e.key === 'Escape') {
      e.preventDefault()
      onCancelEdit()
    }
  }

const Row = React.memo(function Row({
  row,
  measureKey,
  onMeasure,
  onSelectRow,
  onToggleCollapse,
  onCommitEdit,
  onCancelEdit,
  onActivateRow,
  onContextMenu,
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

  if (row.kind === 'input') {
    return (
      <div ref={ref} className="flex">
        <div
          className="flex items-start py-0.5 mx-2 pr-2 flex-1 min-w-0"
          style={{ paddingLeft: row.depth * INDENT + 4 }}
        >
          <span className="flex h-5 w-5 shrink-0 items-center justify-center text-gray-400 select-none">
            <span className="size-1 rounded-full bg-gray-300" />
          </span>
          <div className="flex-1 min-w-0">
            <TextEditor
              autoFocus
              value=""
              setValue={onCommitEdit}
              placeholder="New entity…"
              onKeyDown={escapeCancels(onCancelEdit)}
              className={`${TEXT} text-gray-900`}
            />
          </div>
        </div>
      </div>
    )
  }

  return (
    <div
      ref={ref}
      className="flex"
      onClick={() => onSelectRow(row.path)}
      onDoubleClick={onActivateRow ? () => onActivateRow(row.path) : undefined}
      onContextMenu={onContextMenu}
    >
      <div
        className={`flex items-start my-px py-0.5 mx-2 pr-2 rounded-md flex-1 min-w-0 cursor-default ${
          row.selected ? 'bg-blue-100' : 'hover:bg-gray-100/70'
        }`}
        style={{ paddingLeft: row.depth * INDENT + 4 }}
      >
        <span
          className="flex h-5 w-5 shrink-0 items-center justify-center text-gray-400 select-none"
          onClick={(e) => {
            e.stopPropagation()
            if (row.hasChildren) onToggleCollapse(row)
          }}
        >
          {row.hasChildren ? (
            row.collapsed ? (
              <ChevronRight size={14} />
            ) : (
              <ChevronDown size={14} />
            )
          ) : (
            <span className="size-1 rounded-full bg-gray-300" />
          )}
        </span>
        <div className="flex-1 min-w-0">
          {row.editing ? (
            <TextEditor
              autoFocus
              value={row.text ?? ''}
              setValue={onCommitEdit}
              onKeyDown={escapeCancels(onCancelEdit)}
              className={`${TEXT} text-gray-900`}
            />
          ) : row.text ? (
            <span
              className={`whitespace-pre-wrap break-words ${TEXT} ${
                row.hasChildren && row.collapsed ? 'text-gray-400' : 'text-gray-900'
              }`}
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
