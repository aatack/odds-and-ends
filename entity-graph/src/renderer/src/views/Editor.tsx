import React, { useEffect, useMemo, useRef, useState } from 'react'
import { useEditor } from './useEditor'
import type { EditorActions, EditorRow, EntityRow } from './useEditor'

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const ROW_HEIGHT = 32 // px — fixed so the list can be windowed cheaply
const INDENT = 20 // px per depth level
const OVERSCAN = 8 // extra rows rendered above/below the viewport
const VIEWPORT_H = 480 // px

const inputClass =
  'w-full bg-white border border-primary-400 rounded-sm px-1.5 py-0 h-6 leading-6 ' +
  'text-text-sm font-serif text-gray-900 focus:outline-none focus:ring-2 focus:ring-primary-300'

// ---------------------------------------------------------------------------
// Dumb rendering component
// ---------------------------------------------------------------------------

export interface EditorProps {
  rows: EditorRow[]
  rowHeight: number
  editing: boolean
  loading: boolean
  error: string | null
  statusMessage: string | null
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  onContainerKeyDown: (e: React.KeyboardEvent) => void
  onEditKeyDown: (e: React.KeyboardEvent<HTMLInputElement>) => void
  onNearEnd: () => void
}

/**
 * Pure presentation: renders the flat row list as an indented, virtualised set
 * of bullets and forwards interaction to the callbacks it is given. Holds no
 * domain logic — only local view state (scroll position, focus).
 */
export function Editor(props: EditorProps): React.JSX.Element {
  const {
    rows,
    rowHeight,
    editing,
    loading,
    error,
    statusMessage,
    onSelectRow,
    onToggleCollapse,
    onContainerKeyDown,
    onEditKeyDown,
    onNearEnd,
  } = props

  const containerRef = useRef<HTMLDivElement>(null)
  const [scrollTop, setScrollTop] = useState(0)

  // Focus the container on mount and whenever an edit finishes, so the
  // navigation hotkeys keep working without an extra click.
  useEffect(() => {
    if (!editing) containerRef.current?.focus()
  }, [editing])

  // Keep the selected row within the viewport as selection moves.
  const selectedIndex = useMemo(
    () => rows.findIndex((r) => r.kind === 'entity' && r.selected),
    [rows],
  )
  useEffect(() => {
    const el = containerRef.current
    if (!el || selectedIndex < 0) return
    const top = selectedIndex * rowHeight
    const bottom = top + rowHeight
    if (top < el.scrollTop) el.scrollTop = top
    else if (bottom > el.scrollTop + el.clientHeight) el.scrollTop = bottom - el.clientHeight
  }, [selectedIndex, rowHeight])

  const total = rows.length
  const first = Math.max(0, Math.floor(scrollTop / rowHeight) - OVERSCAN)
  const visible = Math.ceil(VIEWPORT_H / rowHeight) + OVERSCAN * 2
  const last = Math.min(total, first + visible)
  const slice = rows.slice(first, last)

  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    const el = e.currentTarget
    setScrollTop(el.scrollTop)
    if (el.scrollTop + el.clientHeight >= el.scrollHeight - rowHeight * OVERSCAN) onNearEnd()
  }

  return (
    <div className="card overflow-hidden font-serif">
      {statusMessage && (
        <div className="px-4 py-2 bg-primary-50 border-b border-primary-100 text-text-sm text-primary-700">
          {statusMessage}
        </div>
      )}
      {error && (
        <div className="px-4 py-2 bg-error-50 border-b border-gray-200 text-text-sm text-error-700">
          {error}
        </div>
      )}

      <div
        ref={containerRef}
        tabIndex={0}
        onKeyDown={onContainerKeyDown}
        onScroll={handleScroll}
        className="relative overflow-y-auto focus:outline-none"
        style={{ height: VIEWPORT_H }}
      >
        {total === 0 ? (
          <div className="px-4 py-8 text-center text-text-sm text-gray-400">
            {loading ? 'Loading…' : 'No entities.'}
          </div>
        ) : (
          <div style={{ height: total * rowHeight, position: 'relative' }}>
            {slice.map((row, i) => {
              const index = first + i
              return (
                <Row
                  key={row.kind === 'entity' ? `${row.path.join('/')}` : `input-${index}`}
                  row={row}
                  top={index * rowHeight}
                  rowHeight={rowHeight}
                  onSelectRow={onSelectRow}
                  onToggleCollapse={onToggleCollapse}
                  onEditKeyDown={onEditKeyDown}
                />
              )
            })}
          </div>
        )}
      </div>

      <div className="px-4 py-2 border-t border-gray-200 text-text-xs text-gray-400 flex flex-wrap gap-x-3 gap-y-1">
        <Hint k="w/s" label="up/down" />
        <Hint k="a" label="parent" />
        <Hint k="←/→" label="collapse/expand" />
        <Hint k="e" label="edit" />
        <Hint k="enter" label="new child" />
        <Hint k="del" label="unlink" />
        <Hint k="x" label="move" />
        <Hint k="r / shift+r" label="link" />
      </div>
    </div>
  )
}

function Hint({ k, label }: { k: string; label: string }): React.JSX.Element {
  return (
    <span>
      <kbd className="font-sans font-medium text-gray-500">{k}</kbd> {label}
    </span>
  )
}

// ---------------------------------------------------------------------------
// Row — memoised so scrolling only re-renders rows that actually change
// ---------------------------------------------------------------------------

interface RowProps {
  row: EditorRow
  top: number
  rowHeight: number
  onSelectRow: (path: string[]) => void
  onToggleCollapse: (row: EntityRow) => void
  onEditKeyDown: (e: React.KeyboardEvent<HTMLInputElement>) => void
}

const Row = React.memo(function Row({
  row,
  top,
  rowHeight,
  onSelectRow,
  onToggleCollapse,
  onEditKeyDown,
}: RowProps): React.JSX.Element {
  const base: React.CSSProperties = {
    position: 'absolute',
    top,
    left: 0,
    right: 0,
    height: rowHeight,
  }

  if (row.kind === 'input') {
    return (
      <div style={base} className="flex items-center">
        <div className="flex-1 pr-3" style={{ paddingLeft: row.depth * INDENT + 24 }}>
          <input
            autoFocus
            defaultValue=""
            placeholder="New entity…"
            className={inputClass}
            onKeyDown={onEditKeyDown}
            onClick={(e) => e.stopPropagation()}
          />
        </div>
      </div>
    )
  }

  return (
    <div style={base} className="flex items-center" onClick={() => onSelectRow(row.path)}>
      <div
        className={`flex items-center h-7 mx-2 pr-2 rounded-md flex-1 min-w-0 cursor-default ${
          row.selected ? 'bg-primary-50' : ''
        }`}
        style={{ paddingLeft: row.depth * INDENT + 4 }}
      >
        <span
          className="w-5 shrink-0 text-center text-gray-400 select-none"
          onClick={(e) => {
            e.stopPropagation()
            if (row.hasChildren) onToggleCollapse(row)
          }}
        >
          {row.hasChildren ? (row.collapsed ? '▸' : '▾') : '•'}
        </span>
        {row.editing ? (
          <input
            autoFocus
            defaultValue={row.text ?? ''}
            className={inputClass}
            onKeyDown={onEditKeyDown}
            onClick={(e) => e.stopPropagation()}
          />
        ) : row.text ? (
          <span className="truncate text-text-sm text-gray-900">{row.text}</span>
        ) : (
          <span className="italic text-text-sm text-gray-400">Empty</span>
        )}
      </div>
    </div>
  )
})

// ---------------------------------------------------------------------------
// Container — wires the logic hook to the dumb component
// ---------------------------------------------------------------------------

export interface EditorViewProps {
  rootId: string
  maxDepth?: number
  actions: EditorActions
}

/** Glue between {@link useEditor} (logic) and {@link Editor} (rendering). */
export function EditorView({ rootId, maxDepth, actions }: EditorViewProps): React.JSX.Element {
  const ed = useEditor({ rootId, maxDepth, actions })
  return (
    <Editor
      rows={ed.rows}
      rowHeight={ROW_HEIGHT}
      editing={ed.editing}
      loading={ed.loading}
      error={ed.error}
      statusMessage={ed.statusMessage}
      onSelectRow={ed.selectRow}
      onToggleCollapse={ed.toggleCollapse}
      onContainerKeyDown={ed.onContainerKeyDown}
      onEditKeyDown={ed.onEditKeyDown}
      onNearEnd={ed.loadMore}
    />
  )
}
