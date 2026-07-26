import React from 'react'
import { Check, ChevronDown, ChevronRight, Square } from '@untitledui/icons'
import { TextEditor } from '../components/ui/TextEditor'
import { cn } from '../helpers/cn'
import type { EntityRow, Row } from '../state/derive'
import { FileView } from './FileView'
import { useLongPress } from './useLongPress'

// Pure presentation: the flat row list as an indented set of wrapping bullets, with
// every gesture forwarded to a callback. No state of its own, no reads from the
// store — the logic is in ./useOutline.

/** Pixels of indent per level. Narrower than the desktop's 20: a phone has ~360 to
 * spend, and four levels of 20 is a fifth of the screen gone. */
const INDENT = 15

// User-written entity text, in the serif the desktop app uses for the same thing.
// It wraps rather than truncating — a row is as tall as it needs to be.
const TEXT = 'entity-text block font-serif text-[16px] leading-[1.45]'
const MONO = 'entity-text block font-mono text-[13px] leading-[1.5]'

/** Section headings grow with prominence, easing off as they nest. */
const sectionStyle = (depth: number): React.CSSProperties => ({
  fontSize: `${16 * Math.max(1.08, 1.4 - depth * 0.08)}px`,
  lineHeight: 1.3,
})

/** The glyph in front of a row: a checkbox, a chevron, or a plain bullet. */
function Mark({
  open,
  hasChildren,
  collapsed,
}: {
  open?: boolean
  hasChildren?: boolean
  collapsed?: boolean
}): React.JSX.Element {
  // A checkbox keeps its box even with children — the tick is what the row is
  // *for*, and folding it is a rarer thing to want than ticking it.
  if (open === true) return <Square size={16} />
  if (open === false) return <Check size={16} />
  if (hasChildren) return collapsed ? <ChevronRight size={17} /> : <ChevronDown size={17} />
  return <span className="size-1.5 rounded-full bg-gray-300" />
}

export interface OutlineProps {
  rows: Row[]
  loading: boolean
  error: string | null
  /** True while the app is waiting for a row to be tapped as an argument. */
  picking: boolean
  onTapRow: (row: EntityRow) => void
  onTapMark: (row: EntityRow) => void
  onLongPressRow: (row: EntityRow) => void
  /** Every keystroke of an in-place edit; the draft is part of the state. */
  onDraft: (text: string) => void
  /** Write the draft — a blur, which is every way out of the box bar Cancel. */
  onCommit: () => void
  /** The end of the list is in view and there is another page to ask for. */
  onNearEnd: () => void
}

export function Outline(props: OutlineProps): React.JSX.Element {
  const {
    rows,
    loading,
    error,
    picking,
    onTapRow,
    onTapMark,
    onLongPressRow,
    onDraft,
    onCommit,
    onNearEnd,
  } = props

  if (error) {
    return (
      <div className="px-5 py-10 text-center">
        <p className="text-[15px] text-error-600">{error}</p>
        <p className="mt-2 text-[13px] text-gray-400">
          Pull down the menu and refresh once whatever that was has been dealt with.
        </p>
      </div>
    )
  }

  if (rows.length === 0) {
    return (
      <div className="px-5 py-10 text-center text-[14px] text-gray-400">
        {loading ? 'Loading…' : 'Nothing here yet.'}
      </div>
    )
  }

  return (
    <div className="px-1 pt-1">
      {rows.map((row) => (
        // The input row's key is fixed: moving it must not remount it, or the box
        // being typed into loses focus and the keyboard shuts mid-word.
        //
        // Props are passed one by one rather than spread: `rows` is a fresh array on
        // every state change, and handing it to a memoised row would re-render all two
        // hundred of them whenever anything at all moved.
        <RowView
          key={row.kind === 'entity' ? row.path.join('/') : 'input'}
          row={row}
          picking={picking}
          onTapRow={onTapRow}
          onTapMark={onTapMark}
          onLongPressRow={onLongPressRow}
          onDraft={onDraft}
          onCommit={onCommit}
        />
      ))}
      {/* The sentinel: reaching it asks for the next page. A plain element rather
          than an IntersectionObserver, since it is only ever at the bottom of the
          list and the scroll container already reports its own position. */}
      <EndOfList loading={loading} onReach={onNearEnd} />
    </div>
  )
}

function EndOfList({
  loading,
  onReach,
}: {
  loading: boolean
  onReach: () => void
}): React.JSX.Element {
  const ref = React.useRef<HTMLDivElement>(null)
  React.useEffect(() => {
    const el = ref.current
    if (!el) return
    const observer = new IntersectionObserver((entries) => {
      if (entries.some((e) => e.isIntersecting)) onReach()
    })
    observer.observe(el)
    return () => observer.disconnect()
  }, [onReach])
  return (
    <div ref={ref} className="py-6 text-center text-[12px] text-gray-400">
      {loading ? 'Loading…' : ''}
    </div>
  )
}

// ---------------------------------------------------------------------------
// One row
// ---------------------------------------------------------------------------

const RowView = React.memo(function RowView({
  row,
  picking,
  onTapRow,
  onTapMark,
  onLongPressRow,
  onDraft,
  onCommit,
}: { row: Row } & Omit<OutlineProps, 'rows' | 'loading' | 'error' | 'onNearEnd'>): React.JSX.Element {
  const press = useLongPress(
    () => row.kind === 'entity' && onTapRow(row),
    () => row.kind === 'entity' && onLongPressRow(row),
  )

  const heading = row.section ? sectionStyle(row.depth) : undefined
  const isCode = row.type === 'code'
  const pad = { paddingLeft: row.depth * INDENT + 4 }

  if (row.kind === 'input') {
    return (
      <div className="flex items-start gap-1 py-1.5 pr-3" style={pad}>
        <span className="flex h-6 w-7 shrink-0 items-center justify-center text-gray-300">
          <Mark open={row.open} />
        </span>
        <div className="min-w-0 flex-1">
          <TextEditor
            autoFocus
            value={row.draft}
            setValue={onDraft}
            onBlur={onCommit}
            placeholder={row.section ? 'New section…' : 'New line…'}
            className={cn(isCode ? MONO : TEXT, 'text-gray-900', row.section && 'font-semibold')}
            style={heading}
          />
        </div>
      </div>
    )
  }

  return (
    <div
      className={cn(
        'row-virtual flex items-start gap-1 rounded-lg py-1.5 pr-2 select-none',
        row.selected && !picking && 'bg-blue-50',
        picking && 'active:bg-brand-50',
      )}
      style={{ ...pad, touchAction: 'manipulation' }}
      data-entity-id={row.id}
    >
      {/* The mark has its own tap target: fold and tick live here, so the text next
          to it is free to mean "this row" without ambiguity. */}
      <button
        type="button"
        aria-label={row.open !== undefined ? 'Tick' : row.hasChildren ? 'Fold' : 'Bullet'}
        onClick={() => onTapMark(row)}
        className="flex h-7 w-7 shrink-0 items-center justify-center rounded-md text-gray-400 active:bg-gray-100"
      >
        <Mark open={row.open} hasChildren={row.hasChildren} collapsed={row.collapsed} />
      </button>

      <div className="min-w-0 flex-1" {...(row.editing ? {} : press)}>
        {row.editing ? (
          <TextEditor
            autoFocus
            value={row.draft ?? ''}
            setValue={onDraft}
            onBlur={onCommit}
            className={cn(isCode ? MONO : TEXT, 'text-gray-900', row.section && 'font-semibold')}
            style={heading}
          />
        ) : isCode ? (
          // Shown, not run: the desktop app runs a code entity in a QuickJS worker
          // with the whole tool registry bound into it, and none of that belongs on
          // a phone. It is still readable and editable here.
          <pre className={cn(MONO, 'overflow-x-auto rounded-md bg-gray-100 px-2 py-1.5 text-gray-800')}>
            {row.text ?? ''}
          </pre>
        ) : row.type === 'file' ? (
          <>
            <FileView id={row.id} mimeType={row.mimeType} alt={row.text} />
            {row.text && <span className={cn(TEXT, 'text-gray-500')}>{row.text}</span>}
          </>
        ) : row.text ? (
          <span
            className={cn(
              TEXT,
              row.section && 'font-semibold',
              row.collapsed && row.hasChildren ? 'text-gray-400' : 'text-gray-900',
            )}
            style={heading}
          >
            {row.text}
          </span>
        ) : (
          <span className={cn(TEXT, 'text-gray-400 italic')}>Empty</span>
        )}
      </div>
    </div>
  )
})
