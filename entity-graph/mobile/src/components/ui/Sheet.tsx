import React from 'react'
import { X } from '@untitledui/icons'
import { cn } from '../../helpers/cn'

// A bottom sheet: the phone's answer to the desktop's popups, dropdowns, context
// menus and command palette, all of which assume a cursor and a corner to open
// towards. A sheet needs neither — it comes up from the edge nearest the thumb,
// takes as much height as it needs, and is dismissed by tapping away from it.
//
// This is the one place the app moves: see the note on `.sheet-in` in index.css.

export function Sheet({
  title,
  onClose,
  children,
  footer,
}: {
  title: string
  onClose: () => void
  children: React.ReactNode
  /** Pinned below the scrolling body — a confirm button, say. */
  footer?: React.ReactNode
}): React.JSX.Element {
  return (
    <div className="fixed inset-0 z-40 flex flex-col justify-end">
      {/* The scrim. Tapping it is the main way out, so it takes the whole area
          above the sheet rather than being decoration. */}
      <div
        className="absolute inset-0 bg-gray-950/25"
        onClick={onClose}
        aria-hidden="true"
      />
      <div
        role="dialog"
        aria-modal="true"
        aria-label={title}
        className={cn(
          'sheet-in relative flex max-h-[85vh] flex-col rounded-t-2xl bg-white shadow-lg',
          'pb-[var(--inset-bottom)]',
        )}
      >
        <div className="flex items-center gap-2 px-4 pt-3 pb-2">
          {/* A grabber, purely so the sheet reads as one — it isn't draggable. */}
          <span className="absolute left-1/2 top-2 h-1 w-9 -translate-x-1/2 rounded-full bg-gray-200" />
          <h2 className="min-w-0 flex-1 truncate pt-2 text-[15px] font-semibold text-gray-800">
            {title}
          </h2>
          <button
            type="button"
            aria-label="Close"
            onClick={onClose}
            className="-mr-2 flex size-11 items-center justify-center rounded-xl text-gray-400 active:bg-gray-100"
          >
            <X size={18} />
          </button>
        </div>
        <div className="min-h-0 flex-1 overflow-y-auto overscroll-contain px-4 pb-3">{children}</div>
        {footer && <div className="px-4 pt-1 pb-3">{footer}</div>}
      </div>
    </div>
  )
}

/** A tappable line in a sheet's list. */
export function SheetRow({
  label,
  detail,
  selected,
  onClick,
}: {
  label: string
  detail?: string
  selected?: boolean
  onClick: () => void
}): React.JSX.Element {
  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        'flex min-h-12 w-full items-center gap-3 rounded-xl px-3 text-left active:bg-gray-100',
        selected ? 'bg-blue-50' : '',
      )}
    >
      <span className="min-w-0 flex-1 truncate text-[15px] text-gray-900">{label}</span>
      {detail && <span className="shrink-0 text-[12px] text-gray-400">{detail}</span>}
    </button>
  )
}
