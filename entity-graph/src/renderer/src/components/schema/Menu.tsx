import React from 'react'
import { Check, ChevronDown } from '@untitledui/icons'
import { cn } from '../../helpers/cn'

// The menu pieces the schema builder and the form share, so a choice looks the
// same wherever it is made: a small chip that opens it, and a line per option
// with a tick on the one chosen.

/** The chip a menu opens from: what is chosen, and a chevron. */
export function MenuChip({
  onClick,
  children,
  className,
}: {
  onClick: () => void
  children: React.ReactNode
  className?: string
}): React.JSX.Element {
  return (
    <button
      className={cn(
        'flex h-6 max-w-full min-w-0 items-center gap-0.5 rounded bg-gray-100 px-1.5 text-[12px] text-gray-600 hover:bg-gray-200 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-500/40',
        className,
      )}
      onClick={onClick}
    >
      <span className="truncate">{children}</span>
      <ChevronDown size={12} className="shrink-0 text-gray-400" />
    </button>
  )
}

/** One line of a menu, with a tick where it is on. */
export function MenuItem({
  on,
  onClick,
  children,
}: {
  on: boolean
  onClick: () => void
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <button
      className="flex w-full items-center gap-2 px-3 py-1.5 text-left text-[13px] text-gray-700 hover:bg-gray-100/70 focus:outline-none"
      onClick={onClick}
    >
      <span className="flex size-3.5 shrink-0 items-center justify-center text-brand-600">
        {on && <Check size={13} />}
      </span>
      {children}
    </button>
  )
}
