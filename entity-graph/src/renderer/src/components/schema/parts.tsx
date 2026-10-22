import React from 'react'
import { Check, ChevronDown, Plus } from '@untitledui/icons'
import { Dropdown } from '../ui/Dropdown'

// The pieces the schema form and the schema builder share, so the two read as
// one thing: the borderless inputs their lines are made of, the menu a choice is
// made from, and the line that adds another.

/** Join class names, skipping the ones that are off. */
export const cx = (...parts: Array<string | false | null | undefined>): string =>
  parts.filter(Boolean).join(' ')

/** A text box with no surface of its own: the line it sits in is the surface. */
export const BARE =
  'h-8 w-full min-w-0 bg-transparent px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 focus:outline-none'
export const BARE_MONO = cx(BARE, 'font-mono text-[12.5px]')
/** A value shown rather than edited. */
export const MUTED = 'px-2.5 font-mono text-[12.5px] leading-8 text-gray-400'

/** The chip a menu opens from: what is chosen, and a chevron. */
export function MenuChip({
  onClick,
  children,
}: {
  onClick: () => void
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <button
      className="flex h-6 max-w-full min-w-0 items-center gap-0.5 rounded bg-gray-100 px-1.5 text-[12px] text-gray-600 hover:bg-gray-200 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-500/40"
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

/** A menu of named options, opened from whatever `trigger` draws. */
export function Picker({
  names,
  at,
  onPick,
  trigger,
}: {
  names: string[]
  /** The option chosen now, or -1 for none. */
  at: number
  onPick: (index: number) => void
  trigger: (toggle: () => void) => React.ReactNode
}): React.JSX.Element {
  return (
    <Dropdown trigger={({ toggle }) => trigger(toggle)}>
      {(close) =>
        names.map((name, i) => (
          <MenuItem
            key={i}
            on={i === at}
            onClick={() => {
              if (i !== at) onPick(i)
              close()
            }}
          >
            {name}
          </MenuItem>
        ))
      }
    </Dropdown>
  )
}

/** The last line of a run: adds another item, field or option. */
export function AddLine({
  onClick,
  className,
  children,
}: {
  onClick: () => void
  className?: string
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <button
      className={cx(
        'flex h-8 w-full items-center gap-1.5 px-2.5 text-[12px] text-gray-400 hover:bg-gray-50 hover:text-gray-700 focus:outline-none focus-visible:bg-gray-50',
        className,
      )}
      onClick={onClick}
    >
      <Plus size={13} /> {children}
    </button>
  )
}
