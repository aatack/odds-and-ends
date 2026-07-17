import { type ReactNode, useEffect, useRef, useState } from 'react'
import { cn } from '../../helpers/cn'

// The one dropdown for the app: an anchored popover that closes on an outside
// click, on Escape, and (via the `close` passed to its children) on selection.
export function Dropdown({
  trigger,
  children,
  align = 'left',
  menuClassName,
}: {
  trigger: (state: { open: boolean; toggle: () => void }) => ReactNode
  children: (close: () => void) => ReactNode
  align?: 'left' | 'right'
  menuClassName?: string
}): React.JSX.Element {
  const [open, setOpen] = useState(false)
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (!open) return
    const onPointerDown = (e: PointerEvent): void => {
      if (ref.current && !ref.current.contains(e.target as Node)) setOpen(false)
    }
    const onKeyDown = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') setOpen(false)
    }
    document.addEventListener('pointerdown', onPointerDown)
    document.addEventListener('keydown', onKeyDown)
    return () => {
      document.removeEventListener('pointerdown', onPointerDown)
      document.removeEventListener('keydown', onKeyDown)
    }
  }, [open])

  return (
    <div ref={ref} className="relative">
      {trigger({ open, toggle: () => setOpen((v) => !v) })}
      {open && (
        <div
          className={cn(
            'absolute z-50 mt-2 min-w-48 rounded-lg bg-white py-1.5 shadow-lg',
            align === 'right' ? 'right-0' : 'left-0',
            menuClassName,
          )}
        >
          {children(() => setOpen(false))}
        </div>
      )}
    </div>
  )
}

export function DropdownItem({
  active = false,
  onClick,
  children,
}: {
  active?: boolean
  onClick: () => void
  children: ReactNode
}): React.JSX.Element {
  return (
    <button
      className={cn(
        'w-full px-3 py-1.5 text-left text-[13px] hover:bg-gray-100/70 focus:outline-none',
        active ? 'font-medium text-brand-700' : 'text-gray-700',
      )}
      onClick={onClick}
    >
      {children}
    </button>
  )
}

export function DropdownSeparator(): React.JSX.Element {
  return <div className="my-1.5 h-px bg-gray-100" />
}
