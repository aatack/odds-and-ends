import { type ReactNode, useEffect, useLayoutEffect, useRef, useState } from 'react'
import { createPortal } from 'react-dom'
import { cn } from '../../helpers/cn'
import { openModal } from '../../state/ui'

// The one dropdown for the app: an anchored popover that closes on an outside
// click, on Escape, and (via the `close` passed to its children) on selection.
// While open it holds the keyboard as a dialog does (`openModal`), so Escape
// closes the menu and not the dialog it sits in.
//
// The menu is portalled to <body> and fixed beside its trigger, sized to the room
// the window has below it (or above, where there is more), and scrolls inside
// itself. Laid out in place, a tall menu inside a scrolling panel would stretch
// the panel and scroll the page instead. Scrolling or resizing anything closes
// it, since it no longer sits by its trigger.

const GAP = 4
const EDGE = 8

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
  const anchor = useRef<HTMLDivElement>(null)
  const menu = useRef<HTMLDivElement>(null)
  const [place, setPlace] = useState<React.CSSProperties | null>(null)

  // Placed before paint, from where the trigger is now.
  useLayoutEffect(() => {
    if (!open || !anchor.current) {
      setPlace(null)
      return
    }
    const box = anchor.current.getBoundingClientRect()
    const below = window.innerHeight - box.bottom - GAP - EDGE
    const above = box.top - GAP - EDGE
    const down = below >= 240 || below >= above
    setPlace({
      ...(down ? { top: box.bottom + GAP } : { bottom: window.innerHeight - box.top + GAP }),
      ...(align === 'right' ? { right: window.innerWidth - box.right } : { left: box.left }),
      maxHeight: Math.max(120, down ? below : above),
    })
  }, [open, align])

  useEffect(() => {
    if (!open) return
    const close = (): void => setOpen(false)
    const onPointerDown = (e: PointerEvent): void => {
      const target = e.target as Node
      if (!anchor.current?.contains(target) && !menu.current?.contains(target)) close()
    }
    // A scroll inside the menu is the menu doing its job; anywhere else moves the
    // trigger out from under it.
    const onScroll = (e: Event): void => {
      if (!menu.current?.contains(e.target as Node)) close()
    }
    const release = openModal(close)
    document.addEventListener('pointerdown', onPointerDown)
    document.addEventListener('scroll', onScroll, true)
    window.addEventListener('resize', close)
    return () => {
      release()
      document.removeEventListener('pointerdown', onPointerDown)
      document.removeEventListener('scroll', onScroll, true)
      window.removeEventListener('resize', close)
    }
  }, [open])

  return (
    <div ref={anchor} className="relative">
      {trigger({ open, toggle: () => setOpen((v) => !v) })}
      {open &&
        place &&
        createPortal(
          <div
            ref={menu}
            className={cn(
              'fixed z-[70] min-w-48 overflow-y-auto rounded-lg bg-white py-1.5 shadow-lg',
              menuClassName,
            )}
            style={place}
          >
            {children(() => setOpen(false))}
          </div>,
          document.body,
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
