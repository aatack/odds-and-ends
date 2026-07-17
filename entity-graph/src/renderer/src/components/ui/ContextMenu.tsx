import { useEffect, useRef } from 'react'
import { cn } from '../../helpers/cn'

export interface ContextMenuItem {
  label: string
  onClick: () => void
  danger?: boolean
}

// A menu anchored at the cursor. Closes on an outside click, Escape, scroll, or
// resize — the same dismissal rules as the Dropdown, positioned at (x, y).
export function ContextMenu({
  x,
  y,
  items,
  onClose,
}: {
  x: number
  y: number
  items: ContextMenuItem[]
  onClose: () => void
}): React.JSX.Element {
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const onPointerDown = (e: PointerEvent): void => {
      if (ref.current && !ref.current.contains(e.target as Node)) onClose()
    }
    const onKeyDown = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') onClose()
    }
    document.addEventListener('pointerdown', onPointerDown)
    document.addEventListener('keydown', onKeyDown)
    document.addEventListener('scroll', onClose, true)
    window.addEventListener('resize', onClose)
    return () => {
      document.removeEventListener('pointerdown', onPointerDown)
      document.removeEventListener('keydown', onKeyDown)
      document.removeEventListener('scroll', onClose, true)
      window.removeEventListener('resize', onClose)
    }
  }, [onClose])

  return (
    <div
      ref={ref}
      className="fixed z-50 min-w-44 rounded-lg bg-white py-1.5 shadow-lg"
      style={{ top: y, left: x }}
    >
      {items.map((item) => (
        <button
          key={item.label}
          onClick={item.onClick}
          className={cn(
            'w-full px-3 py-1.5 text-left text-[13px] hover:bg-gray-100/70 focus:outline-none',
            item.danger ? 'text-error-600' : 'text-gray-700',
          )}
        >
          {item.label}
        </button>
      ))}
    </div>
  )
}
