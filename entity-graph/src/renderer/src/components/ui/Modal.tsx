import type { ReactNode } from 'react'
import { X } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { IconButton } from './IconButton'

/**
 * How much of the screen the card takes. `md` is a form; `wide` is a form with a
 * table in it; `large` is a panel you work in rather than answer — nearly the
 * whole window, its body scrolling inside the card so that whatever the child
 * puts at the top (a tab bar) stays where it is.
 */
export type ModalSize = 'md' | 'wide' | 'large'

const WIDTHS: Record<ModalSize, string> = {
  md: 'max-w-md',
  wide: 'max-w-2xl',
  large: 'max-w-5xl',
}

// A centred overlay with a click-away backdrop and a soft-edged card. The one
// dialog shell for the whole app, so every modal reads the same.
export function Modal({
  title,
  onClose,
  size = 'md',
  children,
}: {
  title: ReactNode
  onClose: () => void
  size?: ModalSize
  children: ReactNode
}): React.JSX.Element {
  const large = size === 'large'
  return (
    <div
      className="fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-gray-950/30 p-6"
      onClick={onClose}
    >
      <div
        className={cn(
          'my-6 w-full rounded-xl bg-white shadow-lg',
          WIDTHS[size],
          // Exactly the room left by the backdrop's padding and the card's own
          // margin (1.5rem each, top and bottom), so the tall card fills the
          // window without the backdrop having anything to scroll.
          large && 'flex h-[calc(100vh-6rem)] flex-col',
        )}
        onClick={(e) => e.stopPropagation()}
      >
        <div className="flex items-center justify-between px-5 py-3.5">
          <p className="text-[13px] font-semibold text-gray-900">{title}</p>
          <IconButton title="Close" onClick={onClose}>
            <X size={16} />
          </IconButton>
        </div>
        {/* A large modal's child owns its own layout and has a height to fill;
            everything else is a stack of fields that grows as it likes. */}
        <div className={cn('px-5 pb-5', large ? 'flex min-h-0 flex-1 flex-col' : 'space-y-4')}>
          {children}
        </div>
      </div>
    </div>
  )
}
