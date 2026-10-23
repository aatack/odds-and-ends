import type { ReactNode } from 'react'
import { cn } from '../../helpers/cn'

// A quiet square control for a single icon. Disabled when no handler is given.
export function IconButton({
  title,
  onClick,
  className,
  children,
}: {
  title: string
  onClick?: () => void
  className?: string
  children: ReactNode
}): React.JSX.Element {
  return (
    <button
      title={title}
      onClick={onClick}
      disabled={!onClick}
      className={cn(
        'inline-flex size-7 items-center justify-center rounded-md text-gray-500',
        'hover:bg-gray-100 hover:text-gray-700 focus:outline-none',
        'focus-visible:ring-2 focus-visible:ring-brand-500/40 disabled:opacity-40',
        className,
      )}
    >
      {children}
    </button>
  )
}
