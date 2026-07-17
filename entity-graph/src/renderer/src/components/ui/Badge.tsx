import type { ReactNode } from 'react'
import { cn } from '../../helpers/cn'

export type BadgeColor = 'gray' | 'brand' | 'success' | 'warning' | 'error' | 'blue'

const COLORS: Record<BadgeColor, string> = {
  gray: 'bg-gray-100 text-gray-600',
  brand: 'bg-brand-50 text-brand-700',
  success: 'bg-success-50 text-success-700',
  warning: 'bg-warning-50 text-warning-700',
  error: 'bg-error-50 text-error-700',
  blue: 'bg-blue-50 text-blue-700',
}

const DOTS: Record<BadgeColor, string> = {
  gray: 'bg-gray-400',
  brand: 'bg-brand-500',
  success: 'bg-success-600',
  warning: 'bg-warning-700',
  error: 'bg-error-600',
  blue: 'bg-blue-700',
}

// `dot` renders the calmest form — a coloured dot with neutral text — preferred
// for status. Without it, a subtle filled chip, for labels like a source's type.
export function Badge({
  color = 'gray',
  dot = false,
  className,
  children,
}: {
  color?: BadgeColor
  dot?: boolean
  className?: string
  children: ReactNode
}): React.JSX.Element {
  if (dot) {
    return (
      <span
        className={cn(
          'inline-flex items-center gap-1.5 text-[11px] font-medium text-gray-600',
          className,
        )}
      >
        <span className={cn('size-1.5 shrink-0 rounded-full', DOTS[color])} />
        {children}
      </span>
    )
  }

  return (
    <span
      className={cn(
        'inline-flex items-center gap-1 rounded-md px-1.5 py-0.5 text-[11px] font-medium',
        COLORS[color],
        className,
      )}
    >
      {children}
    </span>
  )
}
