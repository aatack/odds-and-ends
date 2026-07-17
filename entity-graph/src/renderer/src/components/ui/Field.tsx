import type { ReactNode } from 'react'
import { cn } from '../../helpers/cn'

// A labelled form control: a quiet micro-label above its input.
export function Field({
  label,
  className,
  children,
}: {
  label: ReactNode
  className?: string
  children: ReactNode
}): React.JSX.Element {
  return (
    <label className={cn('block space-y-1', className)}>
      <span className="block text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
        {label}
      </span>
      {children}
    </label>
  )
}
