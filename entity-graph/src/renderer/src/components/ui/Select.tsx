import type { SelectHTMLAttributes } from 'react'
import { cn } from '../../helpers/cn'

const BASE =
  'h-8 w-full rounded-md bg-white px-2.5 text-[13px] text-gray-900 border border-gray-200 ' +
  'shadow-xs transition-colors focus:outline-none focus-visible:border-brand-500 ' +
  'focus-visible:ring-2 focus-visible:ring-brand-500/40 disabled:opacity-50'

export function Select({
  className,
  ...rest
}: SelectHTMLAttributes<HTMLSelectElement>): React.JSX.Element {
  return <select className={cn(BASE, className)} {...rest} />
}
