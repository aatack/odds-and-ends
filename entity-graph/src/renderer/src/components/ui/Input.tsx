import type { InputHTMLAttributes } from 'react'
import { cn } from '../../helpers/cn'

const BASE =
  'w-full rounded-md bg-white px-2.5 text-[13px] text-gray-900 placeholder:text-gray-400 ' +
  'border border-gray-200 shadow-xs focus:outline-none ' +
  'focus-visible:border-brand-500 focus-visible:ring-2 focus-visible:ring-brand-500/40 ' +
  'disabled:opacity-50'

// A compact, soft-bordered text field for structured form entry (URLs, tokens,
// paths). Free-form prose uses TextEditor instead.
export function Input({
  className,
  mono = false,
  ...rest
}: InputHTMLAttributes<HTMLInputElement> & { mono?: boolean }): React.JSX.Element {
  return <input className={cn(BASE, 'h-8', mono && 'font-mono', className)} {...rest} />
}
