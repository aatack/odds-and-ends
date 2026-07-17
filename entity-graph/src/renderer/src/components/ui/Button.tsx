import type { ButtonHTMLAttributes } from 'react'
import { cn } from '../../helpers/cn'

type Variant = 'primary' | 'secondary' | 'tertiary' | 'danger'
type Size = 'sm' | 'md'

const BASE =
  'inline-flex items-center justify-center gap-1.5 rounded-md font-medium ' +
  'transition-colors focus:outline-none focus-visible:ring-2 ' +
  'focus-visible:ring-brand-500/40 disabled:cursor-not-allowed disabled:opacity-50'

const VARIANTS: Record<Variant, string> = {
  primary: 'bg-brand-600 text-white hover:bg-brand-700',
  secondary: 'bg-gray-100 text-gray-700 hover:bg-gray-200',
  tertiary: 'text-gray-500 hover:bg-gray-100 hover:text-gray-700',
  danger: 'text-error-600 hover:bg-error-50',
}

const SIZES: Record<Size, string> = {
  sm: 'h-7 px-2 text-[13px]',
  md: 'h-8 px-3 text-[13px]',
}

export function Button({
  variant = 'secondary',
  size = 'md',
  className,
  ...rest
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  variant?: Variant
  size?: Size
}): React.JSX.Element {
  return <button className={cn(BASE, VARIANTS[variant], SIZES[size], className)} {...rest} />
}
