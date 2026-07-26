import React from 'react'
import { cn } from '../../helpers/cn'

// One button, three tones. Sized for a thumb rather than a cursor: nothing here is
// smaller than 44px in its tappable direction, which is the smallest target that
// reliably takes a finger.

export type ButtonTone = 'primary' | 'plain' | 'quiet' | 'danger'

const TONES: Record<ButtonTone, string> = {
  primary: 'bg-brand-600 text-white active:bg-brand-700',
  plain: 'bg-gray-100 text-gray-800 active:bg-gray-200',
  quiet: 'text-gray-600 active:bg-gray-100',
  danger: 'bg-error-50 text-error-700 active:bg-error-200',
}

export interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  tone?: ButtonTone
  /** Fill the width of its container — what a sheet's confirm button wants. */
  block?: boolean
}

export function Button({
  tone = 'plain',
  block,
  className,
  ...rest
}: ButtonProps): React.JSX.Element {
  return (
    <button
      type="button"
      className={cn(
        'inline-flex min-h-11 items-center justify-center gap-2 rounded-xl px-4 text-[15px] font-medium select-none',
        'disabled:opacity-40',
        TONES[tone],
        block && 'w-full',
        className,
      )}
      {...rest}
    />
  )
}

/**
 * A bar button: an icon over a caption, in a fixed-width column. The bottom bar is
 * built from these, so they share one shape and the row of them reads as a unit.
 *
 * One of them can be `primary`, and exactly one is: the bar has a busiest button and
 * saying which costs a tint. It stays the same shape and the same size — the emphasis
 * is which one your eye lands on, not which one your thumb can hit.
 */
export function BarButton({
  icon,
  caption,
  primary,
  className,
  ...rest
}: React.ButtonHTMLAttributes<HTMLButtonElement> & {
  icon: React.ReactNode
  caption: string
  primary?: boolean
}): React.JSX.Element {
  return (
    <button
      type="button"
      className={cn(
        'flex min-h-14 flex-1 flex-col items-center justify-center gap-0.5 rounded-xl px-1 select-none',
        'disabled:opacity-30',
        primary
          ? 'bg-brand-50 font-medium text-brand-700 active:bg-brand-100'
          : 'text-gray-700 active:bg-gray-100',
        className,
      )}
      {...rest}
    >
      <span className="flex h-5 items-center">{icon}</span>
      <span className="text-[10.5px] leading-none tracking-normal">{caption}</span>
    </button>
  )
}
