/**
 * The handful of primitives everything else is drawn with. Dumb components:
 * props in, gestures out.
 */

import type { ButtonHTMLAttributes, InputHTMLAttributes, ReactNode } from 'react'

export const cn = (...parts: (string | false | null | undefined)[]): string =>
  parts.filter(Boolean).join(' ')

type ButtonProps = ButtonHTMLAttributes<HTMLButtonElement> & {
  variant?: 'plain' | 'quiet' | 'accent' | 'danger'
  size?: 'sm' | 'md'
}

const VARIANTS: Record<NonNullable<ButtonProps['variant']>, string> = {
  plain: 'bg-panel text-ink shadow-xs hover:bg-sunken',
  quiet: 'bg-transparent text-muted hover:bg-line/60 hover:text-ink',
  accent: 'bg-brand-600 text-white hover:bg-brand-700',
  danger: 'bg-transparent text-danger hover:bg-danger/10',
}

export function Button({ variant = 'plain', size = 'md', className, ...rest }: ButtonProps) {
  return (
    <button
      {...rest}
      className={cn(
        'inline-flex items-center gap-1.5 rounded-md font-medium disabled:opacity-40',
        size === 'sm' ? 'h-6 px-2 text-[11px]' : 'h-7 px-2.5 text-xs',
        VARIANTS[variant],
        className,
      )}
    />
  )
}

export function Panel({ children, className }: { children: ReactNode; className?: string }) {
  return <div className={cn('flex min-h-0 flex-col bg-panel', className)}>{children}</div>
}

export function PanelHeader({ children }: { children: ReactNode }) {
  return (
    <div className="flex h-9 shrink-0 items-center gap-2 px-3 text-[11px] font-medium tracking-wide text-faint uppercase">
      {children}
    </div>
  )
}

export function TextField({ className, ...rest }: InputHTMLAttributes<HTMLInputElement>) {
  return (
    <input
      {...rest}
      className={cn(
        'h-6 w-full min-w-0 rounded bg-sunken px-1.5 text-xs text-ink placeholder:text-faint',
        'hover:bg-line/70 focus:bg-line/70',
        className,
      )}
    />
  )
}

/**
 * A number that can also be scrubbed sideways, which is what makes a
 * parametric model worth having: one drag and the whole thing re-renders.
 */
export function NumberField({
  value,
  onChange,
  step = 0.1,
  className,
}: {
  value: number
  onChange: (value: number) => void
  step?: number
  className?: string
}) {
  return (
    <input
      type="number"
      value={Number.isFinite(value) ? round(value) : 0}
      step={step}
      onChange={(event) => {
        const next = Number(event.target.value)
        if (Number.isFinite(next)) onChange(next)
      }}
      className={cn(
        'nodrag h-6 w-full min-w-0 rounded bg-sunken px-1.5 text-right text-xs text-ink',
        'hover:bg-line/70 focus:bg-line/70',
        className,
      )}
    />
  )
}

const round = (value: number): number => Math.round(value * 1e6) / 1e6

export function Empty({ children }: { children: ReactNode }) {
  return <div className="px-3 py-6 text-center text-xs text-faint">{children}</div>
}
