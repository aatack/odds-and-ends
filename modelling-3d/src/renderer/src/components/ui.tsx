/**
 * The handful of primitives everything else is drawn with. Dumb components:
 * props in, gestures out.
 */

import { forwardRef, useEffect, useState } from 'react'
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

export const TextField = forwardRef<HTMLInputElement, InputHTMLAttributes<HTMLInputElement>>(
  function TextField({ className, ...rest }, ref) {
    return (
      <input
        ref={ref}
        {...rest}
        className={cn(
          'h-6 w-full min-w-0 rounded bg-sunken px-1.5 text-xs text-ink placeholder:text-faint',
          'hover:bg-line/70 focus:bg-line/70',
          className,
        )}
      />
    )
  },
)

/**
 * A number, kept as text while it is being typed.
 *
 * The field holds a draft rather than the value, so clearing it leaves it
 * *empty* instead of snapping to zero, and half-typed things — `-`, `1.`,
 * `1e` — survive long enough to be finished. Empty reads as zero downstream,
 * and the draft is only thrown away when the value changes from elsewhere.
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
  const shown = Number.isFinite(value) ? String(round(value)) : '0'
  const [draft, setDraft] = useState<string | null>(null)
  const [committed, setCommitted] = useState(shown)

  // A change from outside — another node, an undone edit — wins over a draft
  // that is only a different spelling of the same number.
  useEffect(() => {
    if (shown !== committed) {
      setCommitted(shown)
      setDraft(null)
    }
  }, [shown, committed])

  return (
    <input
      type="text"
      inputMode="decimal"
      value={draft ?? shown}
      step={step}
      onChange={(event) => {
        const text = event.target.value
        setDraft(text)
        const next = text.trim() === '' ? 0 : Number(text)
        if (Number.isFinite(next)) {
          setCommitted(String(round(next)))
          onChange(next)
        }
      }}
      onBlur={() => setDraft(null)}
      className={cn(
        'nodrag h-6 w-full min-w-0 rounded bg-sunken px-1.5 text-right text-xs text-ink tabular-nums',
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
