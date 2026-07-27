import React from 'react'
import { cn } from '../../helpers/cn'

// A labelled control. Fields stack down a sheet, so the label sits above rather
// than beside: a phone has no width to spend on a column of labels.

export function Field({
  label,
  hint,
  children,
}: {
  label: string
  hint?: string
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <label className="flex flex-col gap-1.5">
      <span className="text-[13px] font-medium text-gray-600">{label}</span>
      {children}
      {hint && <span className="text-[12px] text-gray-400">{hint}</span>}
    </label>
  )
}

const CONTROL =
  'w-full rounded-xl bg-gray-100 px-3.5 py-3 text-gray-900 placeholder:text-gray-400 outline-none focus-visible:bg-gray-50 focus-visible:ring-2 focus-visible:ring-brand-300'

export const Input = ({
  className,
  ...rest
}: React.InputHTMLAttributes<HTMLInputElement>): React.JSX.Element => (
  <input className={cn(CONTROL, className)} {...rest} />
)

export const TextArea = ({
  className,
  ...rest
}: React.TextareaHTMLAttributes<HTMLTextAreaElement>): React.JSX.Element => (
  <textarea className={cn(CONTROL, 'min-h-24 resize-y', className)} {...rest} />
)

export const Select = ({
  className,
  ...rest
}: React.SelectHTMLAttributes<HTMLSelectElement>): React.JSX.Element => (
  <select className={cn(CONTROL, 'appearance-none', className)} {...rest} />
)
