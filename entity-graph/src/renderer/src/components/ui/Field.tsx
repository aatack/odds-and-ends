import type { ReactNode } from 'react'

// A labelled form control: a quiet micro-label above its input.
export function Field({
  label,
  children,
}: {
  label: ReactNode
  children: ReactNode
}): React.JSX.Element {
  return (
    <label className="block space-y-1">
      <span className="block text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
        {label}
      </span>
      {children}
    </label>
  )
}
