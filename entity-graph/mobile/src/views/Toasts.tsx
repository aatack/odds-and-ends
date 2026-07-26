import React from 'react'
import { cn } from '../helpers/cn'
import { useToasts } from '../state/hooks'
import { dismissToast } from '../state/toast'

// Where a tool's result lands. At the top, because the bottom belongs to the bar and
// to the keyboard — and low enough not to sit under the status bar.

export function Toasts(): React.JSX.Element | null {
  const toasts = useToasts()
  if (!toasts.length) return null
  return (
    <div className="pointer-events-none fixed inset-x-0 top-[var(--inset-top)] z-50 flex flex-col items-center gap-1.5 px-3 pt-2">
      {toasts.map((t) => (
        <button
          key={t.id}
          type="button"
          onClick={() => dismissToast(t.id)}
          className={cn(
            'pointer-events-auto max-w-full rounded-xl px-3.5 py-2.5 text-left text-[13.5px] shadow-lg',
            t.kind === 'error' ? 'bg-error-50 text-error-700' : 'bg-gray-900 text-white',
          )}
        >
          {t.message}
        </button>
      ))}
    </div>
  )
}
