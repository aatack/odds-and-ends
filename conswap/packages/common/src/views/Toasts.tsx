import type { ReactNode } from 'react'
import type { Toast } from '../state'

interface ToastsProps {
  toasts: Toast[]
  onDismiss(id: string): void
  onRetry(id: string): void
}

export function Toasts({ toasts, onDismiss, onRetry }: ToastsProps): ReactNode {
  if (toasts.length === 0) return null
  return (
    <div className="fixed right-4 bottom-4 z-50 flex w-[320px] flex-col gap-2">
      {toasts.map((toast) => (
        <div
          key={toast.id}
          style={{ boxShadow: 'var(--shadow)' }}
          className={`rounded-lg border bg-panel px-3 py-2 ${toast.tone === 'error' ? 'border-bad/40' : 'border-line'}`}
        >
          <div className={`text-[12.5px] ${toast.tone === 'error' ? 'text-bad' : 'text-ink'}`}>{toast.text}</div>
          <div className="mt-1 flex items-center gap-3 text-[11px] text-faint">
            {toast.retry && (
              <>
                <span>nothing was lost — it is still on the server</span>
                <button onClick={() => onRetry(toast.id)} className="text-accent hover:underline">
                  try again
                </button>
              </>
            )}
            <button onClick={() => onDismiss(toast.id)} className="ml-auto hover:text-muted">
              dismiss
            </button>
          </div>
        </div>
      ))}
    </div>
  )
}
