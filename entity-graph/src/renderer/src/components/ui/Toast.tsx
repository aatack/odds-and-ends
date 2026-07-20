import React, { useSyncExternalStore } from 'react'
import { v4 as uuid } from 'uuid'
import { cn } from '../../helpers/cn'

// A tiny global toast store. Transient messages (export confirmations, errors)
// auto-dismiss; sticky ones (a pending move/link prompt) stay until dismissed by
// id. Rendered once by <Toaster/> in the app shell, so any code — including the
// per-frame editors — can surface a message without threading state upward.

export type ToastVariant = 'info' | 'success' | 'error'

export interface Toast {
  id: string
  message: string
  variant: ToastVariant
  sticky?: boolean
}

const DISMISS_MS = 2000

let toasts: Toast[] = []
const listeners = new Set<() => void>()
const emit = (): void => listeners.forEach((l) => l())

/** Show (or, with a fixed `id`, update) a toast. Returns its id. */
export function showToast(t: { message: string; variant?: ToastVariant; sticky?: boolean; id?: string }): string {
  const id = t.id ?? uuid()
  const next: Toast = { id, message: t.message, variant: t.variant ?? 'info', sticky: t.sticky }
  toasts = toasts.some((x) => x.id === id) ? toasts.map((x) => (x.id === id ? next : x)) : [...toasts, next]
  emit()
  if (!next.sticky) window.setTimeout(() => dismissToast(id), DISMISS_MS)
  return id
}

export function dismissToast(id: string): void {
  const before = toasts.length
  toasts = toasts.filter((t) => t.id !== id)
  if (toasts.length !== before) emit()
}

function useToasts(): Toast[] {
  return useSyncExternalStore(
    (l) => {
      listeners.add(l)
      return () => listeners.delete(l)
    },
    () => toasts,
    () => toasts,
  )
}

const VARIANT: Record<ToastVariant, string> = {
  info: 'bg-brand-50 text-brand-700 border-brand-100',
  success: 'bg-success-50 text-success-700 border-success-100',
  error: 'bg-error-50 text-error-700 border-error-100',
}

/** The one toast layer; mount near the app root. */
export function Toaster(): React.JSX.Element | null {
  const items = useToasts()
  if (items.length === 0) return null
  return (
    <div className="pointer-events-none fixed bottom-4 right-4 z-50 flex w-72 flex-col gap-2">
      {items.map((t) => (
        <button
          key={t.id}
          onClick={() => dismissToast(t.id)}
          className={cn(
            'pointer-events-auto rounded-md border px-3 py-2 text-left text-[13px] shadow-sm',
            VARIANT[t.variant],
          )}
        >
          {t.message}
        </button>
      ))}
    </div>
  )
}
