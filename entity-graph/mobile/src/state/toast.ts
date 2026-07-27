import { atom } from './atom'

// What a tool has to say for itself, once it has run. Runtime only.
//
// Errors surface here rather than being raised by components: a tool throws, the
// dispatcher settles it, and this is where the message lands. Components never
// raise a toast of their own, so there is one place to look for "why didn't that
// work" — which matters more on a phone, where there is no console to open.

export interface Toast {
  id: number
  kind: 'info' | 'error'
  message: string
}

export const toastsAtom = atom<Toast[]>([])

let nextId = 1

/** How long a toast stays up. Errors linger — they are worth reading. */
const LIFETIME = { info: 2200, error: 6000 }

export function toast(message: string, kind: 'info' | 'error' = 'info'): void {
  const id = nextId++
  toastsAtom.set((ts) => [...ts, { id, kind, message }])
  setTimeout(() => dismissToast(id), LIFETIME[kind])
}

export const dismissToast = (id: number): void =>
  toastsAtom.set((ts) => (ts.some((t) => t.id === id) ? ts.filter((t) => t.id !== id) : ts))
