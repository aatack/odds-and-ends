import { useSyncExternalStore } from 'react'

// A tiny global "reload" signal. Mutations made outside a given editor's own
// useEditor instance — e.g. a create/rename run from the command palette against
// an arbitrary entity id — bump this version, and every mounted editor re-queries
// in place. Mirrors the toast store's module-level pub/sub, so callers don't have
// to thread a refresh callback down through the layout tree.

let version = 0
const listeners = new Set<() => void>()

/** Ask every mounted editor to re-query its tree. */
export function emitReload(): void {
  version += 1
  listeners.forEach((l) => l())
}

/** A value that changes each time {@link emitReload} is called. */
export function useReloadSignal(): number {
  return useSyncExternalStore(
    (l) => {
      listeners.add(l)
      return () => listeners.delete(l)
    },
    () => version,
    () => version,
  )
}
