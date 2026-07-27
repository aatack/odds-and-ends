// A tiny store primitive. One atom = one value; code reads and writes through
// get/set, and anything watching subscribes.
//
// Deliberately minimal — no context, no dependencies, no React, and nothing that
// only a browser has — so a state layer built on it stays framework-free and can
// be driven headlessly. Each client's own `state/atom` adds the persistent
// variant, which needs localStorage and therefore doesn't belong here.

export interface Atom<T> {
  get: () => T
  set: (next: T | ((prev: T) => T)) => void
  subscribe: (listener: () => void) => () => void
}

const resolve = <T,>(next: T | ((prev: T) => T), prev: T): T =>
  typeof next === 'function' ? (next as (p: T) => T)(prev) : next

/** An in-memory atom. Use for caches and anything that should not outlive a session. */
export function atom<T>(initial: T): Atom<T> {
  const listeners = new Set<() => void>()
  let value = initial
  return {
    get: () => value,
    set: (next) => {
      const resolved = resolve(next, value)
      if (resolved === value) return
      value = resolved
      listeners.forEach((l) => l())
    },
    subscribe: (l) => {
      listeners.add(l)
      return () => listeners.delete(l)
    },
  }
}
