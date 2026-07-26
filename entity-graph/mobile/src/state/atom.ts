// The same store primitive the desktop app uses: one atom = one value, optionally
// mirrored into localStorage. No React, no context, no dependencies, so the state
// layer stays framework-free — the React bindings live in ./hooks.

export interface Atom<T> {
  get: () => T
  set: (next: T | ((prev: T) => T)) => void
  subscribe: (listener: () => void) => () => void
}

const resolve = <T,>(next: T | ((prev: T) => T), prev: T): T =>
  typeof next === 'function' ? (next as (p: T) => T)(prev) : next

/** An in-memory atom. For caches and anything that shouldn't outlive a session. */
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

/** An atom mirrored into localStorage, so its value survives a reload. */
export function persistentAtom<T>(
  key: string,
  fallback: T,
  /** Rejects a stored blob that no longer matches the current shape. */
  isValid: (parsed: unknown) => boolean = () => true,
): Atom<T> {
  const load = (): T => {
    try {
      const raw = localStorage.getItem(key)
      if (raw == null) return fallback
      const parsed = JSON.parse(raw)
      return isValid(parsed) ? (parsed as T) : fallback
    } catch {
      return fallback
    }
  }

  const inner = atom<T>(load())
  return {
    ...inner,
    set: (next) => {
      const before = inner.get()
      inner.set(next)
      const after = inner.get()
      if (after === before) return
      try {
        localStorage.setItem(key, JSON.stringify(after))
      } catch {
        // Best-effort: ignore quota and serialisation failures.
      }
    },
  }
}
