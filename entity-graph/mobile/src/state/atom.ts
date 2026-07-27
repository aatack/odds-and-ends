import { atom, type Atom } from '../../../src/core/atom'

// The store primitive: one atom = one value, optionally mirrored into
// localStorage. The in-memory half is shared with the desktop app — it is the
// same twenty lines, and the cache they both use is built on it. This adds the
// persistent variant, which needs a browser and so isn't part of the model.

export { atom }
export type { Atom }

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
