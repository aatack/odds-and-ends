import { atom, type Atom } from '../../../core/atom'

// The persistent half of the store primitive. The in-memory atom is shared with
// every other client (`core/atom`); this adds the localStorage mirror, which
// belongs to a browser rather than to the model.

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
        // Best-effort: ignore quota / serialisation failures.
      }
    },
  }
}
