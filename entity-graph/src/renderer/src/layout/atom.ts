import { useSyncExternalStore } from 'react'

// A tiny global store backed by localStorage. One atom = one JSON blob under
// `key`. Components read it reactively with useAtom; code outside React (an
// action registry, say) can read and write it through get/set. Deliberately
// minimal — no context, no dependencies — because the layout is the only piece
// of genuinely global, persisted UI state we have.

export interface Atom<T> {
  get: () => T
  set: (next: T | ((prev: T) => T)) => void
  subscribe: (listener: () => void) => () => void
}

export function persistentAtom<T>(
  key: string,
  fallback: T,
  /** Rejects a stored blob that no longer matches the current shape. */
  isValid: (parsed: unknown) => boolean = () => true,
): Atom<T> {
  const listeners = new Set<() => void>()

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

  let value: T = load()

  return {
    get: () => value,
    set: (next) => {
      value = typeof next === 'function' ? (next as (prev: T) => T)(value) : next
      try {
        localStorage.setItem(key, JSON.stringify(value))
      } catch {
        // Best-effort: ignore quota / serialisation failures.
      }
      listeners.forEach((l) => l())
    },
    subscribe: (l) => {
      listeners.add(l)
      return () => listeners.delete(l)
    },
  }
}

export function useAtom<T>(atom: Atom<T>): [T, Atom<T>['set']] {
  const value = useSyncExternalStore(atom.subscribe, atom.get, atom.get)
  return [value, atom.set]
}
