import { atom, type Atom } from '../../../core/atom'

// The persistent half of the store primitive. The in-memory atom is shared with
// every other client (`core/atom`); this adds the localStorage mirror, which
// belongs to a browser rather than to the model.

export { atom }
export type { Atom }

/**
 * Values whose mirror is out of date, and the write that will catch them up.
 *
 * Deferred because of what the persisted atoms are: the layout holds the
 * selection, so *every* cursor move writes one — and `setItem` serialises the
 * whole value and hits the disk synchronously, inside the keystroke that caused
 * it. Holding a movement key down paid that on every press. Coalescing means a
 * burst of movement writes once, at the end.
 *
 * Only the last value per key is kept, since that is all a mirror is. The window
 * going away flushes what is outstanding, so the worst a reload can lose is the
 * fraction of a second of where the cursor had got to.
 */
const outstanding = new Map<string, unknown>()
let scheduled: ReturnType<typeof setTimeout> | null = null

/** Long enough to swallow a run of keystrokes, short enough not to be noticed. */
const WRITE_DELAY_MS = 400

function flush(): void {
  if (scheduled) {
    clearTimeout(scheduled)
    scheduled = null
  }
  for (const [key, value] of outstanding) {
    try {
      localStorage.setItem(key, JSON.stringify(value))
    } catch {
      // Best-effort: ignore quota / serialisation failures.
    }
  }
  outstanding.clear()
}

// `pagehide` rather than `unload`, which a reload does not reliably fire. Guarded
// because the state layer is also driven headlessly, where there is no window.
if (typeof addEventListener === 'function') {
  addEventListener('pagehide', flush)
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
      // Identical means the atom didn't even notify, so there is nothing to mirror.
      if (after === before) return
      outstanding.set(key, after)
      if (!scheduled) scheduled = setTimeout(flush, WRITE_DELAY_MS)
    },
  }
}

/** Write everything outstanding now. For a test, or before something drastic. */
export const flushPersisted = flush
