import { useSyncExternalStore } from 'react'

// The activity log: a module-level store (like the toast/reload buses) recording
// the parameterised commands run from the command palette, and — crucially — the
// ones abandoned mid-wizard. A cancelled entry keeps the half-entered values so
// its wizard can be reopened and resumed. Only field-bearing commands are logged;
// bare navigation commands would just be noise.

export type ActionStatus = 'success' | 'error' | 'cancelled'

export interface ActionLogEntry {
  /** Stable id; a resumed-then-recancelled wizard reuses it to update in place. */
  key: string
  /** The command's id, so a resume/rerun can re-find it. */
  commandId: string
  /** The command's label, shown in the log. */
  title: string
  status: ActionStatus
  at: number
  error: string | null
  /** The entered (or half-entered) field values, keyed by field name. */
  values: Record<string, string>
}

const MAX_LOG = 200

let entries: ActionLogEntry[] = []
const listeners = new Set<() => void>()
const emit = (): void => listeners.forEach((l) => l())

const cap = (list: ActionLogEntry[]): ActionLogEntry[] => list.slice(0, MAX_LOG)

/**
 * Record a completed command run (newest first). Keyed by `key`: completing a
 * resumed action reuses its cancelled entry's key, so it updates that entry in
 * place rather than leaving a stale "cancelled" duplicate behind.
 */
export function logAction(entry: Omit<ActionLogEntry, 'at'> & { at?: number }): void {
  const full: ActionLogEntry = { ...entry, at: entry.at ?? Date.now() }
  entries = entries.some((e) => e.key === full.key)
    ? entries.map((e) => (e.key === full.key ? full : e))
    : cap([full, ...entries])
  emit()
}

/**
 * Record a command abandoned from the palette, keyed by the id its wizard was
 * given when it opened. Resuming then re-cancelling reuses that key, so the
 * existing entry is updated in place rather than duplicated.
 */
export function logCancelled(
  key: string,
  command: { id: string; title: string },
  values: Record<string, string>,
): void {
  const existing = entries.some((e) => e.key === key)
  if (existing) {
    entries = entries.map((e) =>
      e.key === key ? { ...e, status: 'cancelled', values, at: Date.now() } : e,
    )
  } else {
    entries = cap([
      {
        key,
        commandId: command.id,
        title: command.title,
        status: 'cancelled',
        at: Date.now(),
        error: null,
        values,
      },
      ...entries,
    ])
  }
  emit()
}

export function clearActionLog(): void {
  if (entries.length === 0) return
  entries = []
  emit()
}

export function useActionLog(): ActionLogEntry[] {
  return useSyncExternalStore(
    (l) => {
      listeners.add(l)
      return () => listeners.delete(l)
    },
    () => entries,
    () => entries,
  )
}
