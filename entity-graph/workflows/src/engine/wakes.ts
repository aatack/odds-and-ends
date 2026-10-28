// When something wakes: one rule, for bots and scripts alike. It wakes once something affected it (anything but itself)
// since it last woke, and that has settled for its debounce; or once its cycle has passed since it last woke, while it
// has work (tasks assigned to it), news or not. Never while it is running: the caller sees to that.

export interface WakeState {
  /** When it last woke (a run began); 0 if never. */
  lastWake: number
  /** When it was last affected since it last woke, by anything but itself; null if nothing has. */
  edited: number | null
  debounceMs: number
  /** How often it wakes while it has work; null: only when affected. */
  cycleMs: number | null
  /** It has tasks assigned to it. */
  hasWork: boolean
}

/** When it is due: (edited and settled) or (a cycle on, with work). Null: nothing calls for it. */
export function dueAt(s: WakeState): number | null {
  const due: number[] = []
  if (s.edited !== null) due.push(s.edited + s.debounceMs)
  if (s.hasWork && s.cycleMs) due.push(s.lastWake + s.cycleMs)
  return due.length ? Math.min(...due) : null
}
