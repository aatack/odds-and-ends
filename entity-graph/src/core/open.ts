// Whether a task is open *now*. `open: true` says it is left to do and `false`
// that it is done. `open` may instead say what the task is waiting on, and while
// every one of those is still waiting the task reads as done. So the walk to the
// next open task steps past what cannot be done yet, and comes back to it
// without anybody ticking it.
//
//   open: { snooze: '2026-09-23T09:00:00Z' }            until then
//   open: { toolCall: '<call id>' }                     while that call runs
//   open: { code: 'tool.getEntity("x").values.done' }   until this says true
//
// Any number of them, in one object or a list of objects. The task opens as
// soon as *any* of them does: a snooze beside a tool call is how to say "tell me
// anyway if it has not finished by tomorrow", and a call that finishes early
// needs no snooze to run out first. Drawn, a waiting task is an unticked one.

/** What a wait condition needs from outside the entity. */
export interface WaitProbes {
  now: number
  /** True while the call with this id is still running. */
  running: (callId: string) => boolean
  /**
   * What a `code` condition returned, or undefined when it has yet to run. Only
   * asked once nothing cheaper has opened the task.
   */
  code: (code: string) => { result: unknown } | undefined
}

interface Condition {
  snooze?: unknown
  toolCall?: unknown
  code?: unknown
}

const isCondition = (c: unknown): c is Condition =>
  c != null && typeof c === 'object' && !Array.isArray(c)

/** True when `open` says what a task waits on, rather than `true` or `false`. */
export const waits = (open: unknown): boolean => isCondition(open) || Array.isArray(open)

const conditionsOf = (open: unknown): Condition[] =>
  (Array.isArray(open) ? open : [open]).filter(isCondition)

/**
 * `open` as a checkbox: unticked for a task left to do, whatever it waits on;
 * ticked for one done; undefined for anything that is not a task.
 */
export function checkboxOf(open: unknown): boolean | undefined {
  if (open === true || waits(open)) return true
  if (open === false) return false
  return undefined
}

/** A snooze that cannot be read as a time has run out: better offered than lost. */
const snoozedUntil = (value: unknown): number => {
  if (typeof value === 'number') return value
  if (typeof value === 'string') return Date.parse(value)
  return NaN
}

/**
 * True while every condition in `open` is still waiting; false once one of them
 * has stopped, or when there are none. Undefined when the answer
 * rests on a `code` condition that has yet to run.
 */
export function isWaiting(open: unknown, probes: WaitProbes): boolean | undefined {
  const conditions = conditionsOf(open)
  const code: string[] = []
  let any = false
  for (const c of conditions) {
    if (c.snooze != null) {
      any = true
      const until = snoozedUntil(c.snooze)
      if (!(probes.now < until)) return false
    }
    if (typeof c.toolCall === 'string') {
      any = true
      if (!probes.running(c.toolCall)) return false
    }
    if (typeof c.code === 'string') {
      any = true
      code.push(c.code)
    }
  }
  if (!any) return false
  let unknown = false
  for (const body of code) {
    const ran = probes.code(body)
    if (!ran) unknown = true
    else if (ran.result) return false
  }
  return unknown ? undefined : true
}

/**
 * True while a snooze or a tool call is keeping the task shut: it has at least
 * one of those, and nothing has opened it yet. A `code` condition is taken as
 * not having said, since drawing a row is no reason to run one — so a task that
 * waits on code alone is never paused in this sense.
 */
export function isPaused(open: unknown, probes: Omit<WaitProbes, 'code'>): boolean {
  const timed = conditionsOf(open).some((c) => c.snooze != null || typeof c.toolCall === 'string')
  return timed && isWaiting(open, { ...probes, code: () => undefined }) !== false
}

/** The soonest snooze in `open` still to run out after `now`, if any. */
export function nextWake(open: unknown, now: number): number | undefined {
  let soonest: number | undefined
  for (const c of conditionsOf(open)) {
    if (c.snooze == null) continue
    const until = snoozedUntil(c.snooze)
    if (until > now && (soonest === undefined || until < soonest)) soonest = until
  }
  return soonest
}

/** What {@link openNow} says while a `code` condition has yet to run. */
export const PENDING = Symbol('pending')

/**
 * `open` as it stands now: `true` or `false` for a task that waits on something,
 * so the walk steps past one still waiting and does not look below it. Anything
 * else is handed back as written.
 */
export function openNow(open: unknown, probes: WaitProbes): unknown {
  if (!waits(open)) return open
  const waiting = isWaiting(open, probes)
  if (waiting === undefined) return PENDING
  return !waiting
}

const UNIT_MS: Record<string, number> = { h: 3_600_000, d: 86_400_000, w: 604_800_000 }

/** `3d`, `1w`, `12h` as milliseconds; null when it is not one of those. */
export function parseDuration(text: string): number | null {
  const match = /^\s*(\d+(?:\.\d+)?)\s*([hdw])\s*$/i.exec(text)
  if (!match) return null
  return Number(match[1]) * UNIT_MS[match[2].toLowerCase()]
}

/**
 * An `open` value snoozed until `until`. Whatever else it waits on is kept, so
 * snoozing a task that waits on a call still wakes it when the call ends; `true`,
 * `false` or nothing at all become a task waiting on the snooze alone.
 */
export function withSnooze(open: unknown, until: string): unknown {
  const snooze = { snooze: until }
  if (Array.isArray(open)) {
    const rest = conditionsOf(open)
      .map(({ snooze: _, ...other }) => other)
      .filter((c) => Object.keys(c).length > 0)
    return [...rest, snooze]
  }
  return isCondition(open) ? { ...open, ...snooze } : snooze
}
