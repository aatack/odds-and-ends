// Whether a task is open *now*. `open: true` says it is left to do; a `wait`
// value says what it is waiting on, and while every one of those is still
// waiting the task reads as done. So the walk to the next open task steps past
// what cannot be done yet, and comes back to it without anybody ticking it.
//
//   wait: { snooze: '2026-09-23T09:00:00Z' }            until then
//   wait: { toolCall: '<call id>' }                     while that call runs
//   wait: { code: 'tool.getEntity("x").values.done' }   until this says true
//
// Any number of them, in one object or a list of objects. The task opens as
// soon as *any* of them does: a snooze beside a tool call is how to say "tell me
// anyway if it has not finished by tomorrow", and a call that finishes early
// needs no snooze to run out first.

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

const conditionsOf = (wait: unknown): Condition[] => {
  const list = Array.isArray(wait) ? wait : [wait]
  return list.filter((c): c is Condition => c != null && typeof c === 'object' && !Array.isArray(c))
}

/** A snooze that cannot be read as a time has run out: better offered than lost. */
const snoozedUntil = (value: unknown): number => {
  if (typeof value === 'number') return value
  if (typeof value === 'string') return Date.parse(value)
  return NaN
}

/**
 * True while every wait condition on these values is still waiting; false once
 * one of them has stopped, or when there are none. Undefined when the answer
 * rests on a `code` condition that has yet to run.
 */
export function isWaiting(values: Record<string, unknown>, probes: WaitProbes): boolean | undefined {
  const conditions = conditionsOf(values.wait)
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

/** What {@link openNow} says while a `code` condition has yet to run. */
export const PENDING = Symbol('pending')

/**
 * `open` as it stands now: a task still waiting reads as ticked, so the walk
 * steps past it and does not look below it. Anything that is not a task is
 * handed back as written.
 */
export function openNow(values: Record<string, unknown>, probes: WaitProbes): unknown {
  if (values.open !== true) return values.open
  const waiting = isWaiting(values, probes)
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
 * A `wait` value with its snooze set to `until`, and every other condition kept:
 * snoozing a task that waits on a call still wakes it when the call ends.
 */
export function withSnooze(wait: unknown, until: string): unknown {
  const snooze = { snooze: until }
  if (Array.isArray(wait)) {
    const rest = conditionsOf(wait)
      .map(({ snooze: _, ...other }) => other)
      .filter((c) => Object.keys(c).length > 0)
    return [...rest, snooze]
  }
  const [only] = conditionsOf(wait)
  return only ? { ...only, ...snooze } : snooze
}
