// Times and durations as the app writes them, and a clock for what counts down.

import { useEffect, useState } from 'react'

export const time = (ms: number) => new Date(ms).toLocaleString()
/** When something was, as short as says it: 18:32 today, Friday 18:32 within the week, 17 August 17:12 before that (and its year, from another). */
export const shortTime = (ms: number) => {
  const at = new Date(ms)
  const now = new Date()
  const clock = at.toLocaleTimeString('en-GB', { hour: '2-digit', minute: '2-digit', hour12: false })
  const midnight = new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime()
  if (ms >= midnight) return clock
  if (ms >= midnight - 6 * 86_400_000) return `${at.toLocaleDateString('en-GB', { weekday: 'long' })} ${clock}`
  const year = at.getFullYear() === now.getFullYear() ? undefined : ('numeric' as const)
  return `${at.toLocaleDateString('en-GB', { day: 'numeric', month: 'long', year })} ${clock}`
}
/** A duration to the second: 45s, 3m 07s, 2h 05m. */
export const minutes = (ms: number) => {
  const s = Math.floor(ms / 1000)
  const pad = (n: number) => String(n).padStart(2, '0')
  if (s < 60) return `${s}s`
  if (s < 3600) return `${Math.floor(s / 60)}m ${pad(s % 60)}s`
  return `${Math.floor(s / 3600)}h ${pad(Math.floor(s / 60) % 60)}m`
}

// One clock for everything that counts, ticking here (never asking the backend): each count is worked out from the
// time it counts from, which the backend gave once.
const ticking = new Set<(now: number) => void>()
let clock: ReturnType<typeof setInterval> | null = null
function tick(fn: (now: number) => void): () => void {
  ticking.add(fn)
  clock ??= setInterval(() => {
    const now = Date.now()
    for (const f of ticking) f(now)
  }, 1_000)
  return () => {
    ticking.delete(fn)
    if (!ticking.size && clock) {
      clearInterval(clock)
      clock = null
    }
  }
}

/** The time, by the second: for what counts (only those parts, so the rest isn't drawn again each second). */
export function useNow() {
  const [now, setNow] = useState(Date.now())
  useEffect(() => tick(setNow), [])
  return now
}

/** How long since `from`, counting up by the second: `45s`, `3m 07s`. */
export function Elapsed({ from }: { from: number }) {
  const now = useNow()
  return <>{minutes(Math.max(0, now - from))}</>
}
