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

/** The time, by the second: for what counts down (only those cards, so the rest of the chat isn't drawn again each second). */
export function useNow() {
  const [now, setNow] = useState(Date.now())
  useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 1_000)
    return () => clearInterval(t)
  }, [])
  return now
}
