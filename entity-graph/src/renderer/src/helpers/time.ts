const UNITS: Array<[Intl.RelativeTimeFormatUnit, number]> = [
  ['year', 365 * 24 * 60 * 60 * 1000],
  ['month', 30 * 24 * 60 * 60 * 1000],
  ['day', 24 * 60 * 60 * 1000],
  ['hour', 60 * 60 * 1000],
  ['minute', 60 * 1000],
]

const rtf = new Intl.RelativeTimeFormat('en', { numeric: 'auto' })

/** A short "3 minutes ago" style label for a past timestamp. */
export function relativeTime(from: number, now: number = Date.now()): string {
  const diff = from - now
  for (const [unit, ms] of UNITS) {
    if (Math.abs(diff) >= ms) return rtf.format(Math.round(diff / ms), unit)
  }
  return 'just now'
}

const pad = (n: number): string => String(n).padStart(2, '0')

/**
 * How long something has been going: `0:07`, `4:31`, `1:02:00` past the hour.
 * Counted rather than described, which is what a clock you are watching wants —
 * "3 minutes ago" is the same answer for three minutes at a stretch.
 */
export function elapsedTime(ms: number): string {
  const total = Math.max(0, Math.floor(ms / 1000))
  const hours = Math.floor(total / 3600)
  const minutes = Math.floor(total / 60) % 60
  const seconds = total % 60
  return hours ? `${hours}:${pad(minutes)}:${pad(seconds)}` : `${minutes}:${pad(seconds)}`
}
