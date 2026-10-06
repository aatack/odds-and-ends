/** `14:03` today, `Mon` this week, `3 Oct` this year, else `3 Oct 2024`. */
export function shortTime(ts: string | undefined, now = new Date()): string {
  if (!ts) return ''
  const date = new Date(Number(ts) * 1000)
  const days = (now.getTime() - date.getTime()) / 86_400_000
  if (date.toDateString() === now.toDateString()) {
    return date.toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit', hourCycle: 'h23' })
  }
  if (days < 6) return date.toLocaleDateString(undefined, { weekday: 'short' })
  if (date.getFullYear() === now.getFullYear()) return date.toLocaleDateString(undefined, { day: 'numeric', month: 'short' })
  return date.toLocaleDateString(undefined, { day: 'numeric', month: 'short', year: 'numeric' })
}

/**
 * Name colours, picked to stay apart from each other and readable as bold
 * text on the light canvas.
 */
const authorColours = [
  '#c62828', // red
  '#d84315', // orange
  '#b45309', // amber
  '#8a7300', // olive
  '#558b2f', // lime
  '#2e7d32', // green
  '#00796b', // teal
  '#00838f', // cyan
  '#0277bd', // sky
  '#1565c0', // blue
  '#3949ab', // indigo
  '#5e35b1', // violet
  '#8e24aa', // purple
  '#ad1457', // magenta
  '#d81b60', // pink
  '#6d4c41', // brown
  '#546e7a', // slate
]

/** A fixed colour per user id (FNV-1a, so similar ids still spread out). */
export function authorColour(key: string): string {
  let hash = 0x811c9dc5
  for (const char of key) hash = Math.imul(hash ^ char.charCodeAt(0), 0x01000193) >>> 0
  return authorColours[hash % authorColours.length]
}

/** `Monday 5 October 2026, 10:22:14`, for the tooltip on a time. */
export function fullTime(ts: string): string {
  return new Date(Number(ts) * 1000).toLocaleString(undefined, {
    weekday: 'long',
    day: 'numeric',
    month: 'long',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
    hourCycle: 'h23',
  })
}

/** `5 Oct, 07:52`: where loaded history starts. */
export function cursorTime(ts: string): string {
  return new Date(Number(ts) * 1000).toLocaleString(undefined, {
    day: 'numeric',
    month: 'short',
    hour: '2-digit',
    minute: '2-digit',
    hourCycle: 'h23',
  })
}
