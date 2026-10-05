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

/** Slack-like name colours, fixed per person. */
const authorColours = ['#8e3f8b', '#2b9e6e', '#1d74b5', '#c05b16', '#b8335a', '#5b4fc4', '#1f8a8a', '#7a6a12']

export function authorColour(key: string): string {
  let hash = 0
  for (const char of key) hash = (hash * 31 + char.charCodeAt(0)) >>> 0
  return authorColours[hash % authorColours.length]
}
