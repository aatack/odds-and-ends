/** `14:03` today, `Mon` this week, `3 Oct` this year, else `3 Oct 2024`. */
export function shortTime(ts: string | undefined, now = new Date()): string {
  if (!ts) return ''
  const date = new Date(Number(ts) * 1000)
  const days = (now.getTime() - date.getTime()) / 86_400_000
  if (date.toDateString() === now.toDateString()) {
    return date.toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit' })
  }
  if (days < 6) return date.toLocaleDateString(undefined, { weekday: 'short' })
  if (date.getFullYear() === now.getFullYear()) return date.toLocaleDateString(undefined, { day: 'numeric', month: 'short' })
  return date.toLocaleDateString(undefined, { day: 'numeric', month: 'short', year: 'numeric' })
}
