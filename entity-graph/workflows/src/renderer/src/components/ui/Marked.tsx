import { Fragment } from 'react'

/** Plain text with `phrase` marked in yellow wherever it appears (case-insensitive, trimmed): as `Markdown`'s `highlight` does for markdown. */
export function Marked({ text, phrase }: { text: string; phrase?: string }): React.JSX.Element {
  const p = phrase?.trim().toLowerCase()
  if (!p) return <>{text}</>
  const parts: React.ReactNode[] = []
  const lower = text.toLowerCase()
  let at = 0
  for (let i = lower.indexOf(p); i >= 0; i = lower.indexOf(p, at)) {
    parts.push(<Fragment key={parts.length}>{text.slice(at, i)}</Fragment>, <mark key={parts.length + 1}>{text.slice(i, i + p.length)}</mark>)
    at = i + p.length
  }
  parts.push(<Fragment key={parts.length}>{text.slice(at)}</Fragment>)
  return <>{parts}</>
}
