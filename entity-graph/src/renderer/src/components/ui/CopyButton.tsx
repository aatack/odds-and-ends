import { useEffect, useRef, useState } from 'react'
import { Check, Copy01 } from '@untitledui/icons'
import { IconButton } from './IconButton'

/**
 * Copy a string, and say so for a moment. The tick is the only feedback, so it
 * has to last long enough to be read — but not past the component's life, hence
 * the cleared timer.
 */
export function CopyButton({
  value,
  title = 'Copy',
  icon,
}: {
  value: string
  title?: string
  /** Something other than the clipboard glyph, where what is copied has its own. */
  icon?: React.ReactNode
}): React.JSX.Element {
  const [copied, setCopied] = useState(false)
  const timer = useRef<ReturnType<typeof setTimeout>>()

  useEffect(() => () => clearTimeout(timer.current), [])

  const copy = async (): Promise<void> => {
    await navigator.clipboard.writeText(value)
    setCopied(true)
    clearTimeout(timer.current)
    timer.current = setTimeout(() => setCopied(false), 1200)
  }

  return (
    <IconButton title={copied ? 'Copied!' : title} onClick={() => void copy()}>
      {copied ? <Check size={16} /> : (icon ?? <Copy01 size={16} />)}
    </IconButton>
  )
}
