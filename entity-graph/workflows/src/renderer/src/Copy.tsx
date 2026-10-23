import { useState } from 'react'

/** A value to copy, with a button that copies it. */
export function Copyable({ value, display, block }: { value: string; display?: string; block?: boolean }) {
  const [copied, setCopied] = useState(false)
  return (
    <div className={`copyable ${block ? 'block' : ''}`}>
      <code>{display ?? value}</code>
      <button
        onClick={() => {
          void navigator.clipboard.writeText(value)
          setCopied(true)
          setTimeout(() => setCopied(false), 1500)
        }}
      >
        {copied ? 'Copied' : 'Copy'}
      </button>
    </div>
  )
}
