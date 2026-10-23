import { useState } from 'react'
import { fenced, Markdown } from './components/ui/Markdown'

/** A value to copy, with a button that copies it. A block is a code sample, in `language`, drawn as one. */
export function Copyable({ value, display, block, language }: { value: string; display?: string; block?: boolean; language?: string }) {
  const [copied, setCopied] = useState(false)
  return (
    <div className={`copyable ${block ? 'block' : ''}`}>
      {block ? <Markdown text={fenced(display ?? value, language)} /> : <code>{display ?? value}</code>}
      <button data-ui
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
