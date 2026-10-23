import React, { useMemo } from 'react'
import ReactMarkdown, { type Components } from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { cn } from '../../helpers/cn'
import { CodeBlock } from './CodeBlock'

// Text, rendered: every message, description and code sample in the app goes
// through here. A line of prose with no markup in it comes out as it would as a
// plain span (no margins), so it can stand anywhere text did. Pared down from
// entity-graph's (no maths, no custom fields); the typography is under
// `.markdown` in `styles.css`.

/** The text inside an element the renderer built, ignoring anything nested. */
const plainText = (node: React.ReactNode): string =>
  React.Children.toArray(node)
    .map((child) => (typeof child === 'string' ? child : ''))
    .join('')

/** A fenced block: its info string is the Prism language. */
const fence: Components['pre'] = ({ children }) => {
  const code = React.Children.toArray(children).find(
    (child): child is React.ReactElement<{ className?: string; children?: React.ReactNode }> => React.isValidElement(child),
  )
  const language = /language-([\w-]+)/.exec(code?.props.className ?? '')?.[1]
  return <CodeBlock code={plainText(code?.props.children).replace(/\n$/, '')} language={language} />
}

const COMPONENTS: Components = {
  pre: fence,
  // Links open in the browser: the window's open handler sends http(s) there.
  a: ({ href, children }) => (
    <a href={href} target="_blank" rel="noreferrer">
      {children}
    </a>
  ),
}

const REMARK = [remarkGfm]

export function Markdown({ text, className }: { text: string; className?: string }): React.JSX.Element {
  const tree = useMemo(
    () => (
      <ReactMarkdown remarkPlugins={REMARK} components={COMPONENTS}>
        {text}
      </ReactMarkdown>
    ),
    [text],
  )
  return <div className={cn('markdown', className)}>{tree}</div>
}

/** Source as a fenced block, for code that is not inside any prose. */
export const fenced = (code: string, language = ''): string => {
  const ticks = '`'.repeat(Math.max(3, ...[...code.matchAll(/`+/g)].map((m) => m[0].length + 1)))
  return `${ticks}${language}\n${code}\n${ticks}`
}
