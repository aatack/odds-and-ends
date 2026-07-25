import React, { useMemo } from 'react'
import ReactMarkdown, { type Components } from 'react-markdown'
import rehypeKatex from 'rehype-katex'
import remarkGfm from 'remark-gfm'
import remarkMath from 'remark-math'
import type { PluggableList } from 'unified'
// KaTeX's own stylesheet, and with it the font files it references — bundled by
// vite like the app's other fonts, since nothing here may reach the network.
import 'katex/dist/katex.min.css'
import { CodeBlock } from './CodeBlock'
import { cn } from '../../helpers/cn'

// Entity text, rendered. Every row goes through here, so the common case — one
// line of prose with no markup in it — has to come out looking exactly as it did
// when it was a plain span: same font, same size, no margins. The block
// constructs (lists, quotes, tables, fences, headings) then come for free on the
// rows that use them.
//
// Typography lives in `index.css` under `.markdown`, not here: the descendant
// selectors it needs (a paragraph's spacing depends on having a sibling) can't be
// expressed as utility classes on elements this component doesn't render itself.

/** The text inside an element the renderer built, ignoring anything nested. */
const plainText = (node: React.ReactNode): string =>
  React.Children.toArray(node)
    .map((child) => (typeof child === 'string' ? child : ''))
    .join('')

/**
 * A fenced block. The info string is the Prism language, so ```` ```python ````
 * highlights as Python; a bare fence is left as TypeScript, which is what the
 * app's own code entities are. The fence's `code` child carries both.
 */
const fence: Components['pre'] = ({ children }) => {
  const code = React.Children.toArray(children).find(
    (child): child is React.ReactElement<{ className?: string; children?: React.ReactNode }> =>
      React.isValidElement(child),
  )
  const language = /language-([\w-]+)/.exec(code?.props.className ?? '')?.[1]
  // The parser leaves the fence's closing newline on the text; a code block that
  // ends in a blank line is a distraction.
  return <CodeBlock code={plainText(code?.props.children).replace(/\n$/, '')} language={language} />
}

const COMPONENTS: Components = {
  pre: fence,
  // Links open in the browser rather than navigating the app out of existence.
  a: ({ href, children }) => (
    <a href={href} target="_blank" rel="noreferrer">
      {children}
    </a>
  ),
}

// `$x$` inline and `$$x$$` on its own line, turned into markup by KaTeX rather
// than being left to a browser that has no idea what to do with them.
const REMARK = [remarkGfm, remarkMath]
// Malformed maths renders as its own red source rather than throwing: a row is
// typed a character at a time, and half an expression is not an error.
const REHYPE: PluggableList = [[rehypeKatex, { throwOnError: false }]]

export function Markdown({
  text,
  className,
  style,
}: {
  text: string
  className?: string
  style?: React.CSSProperties
}): React.JSX.Element {
  // The parse is the expensive part of a row, and rows re-render whenever the
  // selection moves past them.
  const tree = useMemo(
    () => (
      <ReactMarkdown remarkPlugins={REMARK} rehypePlugins={REHYPE} components={COMPONENTS}>
        {text}
      </ReactMarkdown>
    ),
    [text],
  )
  return (
    <div className={cn('markdown', className)} style={style}>
      {tree}
    </div>
  )
}
