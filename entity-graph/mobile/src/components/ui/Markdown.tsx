import React, { useMemo } from 'react'
import ReactMarkdown, { type Components } from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { cn } from '../../helpers/cn'

// Entity text, rendered — the same idea as the desktop's version, and the same
// requirement: every row goes through here, so the common case (one line of prose
// with no markup in it) has to come out looking exactly as the plain span did. Same
// font, same size, no margins. The block constructs then come free on the rows that
// use them.
//
// Typography lives in `index.css` under `.markdown`, not here: the selectors it needs
// are descendant and sibling ones over elements this component doesn't render itself.
//
// Two of the desktop's plugins are left out. Maths (KaTeX) would bring a stylesheet
// and a set of font files for something you are unlikely to be reading on a phone,
// and syntax highlighting likewise — a fence renders as plain monospace here.

const COMPONENTS: Components = {
  // A link opens in a new tab rather than navigating this one, which in an installed
  // PWA would replace the app itself with whatever was linked.
  a: ({ href, children }) => (
    <a href={href} target="_blank" rel="noreferrer">
      {children}
    </a>
  ),
  // The row's own long-press has already been suppressed; an image inside one should
  // not be draggable out from under it either.
  img: ({ src, alt }) => <img src={typeof src === 'string' ? src : undefined} alt={alt ?? ''} draggable={false} />,
}

const REMARK = [remarkGfm]

export function Markdown({
  text,
  className,
  style,
}: {
  text: string
  className?: string
  style?: React.CSSProperties
}): React.JSX.Element {
  // Parsing is the expensive part of a row, and rows re-render whenever the selection
  // moves past them.
  const tree = useMemo(
    () => (
      <ReactMarkdown remarkPlugins={REMARK} components={COMPONENTS}>
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
