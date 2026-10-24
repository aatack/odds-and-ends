import React, { useMemo } from 'react'
import { Globe01 } from '@untitledui/icons'
import ReactMarkdown, { type Components } from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { cn } from '../../helpers/cn'

/** Where a link goes, as short as it reads: its host, or the whole thing if it has none. */
const hostOf = (href: string): string => {
  try {
    return new URL(href).host || href
  } catch {
    return href
  }
}

/** A href only where a tap is safe to follow: a page or a mail, never `javascript:`. */
const safeHref = (href: string | undefined): string | undefined =>
  href && (/^https?:\/\//i.test(href) || /^mailto:/i.test(href)) ? href : undefined

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
  // A link is drawn as a pill, the desktop's `LinkPill` at phone scale: the words it
  // was written with behind a mark saying it goes to the web, its host when it had no
  // words. A tap opens it in a new tab rather than navigating this one, which in an
  // installed PWA would replace the app itself with whatever was linked. No hover on a
  // phone — `active:` while the finger is down. A href only where a tap is safe to
  // follow: a `javascript:` link draws as a pill but carries no address to go to.
  a: ({ href, children }) => {
    const safe = safeHref(href)
    return (
      <a
        href={safe}
        {...(safe ? { target: '_blank', rel: 'noreferrer' } : {})}
        className={cn(
          'mx-0.5 inline-flex max-w-full items-center gap-1 rounded bg-gray-100 px-1.5 align-middle',
          'text-[0.95em] leading-5 text-gray-700 no-underline shadow-xs active:bg-gray-200',
        )}
      >
        <Globe01 size={11} className="shrink-0 text-gray-400" />
        <span className="truncate">{children || hostOf(href ?? '')}</span>
      </a>
    )
  },
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
