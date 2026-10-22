import React from 'react'
import { Globe01 } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { urlEntityId } from '../../../core/builtins'
import { peekHandlers } from './peekHover'

/** Where a link goes, as short as it reads: its host, or the whole thing if it has none. */
const hostOf = (href: string): string => {
  try {
    return new URL(href).host || href
  } catch {
    return href
  }
}

/** Only a page on the web is worth a peek: a `mailto:` has nothing to show. */
const peekable = (href: string): boolean => /^https?:\/\//i.test(href)

/**
 * A link in an entity's text, drawn as a pill like an entity is: the words it was
 * written with, behind a mark saying it goes to the web. Hovering a page opens a
 * peek onto it.
 *
 * Still an anchor, because what a click does with a link is decided once, in
 * `App`: a plain click does nothing, and ⌘/Ctrl-click opens it in the browser.
 */
export function LinkPill({
  href,
  children,
}: {
  href: string
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <a
      href={href}
      title={peekable(href) ? undefined : href}
      className={cn(
        'mx-0.5 inline-flex max-w-full items-center gap-1 rounded bg-gray-100 px-1.5 align-middle',
        'text-[0.95em] leading-5 text-gray-700 no-underline shadow-xs hover:bg-gray-200',
      )}
      {...peekHandlers(peekable(href) ? urlEntityId(href) : null)}
    >
      <Globe01 size={11} className="shrink-0 text-gray-400" />
      <span className="truncate">{children || hostOf(href)}</span>
    </a>
  )
}
