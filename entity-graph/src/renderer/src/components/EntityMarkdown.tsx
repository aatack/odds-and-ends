import React from 'react'
import { Markdown, type LinkRenderer } from './ui/Markdown'
import { LinkPill } from './LinkPill'
import { PillBackground, PillContent, PillWrapper } from './EntityPill'
import { PILL_MAX_WIDTH } from './types/types'
import { cn } from '../helpers/cn'
import type { MarkdownFieldProps, MarkdownFields } from './ui/markdownFields'
import { useGetEntities } from '../state/hooks'

// An entity's text, rendered — with the custom inline forms wired up. `Markdown`
// itself knows nothing about entities; this is where the two meet, and it is the
// component the outliner uses.

/** The inline forms entity text can carry. A constant: its identity is a memo key. */
const FIELDS: MarkdownFields = {
  entity: ({ arg, text }: MarkdownFieldProps) => <FieldEntity id={arg} fallback={text} />,
}

export function EntityMarkdown({
  text,
  className,
  style,
  highlight,
}: {
  text: string
  className?: string
  style?: React.CSSProperties
  /** The frame's find text, marked wherever the row says it. */
  highlight?: string
}): React.JSX.Element {

  return (
    <Markdown
      text={text}
      className={className}
      style={style}
      fields={FIELDS}
      highlight={highlight}
      link={renderLink}
    />
  )
}

/** Every link in an entity's text is a pill, which peeks at the page. */
const renderLink: LinkRenderer = ({ href, children }) => <LinkPill href={href}>{children}</LinkPill>

/**
 * `[@entity:id](label)` — another entity, mentioned in the middle of a sentence,
 * as a pill that can be right-clicked and followed like any other.
 *
 * The label is a *fallback* rather than the caption: an entity with text of its
 * own shows that, so a note renamed is renamed everywhere it is mentioned, which
 * is the whole reason a mention names an id rather than spelling out a name. The
 * fallback is what shows until the entity has one.
 */
function FieldEntity({ id, fallback }: { id: string; fallback: string }): React.JSX.Element {
  const entities = useGetEntities()
  const known = !!entities([id])[id]?.values.text
  return (
    <PillWrapper id={id} title={fallback} peek className="mx-0.5 align-middle">
      <PillBackground>
        {known ? (
          <PillContent id={id} bounded />
        ) : (
          <span className={cn('truncate font-serif', PILL_MAX_WIDTH)}>{fallback}</span>
        )}
      </PillBackground>
    </PillWrapper>
  )
}
