import React from 'react'
import { File02, Globe01 } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { useEntityLabel, useGetEntities, useLoadedFileName } from '../../state/hooks'
import { str } from '../../state/derive'
import { PILL_MAX_WIDTH, type PillProps } from './types'

// Every type's pill: the entity as small as it goes, inside a line of prose or
// beside other things. A name and, where the kind has one, a mark in front of it
// saying what sort of thing the name is — nothing a pill could be asked to fit.

/** A pill's extent: never past its room, and within its maximum where bounded. */
const extent = (bounded?: boolean): string => cn('min-w-0 truncate', bounded && PILL_MAX_WIDTH)

/** A label behind a small mark, for every kind that has a mark of its own. */
function Marked({
  icon: Icon,
  label,
  className,
  mono,
  bounded,
}: {
  icon: typeof File02
  label: string
  className?: string
  mono?: boolean
  bounded?: boolean
}): React.JSX.Element {
  return (
    <span className={cn(extent(bounded), 'inline-flex items-center gap-1', className)}>
      <Icon size={11} className="shrink-0 text-gray-400" />
      <span className={cn('truncate', mono ? 'font-mono text-[0.9em]' : 'font-serif')}>{label}</span>
    </span>
  )
}

/** A note: what it says. Serif, since that is what the user typed. */
export function NotePill({ id, className, bounded }: PillProps): React.JSX.Element {
  const label = useEntityLabel(id)
  return <span className={cn(extent(bounded), 'font-serif', className)}>{label}</span>
}

/**
 * A file gives its name, which is the one thing about a file worth reading at
 * this size — once the bytes are on screen somewhere; until then the label has
 * the file's kind.
 */
export function FilePill({ id, className, bounded }: PillProps): React.JSX.Element {
  const label = useEntityLabel(id)
  const name = useLoadedFileName(id)
  return <Marked icon={File02} label={name ?? label} className={className} bounded={bounded} />
}

/** A page: what it is called if the entity says, else where it is. */
export function UrlPill({ id, className, bounded }: PillProps): React.JSX.Element {
  const values = useGetEntities()([id])[id].values
  const url = str(values.url)
  const label = str(values.text) ?? (url ? url.replace(/^https?:\/\//i, '') : id)
  return <Marked icon={Globe01} label={label} className={className} bounded={bounded} />
}
