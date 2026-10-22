import React from 'react'
import { File02 } from '@untitledui/icons'
import { cn } from '../../helpers/cn'
import { useEntityLabel, useLoadedFileName } from '../../state/hooks'
import type { PillProps } from './types'

// Every kind's pill: the entity as small as it goes, inside a line of prose or
// beside other things. A name and, where the kind has one, a mark in front of it
// saying what sort of thing the name is — nothing a pill could be asked to fit.

/**
 * How wide a pill may get before its content gives up and ellipses. A pill is a
 * reference, not the thing: past about this much it stops being glanceable.
 */
const BOUND = 'max-w-[140px] truncate'

/** A label behind a small mark, for every kind that has a mark of its own. */
function Marked({
  icon: Icon,
  label,
  className,
  mono,
}: {
  icon: typeof File02
  label: string
  className?: string
  mono?: boolean
}): React.JSX.Element {
  return (
    <span className={cn(BOUND, 'inline-flex items-center gap-1', className)}>
      <Icon size={11} className="shrink-0 text-gray-400" />
      <span className={cn('truncate', mono ? 'font-mono text-[0.9em]' : 'font-serif')}>{label}</span>
    </span>
  )
}

/** A note: what it says. Serif, since that is what the user typed. */
export function NotePill({ id, className }: PillProps): React.JSX.Element {
  const label = useEntityLabel(id)
  return <span className={cn(BOUND, 'font-serif', className)}>{label}</span>
}

/**
 * A file gives its name, which is the one thing about a file worth reading at
 * this size — once the bytes are on screen somewhere; until then the label has
 * the file's kind.
 */
export function FilePill({ id, className }: PillProps): React.JSX.Element {
  const label = useEntityLabel(id)
  const name = useLoadedFileName(id)
  return <Marked icon={File02} label={name ?? label} className={className} />
}
