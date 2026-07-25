import React, { type ButtonHTMLAttributes } from 'react'
import { File02 } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { useEntityLabel, useEntitySummary, useLoadedFileName } from '../state/hooks'

// An entity referenced in passing: a tab's title, a crumb in the frame trail, a
// mention inside another entity's text. Small enough to sit in a line of prose,
// and still an entity — right-click it for the tool list, middle-click it for a
// new tab, exactly as over a row.
//
// Three parts, so each can be had without the others:
//
//   PillContent     what the entity looks like at this size, which depends on
//                   what kind of entity it is
//   PillWrapper     the entity gestures, around any child at all
//   PillBackground  a surface, for a pill that has to be told apart from the text
//                   around it
//
// `EntityPill` is the three together, which is what most callers want. Tabs take
// the first two and put them on a background of their own.

/**
 * How wide a pill may get before its content gives up and ellipses. A pill is a
 * reference, not the thing: past about this much it stops being glanceable.
 */
const BOUND = 'max-w-[100px] truncate'

/**
 * The entity, as small as it goes. Text ellipses; a file gives its name, which
 * is the one thing about a file worth reading at this size; code keeps its
 * monospace so it doesn't read as prose.
 */
export function PillContent({
  id,
  className,
}: {
  id: string
  className?: string
}): React.JSX.Element {
  const summary = useEntitySummary(id)
  const label = useEntityLabel(id)
  // Only ever the name of a file already on screen somewhere; `label` covers the
  // rest with the file's kind.
  const fileName = useLoadedFileName(id)

  if (summary?.type === 'file') {
    return (
      <span className={cn(BOUND, 'inline-flex items-center gap-1', className)}>
        <File02 size={11} className="shrink-0 text-gray-400" />
        <span className="truncate">{fileName ?? label}</span>
      </span>
    )
  }
  if (summary?.type === 'code') {
    return <span className={cn(BOUND, 'font-mono text-[0.9em]', className)}>{label}</span>
  }
  // Serif: the label is what the user typed, so a pill reads as the entity rather
  // than as a caption the app wrote about it.
  return <span className={cn(BOUND, 'font-serif', className)}>{label}</span>
}

/**
 * Pill behaviour, from the id alone. The app's global handlers pick the entity off
 * the DOM, so publishing it here is the whole implementation: right-click opens
 * the tool list seeded with this entity, middle-click opens it in a new tab. The
 * full label becomes the tooltip, since the content itself is likely truncated.
 */
export function PillWrapper({
  id,
  title,
  className,
  children,
}: {
  id: string
  title?: string
  className?: string
  children: React.ReactNode
}): React.JSX.Element {
  const label = useEntityLabel(id)
  return (
    <span
      data-entity-id={id}
      title={title ?? label}
      className={cn('inline-flex min-w-0 items-center', className)}
    >
      {children}
    </span>
  )
}

/**
 * A pill's own surface: the tone of a secondary button, so a pill inside a
 * sentence reads as something you can click. Tone and a whisper of shadow rather
 * than a border, per the design language. A button, because a pill in text is
 * there to be followed.
 */
export function PillBackground({
  className,
  ...rest
}: ButtonHTMLAttributes<HTMLButtonElement>): React.JSX.Element {
  return (
    <button
      className={cn(
        'inline-flex min-w-0 items-center gap-1 rounded bg-gray-100 px-1.5 py-px',
        'text-gray-700 shadow-xs hover:bg-gray-200',
        'focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-500/40',
        className,
      )}
      {...rest}
    />
  )
}

/** All three: the everyday pill. Button attributes reach the background. */
export function EntityPill({
  id,
  className,
  ...rest
}: { id: string } & ButtonHTMLAttributes<HTMLButtonElement>): React.JSX.Element {
  return (
    <PillWrapper id={id} className={className}>
      <PillBackground {...rest}>
        <PillContent id={id} />
      </PillBackground>
    </PillWrapper>
  )
}
