import React, { type ButtonHTMLAttributes } from 'react'
import { cn } from '../helpers/cn'
import { useEntityLabel, useEntitySummary } from '../state/hooks'
import { kindOf } from './kinds'

// An entity referenced in passing: a tab's title, a crumb in the frame trail, a
// mention inside another entity's text. Small enough to sit in a line of prose,
// and still an entity — right-click it for the tool list, middle-click it for a
// new tab, exactly as over a row.
//
// Three parts, so each can be had without the others:
//
//   PillContent     what the entity looks like at this size, which is its kind's
//                   pill (`./kinds`)
//   PillWrapper     the entity gestures, around any child at all
//   PillBackground  a surface, for a pill that has to be told apart from the text
//                   around it
//
// `EntityPill` is the three together, which is what most callers want. Tabs take
// the first two and put them on a background of their own.

/** The entity, as small as it goes: whatever its kind draws a pill as. */
export function PillContent({
  id,
  className,
}: {
  id: string
  className?: string
}): React.JSX.Element {
  const { Pill } = kindOf(useEntitySummary(id).type)
  return <Pill id={id} className={className} />
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
