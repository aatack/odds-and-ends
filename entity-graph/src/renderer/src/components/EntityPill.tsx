import React, { type ButtonHTMLAttributes } from 'react'
import { cn } from '../helpers/cn'
import { useEntityLabel, useEntitySummary } from '../state/hooks'
import { viewsFor } from './types'
import { peekHandlers } from './peekHover'

// An entity referenced in passing: a tab's title, a crumb in the frame trail, a
// mention inside another entity's text. Small enough to sit in a line of prose,
// and still an entity — right-click it for the tool list, middle-click it for a
// new tab, exactly as over a row.
//
// Three parts, so each can be had without the others:
//
//   PillContent     what the entity looks like at this size, which is its type's
//                   pill (`./types`)
//   PillWrapper     the entity gestures, around any child at all
//   PillBackground  a surface, for a pill that has to be told apart from the text
//                   around it
//
// `EntityPill` is the three together, which is what most callers want; with
// `bare` it is the first two. Tabs take those two and put them on a background
// of their own.
//
// A pill's width is capped only where it is `bounded` — in the tab header and
// inline in text, where one long name would crowd everything beside it.
// Elsewhere it takes the room it is given.

/** The entity, as small as it goes: whatever its kind draws a pill as. */
export function PillContent({
  id,
  className,
  bounded,
}: {
  id: string
  className?: string
  /** Capped at a pill's maximum width; see `PillProps`. */
  bounded?: boolean
}): React.JSX.Element {
  const { Pill } = viewsFor(useEntitySummary(id).type)
  return <Pill id={id} className={className} bounded={bounded} />
}

/**
 * Pill behaviour, from the id alone. The app's global handlers pick the entity off
 * the DOM, so publishing it here is most of the implementation: right-click opens
 * the tool list seeded with this entity, middle-click opens it in a new tab. With
 * `peek`, hovering it opens a peek onto the entity — for a pill that refers to
 * something rather than standing for what is already on screen, as a tab does.
 * The full label becomes the tooltip, since the content itself is likely
 * truncated — except on a pill that peeks, where the peek says it better.
 */
export function PillWrapper({
  id,
  title,
  peek,
  className,
  children,
}: {
  id: string
  title?: string
  peek?: boolean
  className?: string
  children: React.ReactNode
}): React.JSX.Element {
  const label = useEntityLabel(id)
  return (
    <span
      data-entity-id={id}
      title={peek ? undefined : (title ?? label)}
      className={cn('inline-flex min-w-0 items-center', className)}
      {...peekHandlers(peek ? { kind: 'entity', id } : null)}
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

/**
 * The everyday pill, which peeks: the gestures, the surface, and the content.
 * Button attributes reach the surface.
 *
 * `bare` leaves the surface out — the pill's gestures around its content alone,
 * for somewhere that is already a surface (a menu line, a form line), as the
 * schema components take `bare` inside another one. `bounded` caps its width,
 * for a pill among others or in prose. `peek` opens a preview on hover; by
 * default only a pill with its own surface does, since a bare one sits inside
 * something else with its own business — a menu being chosen from, say.
 */
export function EntityPill({
  id,
  bare = false,
  bounded = false,
  peek = !bare,
  className,
  ...rest
}: {
  id: string
  bare?: boolean
  bounded?: boolean
  peek?: boolean
} & ButtonHTMLAttributes<HTMLButtonElement>): React.JSX.Element {
  const content = <PillContent id={id} bounded={bounded} />
  return (
    <PillWrapper id={id} peek={peek} className={className}>
      {bare ? content : <PillBackground {...rest}>{content}</PillBackground>}
    </PillWrapper>
  )
}
