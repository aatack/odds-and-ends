import React from 'react'
import { EntityMarkdown } from '../EntityMarkdown'
import { cn } from '../../helpers/cn'
import type { EntityRow } from '../../state/derive'

// What every card shares: the entity's own text, set the way the outliner sets
// it. A note's card is nothing else, and every other kind draws this under or
// beside whatever it is a surface for.

/**
 * User-written entity text — serif, wrapping to show the whole value rather than
 * truncating. `block` keeps the editing textarea the same height as the static
 * line, so pressing `e` doesn't shift the layout. Colour is applied per use, so a
 * row hiding its children can read as muted.
 */
export const TEXT = 'block font-serif text-[14px] leading-5'

/** Monospace counterpart, for editing `type: code` entities in place. */
export const MONO = 'block font-mono text-[12.5px] leading-5'

/**
 * Section headings grow with prominence: ~1.5x at the top level, easing toward
 * ~1.1x as they nest deeper. Per depth, so it can't be a static utility class.
 * Applied to the editing box as well as the static line, so a section reads as
 * one while it is being typed.
 */
export const sectionStyle = (depth: number): React.CSSProperties => ({
  fontSize: `${14 * Math.max(1.1, 1.5 - depth * 0.1)}px`,
  lineHeight: 1.3,
})

/**
 * The row's text, rendered — or null when it has none. Muted when the row hides
 * children: there is more here than is on screen, and the row is a door rather
 * than the whole of it.
 */
export function RowText({
  row,
  highlight,
  muted,
}: {
  row: EntityRow
  highlight?: string
  /** Always muted, as a caption under a surface is. */
  muted?: boolean
}): React.JSX.Element | null {
  if (!row.text) return null
  return (
    <EntityMarkdown
      text={row.text}
      highlight={highlight}
      className={cn(
        TEXT,
        row.section && 'font-semibold',
        muted ? 'text-gray-500' : row.hidesChildren ? 'text-gray-400' : 'text-gray-900',
      )}
      style={row.section ? sectionStyle(row.depth) : undefined}
    />
  )
}

/** A row with no text at all, which would otherwise be an invisible line. */
export const EMPTY = <span className={`${TEXT} italic text-gray-400`}>Empty</span>

/** A line of chrome under an entity's text: what the app says about it, in sans. */
export function Aside({ children }: { children: React.ReactNode }): React.JSX.Element {
  return <div className="mt-0.5 text-[12px] leading-4 text-gray-400">{children}</div>
}
