import React, { useMemo } from 'react'
import { identiconOf } from '../../helpers/identicon'
import { cn } from '../../helpers/cn'

/**
 * The picture an id draws, round. A cheap way for something with no face — a
 * chat, a person who has never uploaded anything — to look like itself and to
 * keep looking like itself, since nothing about it is stored and the id is all
 * it takes.
 *
 * A cell of margin all round inside the viewBox, so the circle clips the
 * background rather than the corners of the shape.
 */
export function Identicon({
  id,
  size = 28,
  className,
}: {
  id: string
  /** Side, in px. */
  size?: number
  className?: string
}): React.JSX.Element {
  const { cells, colour, side } = useMemo(() => identiconOf(id), [id])
  return (
    <svg
      viewBox={`0 0 ${side + 2} ${side + 2}`}
      width={size}
      height={size}
      className={cn('shrink-0 rounded-full bg-gray-100', className)}
      aria-hidden
    >
      {cells.map((on, i) =>
        on ? (
          <rect
            key={i}
            x={(i % side) + 1}
            y={Math.floor(i / side) + 1}
            width={1}
            height={1}
            fill={colour}
          />
        ) : null,
      )}
    </svg>
  )
}
