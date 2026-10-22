import type React from 'react'
import * as A from '../state/actions'

/** The attribute a peek's window carries, naming it — how a pill knows which peek it is in. */
export const PEEK_ATTRIBUTE = 'data-peek'

/**
 * Hover handlers that peek at an entity — a web page's being its URL entity — to spread onto anything that refers to
 * something else — a pill, a link. Where the hover is, and which peek it is
 * drawn in, are read off the element itself, so nothing has to be threaded down
 * to every pill to say so.
 */
export function peekHandlers(entityId: string | null): {
  onMouseEnter?: (e: React.MouseEvent<HTMLElement>) => void
  onMouseLeave?: () => void
} {
  if (!entityId) return {}
  return {
    onMouseEnter: (e) => {
      const el = e.currentTarget
      const { x, y, width, height } = el.getBoundingClientRect()
      const origin = el.closest(`[${PEEK_ATTRIBUTE}]`)?.getAttribute(PEEK_ATTRIBUTE) ?? null
      A.hoverPeek(entityId, { x, y, width, height }, origin)
    },
    onMouseLeave: () => A.leavePeek(null),
  }
}
