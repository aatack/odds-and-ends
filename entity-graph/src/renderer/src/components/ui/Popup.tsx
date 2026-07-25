import type { ReactNode } from 'react'
import { cn } from '../../helpers/cn'

// The overlay shell the command palette and the activity log share: a
// click-away backdrop and one floating panel, drawn either at the cursor (so it
// reads as a context menu) or centred below the header. Height belongs to the
// contents — a panel is only ever as tall as what it holds — so this fixes the
// width, the corner and the elevation, and nothing else.

/** Where an anchored popup is drawn, in viewport coordinates. */
export interface PopupAnchor {
  x: number
  y: number
}

/** The panel's width in px, to keep an anchored one clear of the right edge. */
export const POPUP_WIDTH = 512

/** Roughly the tallest a panel gets, likewise for the bottom edge. */
const POPUP_HEIGHT = 360

const MARGIN = 8

export function Popup({
  anchor = null,
  onClose,
  children,
}: {
  anchor?: PopupAnchor | null
  onClose: () => void
  children: ReactNode
}): React.JSX.Element {
  const style: React.CSSProperties = anchor
    ? {
        top: Math.min(anchor.y, window.innerHeight - POPUP_HEIGHT),
        left: Math.min(anchor.x, window.innerWidth - POPUP_WIDTH - MARGIN),
      }
    : // Centred, and low enough that the panel sits in the upper third rather
      // than crowding the header.
      { top: '12rem', left: '50%', transform: 'translateX(-50%)' }

  return (
    <div
      className={cn('fixed inset-0 z-50', !anchor && 'bg-gray-950/10 backdrop-blur-xs')}
      onClick={onClose}
      onContextMenu={(e) => {
        // A right-click on the backdrop closes; stop it reaching the window
        // handler, which would otherwise immediately reopen the palette on top.
        e.preventDefault()
        e.stopPropagation()
        onClose()
      }}
    >
      <div
        className="absolute w-[32rem] max-w-[calc(100vw-1rem)] overflow-hidden rounded-lg bg-white shadow-lg"
        style={style}
        onClick={(e) => e.stopPropagation()}
        onContextMenu={(e) => e.stopPropagation()}
      >
        {children}
      </div>
    </div>
  )
}
