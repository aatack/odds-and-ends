import React, { useEffect, useRef, useState } from 'react'
import { PEEK_ATTRIBUTE } from '../components/peekHover'
import { cn } from '../helpers/cn'
import * as A from '../state/actions'
import { PEEK_SIZE } from '../state/peeks'
import { useLayoutState } from '../state/hooks'
import { getLayout } from '../state/store'
import { peeksOf, type PeekState, type Rect } from '../state/types'
import { TAB_BAR_ATTRIBUTE, TabContent } from './TabHeader'

// The floating windows a hover opens onto whatever a pill or a link refers to
// (a link's being its URL entity), or that "Open in peek" opens. Each is a tab
// of frames like any other, rooted at the entity — so it draws the entity's
// focus, and once it has been pressed in it has the keys: its own selection, its
// own stack, the same tools.
//
// Moving or resizing one pins it, after which it stays until its × is pressed.
//
// They sit over dialogs (z-50), since a pill in the inspector peeks too, and
// under popups (z-70): the palette opened from inside a peek belongs on top.

const SIZE = PEEK_SIZE
const GAP = 6
const MINIMUM = { width: 280, height: 160 }

/** Below the anchor if it fits, else above; kept inside the window. */
function place(anchor: Rect): Rect {
  const below = anchor.y + anchor.height + GAP
  const y =
    below + SIZE.height <= window.innerHeight ? below : Math.max(GAP, anchor.y - GAP - SIZE.height)
  const x = Math.min(Math.max(GAP, anchor.x), window.innerWidth - SIZE.width - GAP)
  return { x, y, ...SIZE }
}

export function PeekLayer(): React.JSX.Element | null {
  const layout = useLayoutState()
  const peeks = peeksOf(layout)

  // Pressing anywhere outside every peek gives the keys back to the main view.
  useEffect(() => {
    const onPointerDown = (e: PointerEvent): void => {
      if (!getLayout().peekFocus) return
      const inside = e.target instanceof Element && e.target.closest(`[${PEEK_ATTRIBUTE}]`)
      if (!inside) A.focusPeek(null)
    }
    window.addEventListener('pointerdown', onPointerDown, true)
    return () => window.removeEventListener('pointerdown', onPointerDown, true)
  }, [])

  if (!peeks.length) return null
  const frameOf = (peek: PeekState): string | null => layout.tabs[peek.tabId]?.top ?? null
  return (
    <>
      {peeks.map((peek, i) => (
        <PeekWindow
          key={peek.key}
          peek={peek}
          z={i}
          frameId={frameOf(peek)}
          focused={layout.peekFocus === peek.key}
        />
      ))}
    </>
  )
}

type Grab = { mode: 'move' | 'resize'; start: Rect; x: number; y: number; moved: boolean }

function PeekWindow({
  peek,
  z,
  frameId,
  focused,
}: {
  peek: PeekState
  /** Its place in the stack, bottom first. */
  z: number
  /** The top frame of the peek's tab. */
  frameId: string | null
  /** Whether it has the keys. */
  focused: boolean
}): React.JSX.Element | null {
  const placed = peek.rect ?? place(peek.anchor)
  const [live, setLive] = useState<Rect | null>(null)
  const grab = useRef<Grab | null>(null)
  const rect = live ?? placed

  const begin =
    (mode: Grab['mode']) =>
    (e: React.PointerEvent): void => {
      if (e.button !== 0) return
      e.preventDefault()
      grab.current = { mode, start: rect, x: e.clientX, y: e.clientY, moved: false }
      setLive(rect)
    }
  const move = (e: React.PointerEvent): void => {
    const current = grab.current
    if (!current) return
    const dx = e.clientX - current.x
    const dy = e.clientY - current.y
    current.moved ||= Math.abs(dx) + Math.abs(dy) > 2
    const { start } = current
    setLive(
      current.mode === 'move'
        ? {
            ...start,
            // The bar always stays reachable.
            x: Math.min(Math.max(start.x + dx, 80 - start.width), window.innerWidth - 80),
            y: Math.min(Math.max(start.y + dy, 0), window.innerHeight - 40),
          }
        : {
            ...start,
            width: Math.max(MINIMUM.width, start.width + dx),
            height: Math.max(MINIMUM.height, start.height + dy),
          },
    )
  }
  const end = (): void => {
    const current = grab.current
    grab.current = null
    if (current?.moved && live) A.placePeek(peek.key, live)
    setLive(null)
  }

  // A peek whose tab has gone has nothing left to show.
  if (!frameId) return null

  return (
    <div
      {...{ [PEEK_ATTRIBUTE]: peek.key }}
      className={cn(
        'fixed flex flex-col overflow-hidden rounded-xl bg-white shadow-[0_12px_40px_rgb(0_0_0/0.18)]',
        // The keys are in it — as a group says it is the selected one.
        focused && 'ring-1 ring-inset ring-brand-200',
      )}
      style={{ left: rect.x, top: rect.y, width: rect.width, height: rect.height, zIndex: 55 + z }}
      onMouseEnter={() => A.enterPeek(peek.key)}
      onMouseLeave={() => A.leavePeek(peek.key)}
      onPointerDown={(e) => {
        A.raisePeek(peek.key)
        A.focusPeek(peek.key)
        // Dragged by its tab's bar, anywhere but on something to press.
        const at = e.target instanceof Element ? e.target : null
        if (at?.closest(`[${TAB_BAR_ATTRIBUTE}]`) && !at.closest('button, a')) begin('move')(e)
      }}
    >
      <TabContent tabId={peek.tabId} peek={peek} />
      <div
        className="absolute bottom-0 right-0 size-4 cursor-nwse-resize"
        onPointerDown={(e) => {
          e.stopPropagation()
          begin('resize')(e)
        }}
      />
      {/* While dragging, catches the pointer even over a page in a webview. */}
      {live && (
        <div
          className="fixed inset-0 z-10"
          onPointerMove={move}
          onPointerUp={end}
          onPointerCancel={end}
        />
      )}
    </div>
  )
}
