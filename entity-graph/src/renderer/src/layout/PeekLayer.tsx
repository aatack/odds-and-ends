import React, { useEffect, useRef, useState } from 'react'
import { X } from '@untitledui/icons'
import { PillContent } from '../components/EntityPill'
import { PEEK_ATTRIBUTE } from '../components/peekHover'
import { Button } from '../components/ui/Button'
import { cn } from '../helpers/cn'
import * as A from '../state/actions'
import { PEEK_SIZE } from '../state/peeks'
import { useLayoutState } from '../state/hooks'
import { getLayout } from '../state/store'
import { URL_PREFIX } from '../../../core/builtins'
import { peeksOf, type PeekState, type Rect } from '../state/types'
import { runTool } from '../tools/call'
import { EntityFrame } from './EntityFrame'

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
          rootId={layout.frames[frameOf(peek) ?? '']?.rootId ?? null}
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
  rootId,
  focused,
}: {
  peek: PeekState
  /** Its place in the stack, bottom first. */
  z: number
  /** The top frame of the peek's tab. */
  frameId: string | null
  /** The entity at the root of that frame — the one the bar names. */
  rootId: string | null
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

  const open = (): void => {
    const entityId = rootId ?? peek.entityId
    // A web page nobody has written anything about opens where pages open.
    if (entityId.startsWith(URL_PREFIX)) {
      runTool('link.open', { extra: { url: entityId.slice(URL_PREFIX.length) } })
    } else runTool('entity.openInTab', { extra: { entityId } })
  }

  // A peek whose tab has gone has nothing left to show.
  if (!frameId) return null

  return (
    <div
      {...{ [PEEK_ATTRIBUTE]: peek.key }}
      className="fixed flex flex-col overflow-hidden rounded-xl bg-white shadow-[0_12px_40px_rgb(0_0_0/0.18)]"
      style={{ left: rect.x, top: rect.y, width: rect.width, height: rect.height, zIndex: 55 + z }}
      onMouseEnter={() => A.enterPeek(peek.key)}
      onMouseLeave={() => A.leavePeek(peek.key)}
      onPointerDown={() => {
        A.raisePeek(peek.key)
        A.focusPeek(peek.key)
      }}
    >
      <div
        className={cn(
          'flex shrink-0 items-center gap-2 px-3 py-1.5 text-[12px] text-gray-500 select-none',
          // The keys are in it: the one place it says so.
          focused ? 'bg-gray-100' : 'bg-gray-50',
        )}
        onPointerDown={begin('move')}
      >
        <span className="flex min-w-0 flex-1 items-center">
          {rootId && <PillContent id={rootId} className="text-gray-700" />}
        </span>
        {/* Pressing a button never starts a drag. */}
        <span className="flex items-center gap-1" onPointerDown={(e) => e.stopPropagation()}>
          <Button variant="tertiary" size="sm" onClick={open}>
            Open
          </Button>
          {peek.pinned && (
            <button
              className="text-gray-400 hover:text-gray-700 focus:outline-none"
              title="Close"
              aria-label="Close"
              onClick={() => A.closePeek(peek.key)}
            >
              <X size={13} />
            </button>
          )}
        </span>
      </div>
      <div className="relative min-h-0 flex-1">
        <EntityFrame frameId={frameId} />
      </div>
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
