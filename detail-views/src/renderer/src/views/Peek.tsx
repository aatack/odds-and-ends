import { useMemo, useRef, useState } from 'react'
import type { PointerEvent as ReactPointerEvent } from 'react'
import type { View } from '../../../core/types.ts'
import type { Peek, PeekTarget, Rect } from '../state.ts'
import { ViewPane } from './View.tsx'
import { Button, HeaderPill, PeekContext } from './primitives.tsx'

const size = { width: 720, height: 480 }
const gap = 6
const minimum = { width: 280, height: 160 }

/** Below the anchor if it fits, else above; kept inside the window. */
function place(anchor: Rect): Rect {
  const below = anchor.y + anchor.height + gap
  const y = below + size.height <= window.innerHeight ? below : Math.max(gap, anchor.y - gap - size.height)
  const x = Math.min(Math.max(gap, anchor.x), window.innerWidth - size.width - gap)
  return { x, y, ...size }
}

type Grab = { mode: 'move' | 'resize'; start: Rect; x: number; y: number; moved: boolean }

/**
 * A floating window onto a link or an entity. Dragging the bar or the corner
 * pins it, after which it stays until its × is pressed.
 */
export function PeekWindow(props: {
  peek: Peek
  view: View | undefined
  /** Hovering something inside opens a peek stacked on this one. */
  hover(target: PeekTarget, anchor: Rect, origin: string | null): void
  onEnter(key: string): void
  onLeave(window: string | null): void
  onPlace(key: string, rect: Rect): void
  onRaise(key: string): void
  onClose(key: string): void
  onOpen(target: PeekTarget): void
  onOpenEntity(id: string): void
  onImage(ref: string | null): void
}) {
  const { peek } = props
  const placed = peek.rect ?? place(peek.anchor)
  const [live, setLive] = useState<Rect | null>(null)
  const grab = useRef<Grab | null>(null)
  const rect = live ?? placed
  const { hover, onLeave } = props
  const gestures = useMemo(
    () => ({
      onPeekEnter: (target: PeekTarget, anchor: Rect) => hover(target, anchor, peek.key),
      onPeekLeave: () => onLeave(null),
    }),
    [hover, onLeave, peek.key],
  )

  const begin = (mode: Grab['mode']) => (event: ReactPointerEvent) => {
    if (event.button !== 0) return
    event.preventDefault()
    event.stopPropagation()
    grab.current = { mode, start: rect, x: event.clientX, y: event.clientY, moved: false }
    setLive(rect)
  }
  const move = (event: ReactPointerEvent) => {
    const current = grab.current
    if (!current) return
    const dx = event.clientX - current.x
    const dy = event.clientY - current.y
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
            width: Math.max(minimum.width, start.width + dx),
            height: Math.max(minimum.height, start.height + dy),
          },
    )
  }
  const end = () => {
    const current = grab.current
    grab.current = null
    if (current?.moved && live) props.onPlace(peek.key, live)
    setLive(null)
  }

  return (
    <div
      className={`peek${peek.pinned ? ' pinned' : ''}`}
      style={{ left: rect.x, top: rect.y, width: rect.width, height: rect.height }}
      onMouseEnter={() => props.onEnter(peek.key)}
      onMouseLeave={() => props.onLeave(peek.key)}
      onPointerDown={() => props.onRaise(peek.key)}
    >
      <div className="peek-bar" onPointerDown={begin('move')}>
        <span className="grow">
          {peek.target.kind === 'entity' ? <HeaderPill entity={props.view?.root} /> : peek.target.url}
        </span>
        <Button label="Open" onClick={() => props.onOpen(peek.target)} />
        {peek.pinned && <Button quiet label="×" title="Close" onClick={() => props.onClose(peek.key)} />}
      </div>
      <PeekContext.Provider value={gestures}>
        <div className="peek-body">
          {peek.target.kind === 'url' ? (
            <webview key={peek.target.url} src={peek.target.url} partition="persist:preview" className="peek-page" />
          ) : props.view ? (
            <ViewPane
              view={props.view}
              shown={{ rows: props.view.rows.map((row) => ({ kind: 'entity', key: row.key, row, selected: false, editing: false })), selectedPath: [], selectedIndex: -1 }}
              interactive={false}
              edit={null}
              picking={null}
              acting={null}
              draft=""
              composing={false}
              onSelect={noop}
              onOpen={props.onOpenEntity}
              onImage={props.onImage}
              onDraft={noop}
              onCompose={noop}
              find={null}
              findFocus={0}
              onFind={noop}
              onEditDraft={noop}
              onCommitEdit={noop}
              onCancelEdit={noop}
            />
          ) : null}
        </div>
      </PeekContext.Provider>
      <div className="peek-resize" onPointerDown={begin('resize')} />
      {live && <div className="peek-capture" onPointerMove={move} onPointerUp={end} onPointerCancel={end} />}
    </div>
  )
}

function noop(): void {}
