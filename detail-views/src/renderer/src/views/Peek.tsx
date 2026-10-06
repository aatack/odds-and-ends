import { useRef, useState } from 'react'
import type { PointerEvent as ReactPointerEvent } from 'react'
import type { Focus as FocusData } from '../../../core/types.ts'
import type { Peek, PeekTarget, Rect } from '../state.ts'
import { Focus } from './Focus.tsx'
import { HeaderPill } from './primitives.tsx'

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
  focus: FocusData | undefined
  onEnter(): void
  onLeave(): void
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
      onMouseEnter={props.onEnter}
      onMouseLeave={peek.pinned ? undefined : props.onLeave}
      onPointerDown={() => props.onRaise(peek.key)}
    >
      <div className="peek-bar" onPointerDown={begin('move')}>
        <span className="grow">
          {peek.target.kind === 'entity' ? <HeaderPill entity={props.focus?.entity} /> : peek.target.url}
        </span>
        <button onPointerDown={(event) => event.stopPropagation()} onClick={() => props.onOpen(peek.target)}>
          Open
        </button>
        {peek.pinned && (
          <button
            className="peek-close"
            onPointerDown={(event) => event.stopPropagation()}
            onClick={() => props.onClose(peek.key)}
          >
            ×
          </button>
        )}
      </div>
      <div className="peek-body">
        {peek.target.kind === 'url' ? (
          <webview key={peek.target.url} src={peek.target.url} partition="persist:preview" className="peek-page" />
        ) : props.focus ? (
          <Focus
            focus={props.focus}
            cursor={-1}
            draft=""
            composing={false}
            headed={false}
            onSelect={noop}
            onOpen={props.onOpenEntity}
            onDraft={noop}
            onCompose={noop}
            onImage={props.onImage}
          />
        ) : null}
      </div>
      <div className="peek-resize" onPointerDown={begin('resize')} />
      {live && <div className="peek-capture" onPointerMove={move} onPointerUp={end} onPointerCancel={end} />}
    </div>
  )
}

function noop(): void {}
