import { useMemo, useRef, useState } from 'react'
import type { PointerEvent as ReactPointerEvent } from 'react'
import type { View } from '../../../core/types.ts'
import type { Edit, Peek, PeekTarget, Rect, ShownView } from '../state.ts'
import type { WindowProps } from './App.tsx'
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
 * pins it, after which it stays until its × is pressed. Pressing in it gives
 * it the keys: its rows take the cursor, and a chat's prompt box works.
 */
export function PeekWindow(props: {
  peek: Peek
  view: View | undefined
  /** Its rows, with its selection and any edit in it. */
  shown: ShownView | undefined
  edit: Edit | null
  /** Whether the keys are in it. */
  focused: boolean
  /** Its prompt box's draft, and whether it has the keyboard. */
  draft: string
  prompting: boolean
  window: WindowProps
  /** Hovering something inside opens a peek stacked on this one. */
  hover(target: PeekTarget, anchor: Rect, origin: string | null): void
  onEnter(key: string): void
  onLeave(window: string | null): void
  onPlace(key: string, rect: Rect): void
  onRaise(key: string): void
  onClose(key: string): void
  onOpen(target: PeekTarget): void
  onOpenEntity(id: string): void
  /** Its view is scrolled near its end: walk further. */
  onNearEnd(): void
  onImage(ref: string | null): void
  working: string[] | undefined
  /** The action waiting in this peek, and its prompt. */
  acting: { action: string; draft: string; composing: boolean } | null
  onAction(action: string): void
  onDraft(text: string): void
  onCompose(composing: boolean): void
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

  const close = () => props.onClose(peek.key)
  const handlers = props.window
  const prompt = useMemo(
    () => ({
      draft: props.draft,
      composing: props.prompting,
      onDraft: (text: string) => handlers.onDraft(peek.key, text),
      onCompose: (composing: boolean) => handlers.onCompose(peek.key, composing),
    }),
    [props.draft, props.prompting, handlers, peek.key],
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
      onPointerDown={() => {
        props.onRaise(peek.key)
        handlers.onFocus(peek.key)
      }}
    >
      <div className="peek-bar" onPointerDown={begin('move')}>
        <span className="grow">
          {peek.target.kind === 'entity' ? <HeaderPill entity={props.view?.root} /> : peek.target.url}
        </span>
        <Button label="Open" onClick={() => props.onOpen(peek.target)} />
        {peek.pinned && <Button quiet label="×" title="Close" onClick={close} />}
      </div>
      <PeekContext.Provider value={gestures}>
        <div className="peek-body">
          {peek.target.kind === 'url' ? (
            <webview key={peek.target.url} src={peek.target.url} partition="persist:preview" className="peek-page" />
          ) : props.view && props.shown ? (
            <ViewPane
              view={props.view}
              shown={props.shown}
              interactive={false}
              focused={props.focused}
              prompt={prompt}
              edit={props.edit}
              picking={null}
              working={props.working}
              acting={props.acting?.action ?? null}
              draft={props.acting?.draft ?? ''}
              composing={props.acting?.composing ?? false}
              onAction={props.onAction}
              onSelect={handlers.onSelect}
              onToggle={handlers.onToggle}
              onOpen={props.onOpenEntity}
              onImage={props.onImage}
              onDraft={props.onDraft}
              onCompose={props.onCompose}
              find={null}
              findFocus={0}
              onFind={noop}
              onFindFocused={noop}
              onNearEnd={props.onNearEnd}
              onEditDraft={handlers.onEditDraft}
              onCommitEdit={handlers.onCommitEdit}
              onCancelEdit={handlers.onCancelEdit}
              onClose={close}
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
