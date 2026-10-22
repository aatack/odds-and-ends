import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { File02 } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { useResource } from '../state/hooks'
import { updateUi } from '../state/ui'
import { runTool } from '../tools/call'

/** Whether a file's bytes are text worth reading as text, by what it says it is. */
const textual = (mime: string): boolean =>
  mime.startsWith('text/') || /^application\/(json|xml|javascript|x-yaml|yaml|toml)\b/.test(mime)

/** How much of a text file a preview reads before saying there is more. */
const TEXT_PREVIEW = 200_000

/** A data URL's bytes, as text. */
function decodeText(url: string): string {
  const comma = url.indexOf(',')
  const bytes = Uint8Array.from(atob(url.slice(comma + 1)), (c) => c.charCodeAt(0))
  return new TextDecoder().decode(bytes)
}

/**
 * A `type: 'file'` entity's body: the bytes stored under its id, at one of two
 * sizes. As a card, images show themselves capped in height so a screenshot
 * doesn't take over the tree, and anything else reads as a chip that saves the
 * file when clicked. As a focus — the file's own page — an image is as large as
 * the page allows, a text file is read out in full, and audio and video play.
 *
 * The entity carries its own mime type, so a row knows whether it is about to be
 * a picture before the bytes arrive and doesn't change shape when they do.
 */
export function ResourceView({
  id,
  mimeType,
  alt,
  size = 'card',
}: {
  id: string
  mimeType?: string
  alt?: string
  size?: 'card' | 'focus'
}): React.JSX.Element {
  const resource = useResource(id)
  const mime = resource.status === 'ready' ? resource.mimeType : mimeType
  const isImage = (mime ?? '').startsWith('image/')

  if (resource.status === 'loading') {
    return (
      <span className="block font-serif text-[14px] leading-5 text-gray-400">
        {isImage ? 'Loading image…' : 'Loading file…'}
      </span>
    )
  }
  if (resource.status === 'missing') {
    return (
      <span className="block font-serif text-[14px] leading-5 italic text-gray-400">
        No bytes stored for this file.
      </span>
    )
  }
  if (resource.status === 'error') {
    return (
      <span className="block font-serif text-[14px] leading-5 text-error-600">
        {resource.message}
      </span>
    )
  }

  if (isImage) {
    return (
      <img
        src={resource.url}
        alt={alt || resource.name || 'Image'}
        title={resource.name ?? 'Click to see it full size'}
        // Scaled to fit a box rather than to a width: the smaller of the two caps
        // wins, so a tall screenshot and a wide one both come out as large as they
        // can be without taking over the tree.
        className={cn(
          'my-px max-w-full rounded-md shadow-xs',
          size === 'focus' ? 'max-h-[70vh]' : 'max-h-48',
        )}
        onClick={() => updateUi({ resourceId: id })}
      />
    )
  }

  if (size === 'focus') {
    const mime = resource.mimeType
    if (textual(mime)) return <TextPreview url={resource.url} />
    if (mime.startsWith('video/')) {
      return <video src={resource.url} controls className="my-1 max-h-[70vh] max-w-full rounded-md" />
    }
    if (mime.startsWith('audio/')) return <audio src={resource.url} controls className="my-1 w-full" />
  }

  return (
    <button
      className="my-px inline-flex max-w-full items-center gap-1.5 rounded-md bg-gray-100 px-2 py-1 text-[12px] text-gray-600 shadow-xs hover:text-gray-900 focus:outline-none focus-visible:ring-1 focus-visible:ring-brand-300"
      // Straight to the downloads folder, with no dialog to mis-answer.
      onClick={() => runTool('resource.save', { extra: { entityId: id } })}
      title={`Save ${resource.name ?? 'file'} to your downloads`}
    >
      <File02 size={13} className="shrink-0 text-gray-400" />
      <span className="truncate">{resource.name || resource.mimeType}</span>
    </button>
  )
}

/** A text file read out, as far as {@link TEXT_PREVIEW} goes. */
function TextPreview({ url }: { url: string }): React.JSX.Element {
  const text = useMemo(() => decodeText(url), [url])
  const cut = text.length > TEXT_PREVIEW
  return (
    <pre className="my-1 max-h-[70vh] overflow-auto whitespace-pre-wrap break-words rounded-md bg-gray-50 p-3 font-mono text-[12.5px] leading-5 text-gray-800">
      {cut ? text.slice(0, TEXT_PREVIEW) : text}
      {cut && <span className="text-gray-400">{`\n… ${text.length - TEXT_PREVIEW} more characters`}</span>}
    </pre>
  )
}

/** Where the image sits in the window: a scale, and an offset from the centre. */
type View = { scale: number; x: number; y: number }

const FIT: View = { scale: 1, x: 0, y: 0 }
const MIN_SCALE = 0.5
const MAX_SCALE = 8

/**
 * The gestures over a full-window image: the wheel zooms about the pointer, and a
 * drag pans. The wheel is bound by hand rather than through `onWheel`, because React
 * registers that one passive and a passive listener cannot stop the scroll it came
 * from reaching the page underneath.
 *
 * A drag ends in a click too, so the modal can't close on any click it sees: the pan
 * records whether the pointer moved, and a click that came out of a drag is not a
 * click-away.
 */
function usePanZoom(): {
  view: View
  ref: React.MutableRefObject<HTMLDivElement | null>
  dragged: React.MutableRefObject<boolean>
  handlers: React.HTMLAttributes<HTMLDivElement>
} {
  const ref = useRef<HTMLDivElement | null>(null)
  const [view, setView] = useState<View>(FIT)
  const dragged = useRef(false)
  const from = useRef<{ x: number; y: number } | null>(null)

  useEffect(() => {
    const node = ref.current
    if (!node) return
    const onWheel = (e: WheelEvent): void => {
      e.preventDefault()
      const box = node.getBoundingClientRect()
      // The pointer, relative to the centre the image is scaled about.
      const px = e.clientX - box.left - box.width / 2
      const py = e.clientY - box.top - box.height / 2
      setView((v) => {
        const scale = Math.min(MAX_SCALE, Math.max(MIN_SCALE, v.scale * Math.exp(-e.deltaY / 400)))
        // Hold whatever is under the pointer still while the scale changes.
        const factor = scale / v.scale
        return { scale, x: px - (px - v.x) * factor, y: py - (py - v.y) * factor }
      })
    }
    node.addEventListener('wheel', onWheel, { passive: false })
    return () => node.removeEventListener('wheel', onWheel)
  }, [])

  const onPointerDown = useCallback((e: React.PointerEvent<HTMLDivElement>): void => {
    if (e.button !== 0) return
    dragged.current = false
    from.current = { x: e.clientX, y: e.clientY }
    e.currentTarget.setPointerCapture(e.pointerId)
  }, [])

  const onPointerMove = useCallback((e: React.PointerEvent<HTMLDivElement>): void => {
    const start = from.current
    if (!start) return
    const dx = e.clientX - start.x
    const dy = e.clientY - start.y
    // A few pixels of wobble on the way to a click is not a drag.
    if (!dragged.current && Math.abs(dx) + Math.abs(dy) < 3) return
    dragged.current = true
    from.current = { x: e.clientX, y: e.clientY }
    setView((v) => ({ ...v, x: v.x + dx, y: v.y + dy }))
  }, [])

  const onPointerUp = useCallback((e: React.PointerEvent<HTMLDivElement>): void => {
    from.current = null
    if (e.currentTarget.hasPointerCapture(e.pointerId)) {
      e.currentTarget.releasePointerCapture(e.pointerId)
    }
  }, [])

  const onDoubleClick = useCallback((): void => setView(FIT), [])

  return {
    view,
    ref,
    dragged,
    handlers: {
      onPointerDown,
      onPointerMove,
      onPointerUp,
      onPointerCancel: onPointerUp,
      onDoubleClick
    }
  }
}

/**
 * An image over the whole window, at its own size up to the edges of the screen.
 * Scrolling zooms it and dragging pans it, and a double click puts it back to fitting
 * the window. A click that wasn't a drag is a click-away, and Escape is taken here
 * rather than routed — the key router yields it while this is open, exactly as it
 * does for the activity log.
 */
export function ResourceModal({ id }: { id: string }): React.JSX.Element | null {
  const resource = useResource(id)
  const { view, ref, dragged, handlers } = usePanZoom()

  useEffect(() => {
    const onKeyDown = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') updateUi({ resourceId: null })
    }
    window.addEventListener('keydown', onKeyDown)
    return () => window.removeEventListener('keydown', onKeyDown)
  }, [])

  if (resource.status !== 'ready') return null

  return (
    <div
      ref={ref}
      className="fixed inset-0 z-50 flex touch-none items-center justify-center overflow-hidden bg-gray-950/60 p-6"
      onClick={() => {
        if (!dragged.current) updateUi({ resourceId: null })
      }}
      {...handlers}
    >
      <img
        src={resource.url}
        alt={resource.name ?? 'Image'}
        draggable={false}
        className="max-h-full max-w-full rounded-md object-contain shadow-lg"
        style={{ transform: `translate(${view.x}px, ${view.y}px) scale(${view.scale})` }}
      />
    </div>
  )
}
