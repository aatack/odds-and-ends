import React, { useEffect } from 'react'
import { File02 } from '@untitledui/icons'
import { useResource } from '../state/hooks'
import { updateUi } from '../state/ui'
import { runTool } from '../tools/call'

/**
 * A `type: 'file'` entity's body: the bytes stored under its id. Images show
 * themselves, capped in height so a screenshot doesn't take over the tree;
 * anything else reads as a chip that saves the file when clicked.
 *
 * The entity carries its own mime type, so a row knows whether it is about to be
 * a picture before the bytes arrive and doesn't change shape when they do.
 */
export function ResourceView({
  id,
  mimeType,
  alt,
}: {
  id: string
  mimeType?: string
  alt?: string
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
        className="my-px max-h-96 max-w-full rounded-md shadow-xs"
        onClick={() => updateUi({ resourceId: id })}
      />
    )
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

/**
 * An image over the whole window, at its own size up to the edges of the screen.
 * Anywhere is a click-away, and Escape is taken here rather than routed — the key
 * router yields it while this is open, exactly as it does for the activity log.
 */
export function ResourceModal({ id }: { id: string }): React.JSX.Element | null {
  const resource = useResource(id)

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
      className="fixed inset-0 z-50 flex items-center justify-center bg-gray-950/60 p-6"
      onClick={() => updateUi({ resourceId: null })}
    >
      <img
        src={resource.url}
        alt={resource.name ?? 'Image'}
        className="max-h-full max-w-full rounded-md object-contain shadow-lg"
      />
    </div>
  )
}
