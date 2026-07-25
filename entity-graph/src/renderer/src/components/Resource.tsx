import React from 'react'
import { File02 } from '@untitledui/icons'
import { useResource } from '../state/hooks'

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
        title={resource.name ?? undefined}
        className="my-px max-h-96 max-w-full rounded-md shadow-xs"
      />
    )
  }

  return (
    <a
      href={resource.url}
      download={resource.name ?? id}
      className="my-px inline-flex max-w-full items-center gap-1.5 rounded-md bg-gray-100 px-2 py-1 text-[12px] text-gray-600 no-underline shadow-xs hover:text-gray-900"
      title={`Save ${resource.name ?? 'file'}`}
    >
      <File02 size={13} className="shrink-0 text-gray-400" />
      <span className="truncate">{resource.name || resource.mimeType}</span>
    </a>
  )
}
