import React from 'react'
import { useResource } from '../state/hooks'

/**
 * The bytes behind a `type: 'file'` row. Images show; anything else is named.
 *
 * Read-only, deliberately: pasting or capturing a file is a bigger piece of work
 * (a camera intent, a picker, base64 over the wire) and this app is for reading and
 * writing text away from a desk. A screenshot pasted from the laptop should still
 * be *visible* here, which is what this is for.
 */
export function FileView({
  id,
  mimeType,
  alt,
}: {
  id: string
  mimeType?: string
  alt?: string
}): React.JSX.Element {
  const resource = useResource(id)

  if (resource.status === 'loading') {
    return <span className="text-[13px] text-gray-400">Loading the file…</span>
  }
  if (resource.status === 'missing') {
    return <span className="text-[13px] text-gray-400">No file bytes available</span>
  }
  if (resource.status === 'error') {
    return <span className="text-[13px] text-error-600">{resource.message}</span>
  }

  if ((resource.mimeType || mimeType || '').startsWith('image/')) {
    return (
      <img
        src={resource.dataUrl}
        alt={alt ?? resource.name ?? 'Image'}
        className="max-h-[60vh] w-auto max-w-full rounded-md"
      />
    )
  }

  return (
    <a
      href={resource.dataUrl}
      download={resource.name ?? id}
      className="text-[14px] text-brand-600 underline underline-offset-2"
    >
      {resource.name ?? resource.mimeType}
    </a>
  )
}
