import React from 'react'
import { ResourceView } from '../Resource'
import { EntityMarkdown } from '../EntityMarkdown'
import { useGetEntities } from '../../state/hooks'
import { str } from '../../state/derive'
import { EMPTY, RowText, TEXT } from './text'
import type { CardProps, FocusProps } from './types'

// Every type's card, and the focus of the types that have one. A card is a row's
// worth: the gist, and the one action a type can't do without. A focus is the
// same entity with the whole frame to itself.

/** An entity's values, for a focus — which is handed an id, not a row. */
function useValues(id: string): Record<string, unknown> {
  return useGetEntities()([id])[id].values
}

/** A page's edges, for a focus that is one thing rather than a list of rows. */
function Page({ children }: { children: React.ReactNode }): React.JSX.Element {
  return <div className="h-full overflow-auto bg-white px-6 py-5">{children}</div>
}

// --- Note -------------------------------------------------------------------

/** What the user wrote, and nothing else. */
export function NoteCard({ row, highlight }: CardProps): React.JSX.Element {
  return row.text ? <RowText row={row} highlight={highlight} /> : EMPTY
}

// --- File -------------------------------------------------------------------

/** A thumbnail or a chip, and whatever the user typed as a caption under it. */
export function FileCard({ row, highlight }: CardProps): React.JSX.Element {
  return (
    <>
      <ResourceView id={row.id} mimeType={row.mimeType} alt={row.text} />
      <RowText row={row} highlight={highlight} muted />
    </>
  )
}

/** The whole file: an image as large as the page allows, text read out, media playing. */
export function FileFocus({ entityId }: FocusProps): React.JSX.Element {
  const values = useValues(entityId)
  const caption = str(values.text)
  return (
    <Page>
      <ResourceView id={entityId} mimeType={str(values.mimeType)} alt={caption} size="focus" />
      {caption && <EntityMarkdown text={caption} className={`${TEXT} mt-2 text-gray-500`} />}
    </Page>
  )
}

// --- URL --------------------------------------------------------------------

/** The page a `url` entity points at, if its `url` value is one the app opens. */
export const urlOf = (values: Record<string, unknown>): string | null => {
  const url = str(values.url)
  return url && /^https?:\/\//i.test(url) ? url : null
}

/** Where a page is, as short as it reads. */
const hostOf = (url: string): string => {
  try {
    return new URL(url).host || url
  } catch {
    return url
  }
}

/** What the page is called, if the entity says, and where it is. */
export function UrlCard({ row, highlight }: CardProps): React.JSX.Element {
  const url = str(useValues(row.id).url)
  return (
    <>
      {row.text ? <RowText row={row} highlight={highlight} /> : !url && EMPTY}
      {url && (
        <span className={`${TEXT} truncate font-sans text-[12px] text-gray-400`}>
          {row.text ? hostOf(url) : url}
        </span>
      )}
    </>
  )
}

/** The page itself, in the same locked-down webview a peek uses. */
export function UrlFocus({ entityId }: FocusProps): React.JSX.Element {
  const url = urlOf(useValues(entityId))
  if (!url) {
    return (
      <Page>
        <p className="text-[13px] text-gray-400">No page: its `url` is not an http(s) address.</p>
      </Page>
    )
  }
  return (
    <div className="relative h-full">
      <webview src={url} partition="persist:preview" className="absolute inset-0" />
    </div>
  )
}
