import React from 'react'
import { URL_PREFIX } from '../../../src/core/builtins'
import { Markdown } from '../components/ui/Markdown'
import { cn } from '../helpers/cn'
import { str, type EntityRow } from '../state/derive'
import { useGetEntities } from '../state/hooks'
import { FileView } from './FileView'

// How a row draws its body, chosen by the entity's `type` — the phone's small
// echo of the desktop's kind registry (`../../src/renderer/src/components/types`).
// Adding a type is an entry here, not another branch in `Outline`: the outline
// asks `RowBody` for the body and renders it, knowing nothing of files or urls.

// User-written entity text, in the serif the desktop app uses for the same thing.
// It wraps rather than truncating — a row is as tall as it needs to be. Lives here
// because this is where a row's text is drawn; `Outline` imports it for its editors.
export const TEXT = 'entity-text block font-serif text-[16px] leading-[1.45]'

export interface RowBodyProps {
  row: EntityRow
  /** The resolved class for this row's text — section weight, collapsed tone. */
  textClass: string
  /** The inline style a section heading carries, if any. */
  headingStyle?: React.CSSProperties
}

type RowBody = (props: RowBodyProps) => React.JSX.Element

/** The address a url entity written to a `url:<address>` id carries in its id. */
const addressFromId = (id: string): string | undefined =>
  id.startsWith(URL_PREFIX) ? id.slice(URL_PREFIX.length) : undefined

/** Where a page is, as short as it reads: its host, or the whole thing if it has none. */
const hostOf = (url: string): string => {
  try {
    return new URL(url).host || url
  } catch {
    return url
  }
}

/** The default: markdown, or a quiet placeholder for a line with nothing written yet. */
function NoteBody({ row, textClass, headingStyle }: RowBodyProps): React.JSX.Element {
  // Rendered, not printed: a line with no markup comes out exactly as the plain
  // text did, and the blocks are there for the rows that use them.
  return row.text ? (
    <Markdown text={row.text} className={textClass} style={headingStyle} />
  ) : (
    <span className={cn(TEXT, 'text-gray-400 italic')}>Empty</span>
  )
}

/** The bytes behind a `type: 'file'` row, with its caption beneath. */
function FileBody({ row }: RowBodyProps): React.JSX.Element {
  return (
    <>
      <FileView id={row.id} mimeType={row.mimeType} alt={row.text} />
      {row.text && <Markdown text={row.text} className={cn(TEXT, 'text-gray-500')} />}
    </>
  )
}

/**
 * A web address the store serves as an entity: its note if any, then where it goes —
 * the host when it has words of its own, the whole address when it doesn't. The `url`
 * value is the address; a `url:<address>` id is the fallback for one never written.
 */
function UrlBody({ row, textClass, headingStyle }: RowBodyProps): React.JSX.Element {
  const address = str(useGetEntities()([row.id])[row.id].values.url) ?? addressFromId(row.id)
  // Only an http(s) page is somewhere a tap can go; anything else is shown as text.
  const openable = address !== undefined && /^https?:\/\//i.test(address)
  const shown = address === undefined ? undefined : row.text ? hostOf(address) : address
  const chrome = 'block truncate font-sans text-[13px] text-gray-500'
  return (
    <>
      {row.text ? (
        <Markdown text={row.text} className={textClass} style={headingStyle} />
      ) : address === undefined ? (
        <span className={cn(TEXT, 'text-gray-400 italic')}>Empty</span>
      ) : null}
      {shown !== undefined &&
        (openable ? (
          <a href={address} target="_blank" rel="noreferrer" className={cn(chrome, 'active:text-gray-700')}>
            {shown}
          </a>
        ) : (
          <span className={chrome}>{shown}</span>
        ))}
    </>
  )
}

const bodies: Record<string, RowBody> = {
  file: FileBody,
  url: UrlBody,
}

/** The body to draw for a row, by its `type` — `NoteBody` for a type with no entry. */
export function RowBody(props: RowBodyProps): React.JSX.Element {
  // A component, not a call: a body may use hooks (a url reads its `url` value), so
  // it has to render as its own element rather than inline in this one.
  const Body = (props.row.type && bodies[props.row.type]) || NoteBody
  return <Body {...props} />
}
