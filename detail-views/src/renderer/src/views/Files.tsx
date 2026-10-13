import { memo } from 'react'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { HeaderPill, Highlight } from './primitives.tsx'

// A file pasted into a topic: kept under the app's data directory, named by its text.

/** `12 KB`: how big a file is. */
export function fileSize(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`
  if (bytes < 1024 * 1024) return `${Math.round(bytes / 1024)} KB`
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`
}

const nameOf = (data: Record<string, unknown>): string => String(data.text ?? 'File')
const sizeOf = (data: Record<string, unknown>): string => (typeof data.size === 'number' ? fileSize(data.size) : '')

export function FilePill(props: PillProps) {
  return (
    <span className="item-name">
      <span className="file-mark" role="img" aria-label="File" /> <Highlight text={nameOf(props.entity.data)} find={props.findText} />
    </span>
  )
}

export function FileOverview(props: OverviewProps) {
  const { data } = props.entity
  return (
    <>
      <div className="title">
        <HeaderPill entity={props.entity} />
      </div>
      <div className="facts muted">
        <span>{sizeOf(data)}</span>
        <span>{String(data.path ?? '')}</span>
      </div>
    </>
  )
}

export const FileRow = memo(function FileRow(props: RowProps) {
  return (
    <span className="line">
      <FilePill entity={props.entity} findText={props.findText} /> <span className="muted">{sizeOf(props.entity.data)}</span>
    </span>
  )
})
