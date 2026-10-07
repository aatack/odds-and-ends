import { memo } from 'react'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { HeaderPill, PillContent } from './primitives.tsx'

const text = (data: Record<string, unknown>): string => (typeof data.text === 'string' ? data.text : '')

/** A note: text that means nothing more. */
export function NoteOverview(props: OverviewProps) {
  return <div className="note-overview">{text(props.entity.data)}</div>
}

export const NoteRow = memo(function NoteRow(props: RowProps) {
  return <span className="note">{text(props.entity.data)}</span>
})

export function NotePill(props: PillProps) {
  return <span className="item-name">{text(props.entity.data) || props.fallback}</span>
}

/** A task, from before notes: ticked with Space. */
export function TaskOverview(props: OverviewProps) {
  return (
    <div className="title">
      <HeaderPill entity={props.entity} />
    </div>
  )
}

export const TaskRow = memo(function TaskRow(props: RowProps) {
  const { data } = props.entity
  return (
    <span className={`line${data.done ? ' done' : ''}`}>
      <span className="check">{data.done ? '✓' : '○'}</span> {text(data)}
    </span>
  )
})

export function TaskPill(props: PillProps) {
  const { data } = props.entity
  return (
    <>
      <span className="check">{data.done ? '✓' : '○'}</span>
      <span className="item-name">{text(data)}</span>
    </>
  )
}

export function HomePill(props: PillProps) {
  return <span className="item-name">{text(props.entity.data) || props.entity.id}</span>
}

/** Any item heading its view with nothing more to say than its name. */
export function PillOverview(props: OverviewProps) {
  return (
    <div className="title">
      <HeaderPill entity={props.entity} />
    </div>
  )
}

/** Any item as a row with nothing more to say than its pill. */
export const PillRow = memo(function PillRow(props: RowProps) {
  return (
    <span className="pill-row">
      <PillContent entity={props.entity} />
    </span>
  )
})
