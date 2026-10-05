import { memo } from 'react'
import type { Entity } from '../../../core/types.ts'
import { Composer, Row, Status } from './primitives.tsx'
import type { FocusProps } from './types.ts'

/** A task, or the root of all of them, with what sits under it. */
export function Tasks(props: FocusProps) {
  const { focus } = props
  const isTask = focus.entity?.type === 'task'
  return (
    <div className="pane">
      {isTask && <div className="title">{String(focus.entity!.data.text)}</div>}
      <div className="list">
        {focus.children.map((child, index) => (
          <TaskRow key={child.id} entity={child} selected={index === props.cursor} onSelect={props.onSelect} onOpen={props.onOpen} />
        ))}
      </div>
      <Status error={focus.error} />
      {props.composing && (
        <Composer
          kind="task"
        draft={props.draft}
        composing={props.composing}
        onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      )}
    </div>
  )
}

const TaskRow = memo(function TaskRow(props: {
  entity: Entity
  selected: boolean
  onSelect(id: string): void
  onOpen(id: string): void
}) {
  const { data } = props.entity
  if (props.entity.type !== 'task') return <GenericRow {...props} />
  return (
    <Row id={props.entity.id} selected={props.selected} className={data.done ? 'done' : ''} onSelect={props.onSelect} onOpen={props.onOpen}>
      <span className="check">{data.done ? '✓' : '○'}</span>
      <span className="grow">{String(data.text)}</span>
    </Row>
  )
})

export const GenericRow = memo(function GenericRow(props: {
  entity: Entity
  selected: boolean
  onSelect(id: string): void
  onOpen(id: string): void
}) {
  const { data } = props.entity
  const label = data.title ?? data.text ?? data.name ?? props.entity.id
  return (
    <Row id={props.entity.id} selected={props.selected} onSelect={props.onSelect} onOpen={props.onOpen}>
      <span className="grow">{String(label)}</span>
    </Row>
  )
})

/** Anything without a view of its own. */
export function Generic(props: FocusProps) {
  const { focus } = props
  return (
    <div className="pane">
      <pre className="data">{JSON.stringify(focus.entity?.data ?? {}, null, 2)}</pre>
      <div className="list">
        {focus.children.map((child, index) => (
          <GenericRow key={child.id} entity={child} selected={index === props.cursor} onSelect={props.onSelect} onOpen={props.onOpen} />
        ))}
      </div>
      <Status error={focus.error} />
    </div>
  )
}
