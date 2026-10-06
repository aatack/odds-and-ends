import { memo } from 'react'
import type { PillProps, RowProps } from './kindTypes.ts'
import { Composer, HeaderPill, PillContent, Row, RowFor, Status } from './primitives.tsx'
import type { FocusProps } from './types.ts'

/** A task, or the root of all of them, with what sits under it. */
export function Tasks(props: FocusProps) {
  const { focus } = props
  const isTask = focus.entity?.type === 'task'
  return (
    <div className="pane">
      {isTask && props.headed !== false && (
        <div className="title">
          <HeaderPill entity={focus.entity} />
        </div>
      )}
      <div className="list">
        {focus.children.map((child, index) => (
          <RowFor
            key={child.id}
            entity={child}
            selected={index === props.cursor}
            onSelect={props.onSelect}
            onOpen={props.onOpen}
            onImage={props.onImage}
          />
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

export const TaskRow = memo(function TaskRow(props: RowProps) {
  const { data } = props.entity
  return (
    <Row id={props.entity.id} selected={props.selected} className={data.done ? 'done' : ''} onSelect={props.onSelect}>
      <span className="check">{data.done ? '✓' : '○'}</span>
      <span className="grow">{String(data.text)}</span>
    </Row>
  )
})

export function TaskPill(props: PillProps) {
  const { data } = props.entity
  return (
    <>
      <span className="check">{data.done ? '✓' : '○'}</span>
      <span className="item-name">{String(data.text)}</span>
    </>
  )
}

export function TasksHomePill() {
  return <span className="item-name">Tasks</span>
}

/** A row for any item with nothing more to say than its pill. */
export const GenericRow = memo(function GenericRow(props: RowProps) {
  return (
    <Row id={props.entity.id} selected={props.selected} onSelect={props.onSelect}>
      <span className="grow pill-row">
        <PillContent entity={props.entity} />
      </span>
    </Row>
  )
})

/** Anything without a view of its own. */
export function Generic(props: FocusProps) {
  const { focus } = props
  return (
    <div className="pane">
      {props.headed !== false && (
        <div className="title">
          <HeaderPill entity={focus.entity} />
        </div>
      )}
      <pre className="data">{JSON.stringify(focus.entity?.data ?? {}, null, 2)}</pre>
      <div className="list">
        {focus.children.map((child, index) => (
          <RowFor
            key={child.id}
            entity={child}
            selected={index === props.cursor}
            onSelect={props.onSelect}
            onOpen={props.onOpen}
            onImage={props.onImage}
          />
        ))}
      </div>
      <Status error={focus.error} />
    </div>
  )
}
