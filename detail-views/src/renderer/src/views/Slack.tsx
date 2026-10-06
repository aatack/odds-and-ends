import { memo } from 'react'
import type { Entity } from '../../../core/types.ts'
import { MessageBody, MessageRow, showsAuthor, showsTime } from './messages.tsx'
import { Composer, Row, Status } from './primitives.tsx'
import type { FocusProps } from './types.ts'

/** Every conversation I am in, unread first. */
export function SlackHome(props: FocusProps) {
  const { focus } = props
  if (focus.compose === 'slack-token') {
    return (
      <div className="pane centred">
        <Composer kind="slack-token" placeholder="xoxp-…" {...composerProps(props)} />
        <Status error={focus.error} />
      </div>
    )
  }
  return (
    <div className="pane">
      <div className="list">
        {focus.children.map((child, index) => (
          <ConversationRow key={child.id} entity={child} selected={index === props.cursor} onSelect={props.onSelect} />
        ))}
      </div>
      <Status error={focus.error} />
    </div>
  )
}

const ConversationRow = memo(function ConversationRow(props: {
  entity: Entity
  selected: boolean
  onSelect(id: string): void
}) {
  const unread = Number(props.entity.data.unread ?? 0)
  return (
    <Row id={props.entity.id} selected={props.selected} className={unread ? 'unread' : 'read'} onSelect={props.onSelect}>
      <span className="grow">{String(props.entity.data.title)}</span>
      {unread > 0 && <span className="count">{unread >= 100 ? '99+' : unread}</span>}
    </Row>
  )
})

function composerProps(props: FocusProps) {
  return { draft: props.draft, composing: props.composing, onDraft: props.onDraft, onCompose: props.onCompose }
}

/** Grouped like Slack: a name only where the speaker changes or time passes. */
export function SlackConversation(props: FocusProps) {
  const { focus } = props
  return (
    <div className="pane">
      <div className="title">{String(focus.entity?.data.title ?? '')}</div>
      <div className="list chat">
        {focus.children.map((child, index) => (
          <MessageRow
            key={child.id}
            entity={child}
            author={showsAuthor(focus.children, index)}
            time={showsTime(focus.children, index)}
            selected={index === props.cursor}
            onSelect={props.onSelect}
            onOpen={props.onOpen}
            onImage={props.onImage}
          />
        ))}
        {focus.loading && <div className="loading">Loading…</div>}
      </div>
      <Status error={focus.error} />
      {focus.compose === 'slack' && props.composing && <Composer kind="slack" {...composerProps(props)} />}
    </div>
  )
}

/** A message and its replies. */
export function SlackThread(props: FocusProps) {
  const { focus } = props
  const all = focus.entity ? [focus.entity, ...focus.children] : focus.children
  return (
    <div className="pane">
      <div className="list chat">
        {focus.entity && <MessageBody entity={focus.entity} author={true} time={true} replies={false} onOpen={props.onOpen} onImage={props.onImage} />}
        <div className="divider" />
        {focus.children.map((child, index) => (
          <MessageRow
            key={child.id}
            entity={child}
            author={showsAuthor(all, index + 1) || index === 0}
            time={showsTime(all, index + 1) || index === 0}
            selected={index === props.cursor}
            onSelect={props.onSelect}
            onOpen={props.onOpen}
            onImage={props.onImage}
          />
        ))}
        {focus.loading && <div className="loading">Loading…</div>}
      </div>
      <Status error={focus.error} />
      {focus.compose === 'slack' && props.composing && <Composer kind="slack" {...composerProps(props)} />}
    </div>
  )
}
