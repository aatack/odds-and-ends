import { memo } from 'react'
import { MessageBody, MessageRow, showsAuthor, showsTime } from './messages.tsx'
import { authorColour } from '../format.ts'
import type { PillProps, RowProps } from './kindTypes.ts'
import { Composer, HeaderPill, Row, Status } from './primitives.tsx'
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
          <ConversationRow
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

export const ConversationRow = memo(function ConversationRow(props: RowProps) {
  const unread = Number(props.entity.data.unread ?? 0)
  return (
    <Row id={props.entity.id} selected={props.selected} className={unread ? 'unread' : 'read'} onSelect={props.onSelect}>
      <span className="grow">{String(props.entity.data.title)}</span>
      {unread > 0 && <span className="count">{unread >= 100 ? '99+' : unread}</span>}
    </Row>
  )
})

export function ConversationPill(props: PillProps) {
  const unread = Number(props.entity.data.unread ?? 0)
  return (
    <>
      <span className="item-name">{String(props.entity.data.title ?? props.entity.id)}</span>
      {unread > 0 && <span className="count">{unread >= 100 ? '99+' : unread}</span>}
    </>
  )
}

/** A message in a list other than its own conversation: always says who and when. */
export function SlackMessageRow(props: RowProps) {
  return <MessageRow {...props} author time />
}

export function MessagePill(props: PillProps) {
  const { data } = props.entity
  const firstLine = String(data.text ?? '').split('\n')[0]
  return (
    <span className="item-name">
      <span style={{ color: authorColour(String(data.authorKey)), fontWeight: 700 }}>{String(data.author ?? '')}</span>{' '}
      {firstLine}
    </span>
  )
}

export function UserPill(props: PillProps) {
  return <span className="item-name">{String(props.entity.data.name ?? props.entity.id)}</span>
}

export function SlackHomePill() {
  return <span className="item-name">Slack</span>
}

function composerProps(props: FocusProps) {
  return { draft: props.draft, composing: props.composing, onDraft: props.onDraft, onCompose: props.onCompose }
}

/** Grouped like Slack: a name only where the speaker changes or time passes. */
export function SlackConversation(props: FocusProps) {
  const { focus } = props
  return (
    <div className="pane">
      {props.headed !== false && (
        <div className="title">
          <HeaderPill entity={focus.entity} />
        </div>
      )}
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
