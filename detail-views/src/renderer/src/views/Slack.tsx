import { memo } from 'react'
import { MessageBody, MessageRow, showsAuthor, showsTime } from './messages.tsx'
import { authorColour, cursorTime } from '../format.ts'
import type { PillProps, RowProps } from './kindTypes.ts'
import { Composer, HeaderPill, ItemPill, Row, Status, Working } from './primitives.tsx'
import type { FocusProps } from './types.ts'

/** Every conversation I am in, most recent first. */
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
      {props.headed !== false && (
        <div className="title">
          <Polled data={focus.entity?.data} now={props.now} />
          <History {...props} />
        </div>
      )}
      <div className="list">
        {focus.children.map((child, index) => {
          const Row = child.type === 'slack.message' ? ThreadRow : ConversationRow
          return (
            <Row
              key={child.id}
              entity={child}
              selected={index === props.cursor}
              onSelect={props.onSelect}
              onOpen={props.onOpen}
              onImage={props.onImage}
            />
          )
        })}
      </div>
      <Status error={focus.error ?? ((focus.entity?.data['watch.error'] as string | null | undefined) ?? null)} />
    </div>
  )
}

/**
 * Where what is cached starts, and the button that loads further back. Gone
 * once nothing is older.
 */
function History(props: FocusProps) {
  const data = props.focus.entity?.data ?? {}
  if (!props.focus.older) return null
  return (
    <span className="history">
      {typeof data.from === 'string' && <span title="Everything since is loaded">{cursorTime(data.from)}</span>}
      {props.onOlder && (
        <Working busy={props.working?.includes('older')} label="Older" busyLabel="Loading…" onClick={props.onOlder} />
      )}
    </span>
  )
}

/** How long ago the watch last looked, and how many new messages it found. */
function Polled(props: { data: Record<string, unknown> | undefined; now: number | undefined }) {
  const at = Number(props.data?.polledAt)
  if (!at || !props.now) return null
  const seconds = Math.max(0, Math.round((props.now - at) / 1000))
  return (
    <span className="history" title="Last check for new messages">
      {seconds}s ago · {Number(props.data?.found ?? 0)} new
    </span>
  )
}

export function WatchPill(props: PillProps) {
  return <span className="item-name">{Number(props.entity.data.found ?? 0)} new</span>
}

/** A thread in the workspace's list: where it is, who started it, and how far it has gone. */
export const ThreadRow = memo(function ThreadRow(props: RowProps) {
  const { data } = props.entity
  const replies = Number(data.replyCount ?? 0)
  return (
    <Row id={props.entity.id} selected={props.selected} onSelect={props.onSelect}>
      <span className="grow line">
        <span className="muted">{String(data.where ?? '')}</span>{' '}
        <span style={{ color: authorColour(String(data.authorKey)), fontWeight: 700 }}>{String(data.author ?? '')}</span>{' '}
        {String(data.text ?? '').split('\n')[0]}
      </span>
      {replies > 0 && <span className="muted">{replies}</span>}
    </Row>
  )
})

export const ConversationRow = memo(function ConversationRow(props: RowProps) {
  return (
    <Row id={props.entity.id} selected={props.selected} onSelect={props.onSelect}>
      <span className="grow">{String(props.entity.data.title)}</span>
    </Row>
  )
})

export function ConversationPill(props: PillProps) {
  return <span className="item-name">{String(props.entity.data.title ?? props.entity.id)}</span>
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
          <History {...props} />
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
      {props.headed !== false && (
        <div className="title">
          {typeof focus.entity?.data.conversation === 'string' && (
            <ItemPill id={focus.entity.data.conversation} fallback={String(focus.entity.data.where ?? '')} />
          )}
          {props.onHideChat && typeof focus.entity?.data.conversation === 'string' && (
            <span className="history">
              <button className="action" title="Hide this chat and its threads (Shift+Backspace)" onClick={props.onHideChat}>
                Hide chat
              </button>
            </span>
          )}
          <History {...props} />
        </div>
      )}
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
