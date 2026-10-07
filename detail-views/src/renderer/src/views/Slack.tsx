import { memo } from 'react'
import { authorColour, cursorTime } from '../format.ts'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { MessageBody, MessageRow } from './messages.tsx'
import { Button, Composer, HeaderPill, Highlight, ItemPill, Status } from './primitives.tsx'

const text = (data: Record<string, unknown>, fallback = ''): string =>
  typeof data.text === 'string' && data.text ? data.text : fallback

// --- The workspace -----------------------------------------------------------------

/** Every conversation and thread I follow, most recent first; or, before a token, the box for one. */
export function SlackHomeOverview(props: OverviewProps) {
  const { view, entity } = props
  if (view.compose === 'slack-token') {
    return (
      <div className="centred-box">
        <Composer
          kind="slack-token"
          placeholder="xoxp-…"
          draft={props.draft}
          composing={props.interactive}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      </div>
    )
  }
  return (
    <>
      <div className="title">
        <HeaderPill entity={entity} />
        <Polled data={entity.data} now={props.now} />
        <History {...props} />
      </div>
      <Status error={(entity.data['watch.error'] as string | null | undefined) ?? null} />
    </>
  )
}

export function SlackHomePill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={text(props.entity.data, 'Slack')} find={props.findText} />
    </span>
  )
}

/**
 * Where what is cached starts, and the button that loads further back. Gone
 * once nothing is older.
 */
function History(props: OverviewProps) {
  const data = props.entity.data
  if (!props.view.older) return null
  return (
    <span className="history">
      {typeof data.from === 'string' && <span title="Everything since is loaded">{cursorTime(data.from)}</span>}
      {props.interactive && props.onOlder && (
        <Button busy={props.working?.includes('older')} label="Older" busyLabel="Loading…" onClick={props.onOlder} />
      )}
    </span>
  )
}

/** How long ago the watch last looked, and how many new messages it found. */
function Polled(props: { data: Record<string, unknown>; now: number | undefined }) {
  const at = Number(props.data.polledAt)
  if (!at || !props.now) return null
  const seconds = Math.max(0, Math.round((props.now - at) / 1000))
  return (
    <span className="history" title="Last check for new messages">
      {seconds}s ago · {Number(props.data.found ?? 0)} new
    </span>
  )
}

export function WatchPill(props: PillProps) {
  return <span className="item-name">{Number(props.entity.data.found ?? 0)} new</span>
}

// --- Conversations -------------------------------------------------------------------

export function ConversationOverview(props: OverviewProps) {
  return (
    <div className="title">
      <HeaderPill entity={props.entity} />
      <History {...props} />
    </div>
  )
}

export const ConversationRow = memo(function ConversationRow(props: RowProps) {
  return (
    <span className="line">
      <Highlight text={text(props.entity.data, props.entity.id)} find={props.findText} />
    </span>
  )
})

export function ConversationPill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={text(props.entity.data, props.entity.id)} find={props.findText} />
    </span>
  )
}

// --- Messages --------------------------------------------------------------------------

/** A message on its own: its chat, then the message, its thread below. */
export function MessageOverview(props: OverviewProps) {
  const { entity } = props
  return (
    <>
      <div className="title">
        {typeof entity.data.conversation === 'string' && <ItemPill id={entity.data.conversation} fallback={String(entity.data.where ?? '')} />}
        {props.interactive && props.onHideChat && typeof entity.data.conversation === 'string' && (
          <span className="history">
            <Button
              label="Hide chat"
              busyLabel="Hiding…"
              busy={props.working?.includes('hide')}
              title="Hide this chat and its threads (Shift+Backspace)"
              onClick={props.onHideChat}
            />
          </span>
        )}
        <History {...props} />
      </div>
      <MessageBody entity={entity} author replies={false} onOpen={props.onOpen} onImage={props.onImage} findText={props.findText} />
    </>
  )
}

/**
 * A message as a row. In the workspace it is a thread: where it is, who
 * started it, and how far it has gone. Anywhere else it is chat, grouped with
 * the message above.
 */
export const SlackMessageRow = memo(function SlackMessageRow(props: RowProps) {
  if (props.parent?.type === 'slack.home') return <ThreadLine {...props} />
  return <MessageRow {...props} always={props.parent?.type !== 'slack.conversation' && props.parent?.type !== 'slack.message'} />
})

function ThreadLine(props: RowProps) {
  const { data } = props.entity
  const replies = Number(data.replyCount ?? 0)
  return (
    <span className="line-row">
      <span className="line">
        <span className="muted">{String(data.where ?? '')}</span>{' '}
        <span style={{ color: authorColour(String(data.authorKey)), fontWeight: 700 }}>{String(data.author ?? '')}</span>{' '}
        <Highlight text={String(data.text ?? '').split('\n')[0]} find={props.findText} />
      </span>
      {replies > 0 && <span className="muted">{replies}</span>}
    </span>
  )
}

export function MessagePill(props: PillProps) {
  const { data } = props.entity
  const firstLine = String(data.text ?? '').split('\n')[0]
  return (
    <span className="item-name">
      <span style={{ color: authorColour(String(data.authorKey)), fontWeight: 700 }}>{String(data.author ?? '')}</span>{' '}
      <Highlight text={firstLine} find={props.findText} />
    </span>
  )
}

// --- Users -----------------------------------------------------------------------------

export function UserPill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={text(props.entity.data, props.entity.id)} find={props.findText} />
    </span>
  )
}
