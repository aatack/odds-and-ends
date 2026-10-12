import { memo, useContext, useEffect, useRef } from 'react'
import type { Entity } from '../../../core/types.ts'
import { shortTime } from '../format.ts'
import { ChatPage, ClaudeMark, elapsed, type BubbleOf } from './Claude.tsx'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { ClockContext, Highlight } from './primitives.tsx'

// The topics mode: one list of topics on the left, the chat of the one open
// filling the rest. A message is only written into the chat; Ctrl+Enter sends
// every one since the last dispatch to Claude at once.

const nameOf = (entity: Entity): string => (typeof entity.data.text === 'string' && entity.data.text ? entity.data.text : 'Topic')
const running = (entity: Entity): number | null => (typeof entity.data.runningSince === 'number' ? entity.data.runningSince : null)

/** When a topic was last active, or, while Claude answers in it, how long it has been. */
function When(props: { entity: Entity }) {
  const now = useContext(ClockContext)
  const since = running(props.entity)
  return <span className="muted">{since !== null ? elapsed(now - since) : shortTime(String(props.entity.updatedAt / 1000))}</span>
}

export function TopicPill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={nameOf(props.entity)} find={props.findText} />
    </span>
  )
}

export const TopicRow = memo(function TopicRow(props: RowProps) {
  return (
    <span className="line">
      <Highlight text={nameOf(props.entity)} find={props.findText} /> <When entity={props.entity} />
    </span>
  )
})

/** My messages are bubbles on the right, Claude's answers on the left; anything under them is a line. */
const topicBubble: BubbleOf = (row) => (row.entity.type === 'claude.response' ? 'theirs' : row.depth === 1 && row.entity.type === 'note' ? 'mine' : null)

/** A topic: its chat, and the box for the next message below it. */
export function TopicDetail(props: OverviewProps) {
  return <ChatPage props={props} field="topic" placeholder="Message" bubble={topicBubble} />
}

export interface TopicListProps {
  topics: Entity[]
  /** The topic open, if one is. */
  open: string | null
  /** The name being typed for a new one. */
  draft: string
  busy: boolean
  onDraft(text: string): void
  onOpen(id: string): void
}

/** Every topic, the most recently active first, as a chat app lists its chats; the box at its head names a new one. */
export function TopicList(props: TopicListProps) {
  return (
    <nav className="topic-list">
      <input
        className="find topic-new"
        data-field="topic-name"
        placeholder={props.busy ? 'Making…' : 'New topic'}
        spellCheck={false}
        value={props.draft}
        onChange={(event) => props.onDraft(event.target.value)}
      />
      <div className="topic-cards">
        {props.topics.map((topic) => (
          <TopicCard key={topic.id} entity={topic} open={topic.id === props.open} onOpen={props.onOpen} />
        ))}
      </div>
    </nav>
  )
}

const TopicCard = memo(function TopicCard(props: { entity: Entity; open: boolean; onOpen(id: string): void }) {
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    if (props.open) ref.current?.scrollIntoView({ block: 'nearest' })
  }, [props.open])
  return (
    <div ref={ref} className={`chat-card${props.open ? ' selected' : ''}`} onClick={() => props.onOpen(props.entity.id)}>
      <div className="chat-card-top">
        {running(props.entity) !== null && <ClaudeMark />}
        <span className="chat-card-name">{nameOf(props.entity)}</span>
        <span className="chat-card-time">
          <When entity={props.entity} />
        </span>
      </div>
    </div>
  )
})
