import { memo, useContext, useEffect, useRef, useState } from 'react'
import { branchEntityId } from '../../../core/modules/github/view.ts'
import type { Entity } from '../../../core/types.ts'
import { shortTime } from '../format.ts'
import { ChatPage, ClaudeMark, DirectorySelect, elapsed, type BubbleOf } from './Claude.tsx'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { Button, ClockContext, Highlight, ItemContext, ItemPill } from './primitives.tsx'

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

/** Claude's answers are bubbles on the left, with its mark; my messages, and whatever I pasted, are lines across the width, with no bubble or mark. */
const topicBubble: BubbleOf = (row) => (row.entity.type === 'claude.response' ? 'theirs' : null)

/**
 * A topic: its chat, a flat list with nothing selectable; the box for the
 * next message below it only once Enter opens it; and, at its top right,
 * what the topic runs in.
 */
export function TopicDetail(props: OverviewProps) {
  return (
    <ChatPage
      props={props}
      field="topic"
      placeholder="Message"
      bubble={topicBubble}
      flat
      onDemand
      corner={props.interactive ? <TopicContext props={props} /> : null}
    />
  )
}

/**
 * A small button at the top right of a topic, showing what the topic runs
 * in while it is hovered, or until it is pressed again: its directory, to
 * change, a new worktree of it when it is in a git repo, and the worktree's
 * branch. (Later, any items pinned to the topic.)
 */
function TopicContext(props: { props: OverviewProps }) {
  const chat = props.props
  const { data } = chat.entity
  const { item } = useContext(ItemContext)
  const [hovered, setHovered] = useState(false)
  const [pinned, setPinned] = useState(false)
  // While the directory's box has the keyboard (its menu is drawn outside the card), the card stays.
  const [choosing, setChoosing] = useState(false)
  const repos = Array.isArray(item('claude')?.data.repos) ? (item('claude')!.data.repos as string[]) : []
  const worktree = typeof data.worktree === 'string' && data.worktree ? data.worktree : null
  // A worktree's own worktree is made from the directory it was made from.
  const repo = worktree ? (typeof data.origin === 'string' && data.origin ? data.origin : null) : typeof data.cwd === 'string' && repos.includes(data.cwd) ? data.cwd : null
  const branch = typeof data.branch === 'string' && data.branch ? data.branch : null
  const github = typeof data.github === 'string' && data.github ? data.github : null
  const running = typeof data.runningSince === 'number'
  return (
    <div className="topic-context" onMouseEnter={() => setHovered(true)} onMouseLeave={() => setHovered(false)}>
      <Button label="Context" active={pinned} onClick={() => setPinned(!pinned)} />
      {(hovered || pinned || choosing) && (
        <div className="topic-context-card">
          <div className="topic-context-line">
            {chat.onSetDirectory ? (
              <span onFocus={() => setChoosing(true)} onBlur={() => setChoosing(false)}>
                <DirectorySelect {...chat} worktrees={false} />
              </span>
            ) : (
              <span>{String(data.cwd ?? '')}</span>
            )}
            {repo && chat.onSetDirectory && (
              <Button
                label="New worktree"
                busy={chat.working?.includes('directory')}
                disabled={running && 'Claude is answering: wait for it to finish'}
                onClick={() => chat.onSetDirectory?.(repo, true)}
              />
            )}
          </div>
          {branch && (
            <div className="topic-context-line">
              {github ? (
                <ItemPill id={branchEntityId(github, branch)} fallback={branch} />
              ) : (
                <span className="item-pill">
                  <span className="item-name">{branch}</span>
                </span>
              )}
            </div>
          )}
        </div>
      )}
    </div>
  )
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
