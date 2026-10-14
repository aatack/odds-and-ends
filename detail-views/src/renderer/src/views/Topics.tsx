import { memo, useContext, useEffect, useRef } from 'react'
import type { Entity } from '../../../core/types.ts'
import { shortTime } from '../format.ts'
import { ChatPage, ClaudeMark, elapsed, type BubbleOf } from './Claude.tsx'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { PathPicker } from './Paths.tsx'
import { waitOn } from '../../../core/modules/topics/view.ts'
import type { TopicProps } from './types.ts'
import { Button, ClockContext, Highlight, ItemContext, ItemPill, KindsContext } from './primitives.tsx'

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
 * next message below it (Enter gives it the keyboard); and, at its top
 * right, a picker for the path it runs in, and Resolve.
 */
export function TopicDetail(props: OverviewProps) {
  const topic = props.interactive ? props.topic : undefined
  const target = topic?.targets.find((one) => one.on === topic.target) ?? null
  return (
    <ChatPage
      props={props}
      field="topic"
      placeholder={target ? 'Message in Slack' : 'Message'}
      bubble={topicBubble}
      flat
      onDemand
      above={
        topic && (topic.waits.length > 0 || target) ? (
          <>
            {topic.waits.length > 0 && <TopicWaits topic={topic} />}
            {target && <TargetOverview id={target.on} chat={props} />}
          </>
        ) : null
      }
      outward={target ? <SlackWhere on={target.on} thread={target.threadTs !== undefined} /> : null}
      aside={topic ? (row) => <WaitButtons entity={row.entity} topic={topic} /> : undefined}
      corner={
        props.interactive ? (
          <>
            <PathPicker props={props} />
            <TopicGit props={props} />
            {props.onResolveTopic && (
              <Button
                label="Resolve"
                busy={props.working?.includes('resolve')}
                busyLabel="Resolving…"
                disabled={typeof props.entity.data.runningSince === 'number' && 'Claude is answering: wait for it to finish'}
                onClick={props.onResolveTopic}
              />
            )}
          </>
        ) : null
      }
    />
  )
}

/** The Slack mark (src/renderer/src/assets/slack.svg). */
export function SlackMark() {
  return <span className="slack-mark" role="img" aria-label="Slack" />
}

/** A conversation's name, or a thread's (its conversation's, then "thread"). */
function useWhere(on: string, thread: boolean): string {
  const { item } = useContext(ItemContext)
  const channel = /^slack:(?:conv|msg):([^:]+)/.exec(on)?.[1] ?? ''
  const title = item(`slack:conv:${channel}`)?.data.title
  const name = typeof title === 'string' && title ? title : channel
  return thread ? `${name} · thread` : name
}

/** Where a topic's box sends while it sends to Slack, on the box's right. */
function SlackWhere(props: { on: string; thread: boolean }) {
  return (
    <>
      <SlackMark />
      <span className="where-name">{useWhere(props.on, props.thread)}</span>
    </>
  )
}

/**
 * Beside a Slack message in a topic, while it is hovered: wait on its
 * thread, its conversation, or both. Beside a conversation, wait on it.
 */
function WaitButtons(props: { entity: Entity; topic: TopicProps }) {
  const { entity, topic } = props
  if (entity.type === 'slack.conversation') return <Button quiet label="Wait" onClick={() => topic.onWait(entity.id, 'channel')} />
  // Another topic: wait until it is resolved; a PR, until it is blocked by me, merged or closed.
  if (entity.type === 'topic' || entity.type === 'github.pr') return <Button quiet label="Wait" onClick={() => topic.onWaitFor(entity.id)} />
  // Not until it has loaded: only then is it known whether it is in a thread.
  if (entity.type !== 'slack.message' || typeof entity.data.ts !== 'string') return null
  return (
    <>
      <span className="muted">Wait on</span>
      <Button quiet label="Thread" onClick={() => topic.onWait(entity.id, 'thread')} />
      <Button quiet label="Channel" onClick={() => topic.onWait(entity.id, 'channel')} />
      <Button quiet label="Both" onClick={() => topic.onWait(entity.id, 'both')} />
    </>
  )
}

/**
 * Just above a topic's box: what it waits on, each with how long (or, for
 * a PR, what is mine to do on it), and a × to stop; under each, what has
 * come in since, to add to the topic or pass over.
 */
function TopicWaits(props: { topic: TopicProps }) {
  const { topic } = props
  const now = useContext(ClockContext)
  const { item } = useContext(ItemContext)
  return (
    <div className="topic-waits">
      {topic.waits.map(({ wait, candidates }) => {
        const on = waitOn(wait)
        // A topic's wait is back once that topic is resolved; a Slack one, when something new came in.
        // A PR's: what is mine to do on it (its blockers of my turn), merged or closed.
        const mine = wait.pr ? prBlockers(item(wait.pr)?.data) : []
        const status = wait.topic
          ? item(wait.topic)?.data.resolved
            ? 'resolved'
            : `waiting ${elapsed(now - wait.since)}`
          : wait.pr
            ? mine.length
              ? mine.join(', ')
              : `waiting ${elapsed(now - wait.since)}`
            : candidates.length
            ? `${candidates.length} new`
            : `waiting ${elapsed(now - wait.since)}`
        return (
        <div key={on}>
          <div className="topic-wait">
            {wait.slack && <SlackMark />}
            <span className="wait-on">
              <ItemPill id={on} fallback={wait.slack ? <WaitName on={on} /> : '…'} />
            </span>
            <span className="muted">{status}</span>
            <Button quiet label="×" title="Stop waiting" onClick={() => topic.onUnwait(on)} />
          </div>
          {candidates.map((message) => (
            <div key={message} className="topic-candidate">
              <span className="candidate-item">
                <ItemPill id={message} fallback="…" />
              </span>
              <Button quiet label="Add" onClick={() => topic.onAccept(message)} />
              <Button quiet label="Dismiss" onClick={() => topic.onDismiss(on, message)} />
            </div>
          ))}
        </div>
        )
      })}
    </div>
  )
}

/** What on a PR (presented: its `blockers`) is mine to do, or that it is merged or closed. */
function prBlockers(data: Record<string, unknown> | undefined): string[] {
  const blockers = Array.isArray(data?.blockers) ? (data.blockers as { id: string; label: string; turn: string }[]) : []
  return blockers.filter((blocker) => blocker.turn === 'me' || blocker.id === 'merged' || blocker.id === 'closed').map((blocker) => blocker.label)
}

/**
 * Just above a topic's box while it sends somewhere outside (Slack): that
 * place's overview (the thread's first message, or the conversation), to
 * see where it is going.
 */
function TargetOverview(props: { id: string; chat: OverviewProps }) {
  const { chat } = props
  const kinds = useContext(KindsContext)!
  const { item } = useContext(ItemContext)
  const entity = item(props.id)
  if (!entity) return null
  const Overview = kinds[entity.type].Overview
  // Its overview alone: no tree, no actions, nothing of the topic's own.
  const view = { ...chat.view, root: entity, rows: [], actions: [], older: false, compose: null, error: null, loading: false }
  return (
    <div className="target-overview">
      <Overview
        {...chat}
        view={view}
        shown={{ rows: [], keys: [], selectedPath: [], selectedIndex: -1 }}
        interactive={false}
        onAction={undefined}
        onOlder={undefined}
        onPause={undefined}
        entity={entity}
        findText={undefined}
      />
    </div>
  )
}

function WaitName(props: { on: string }) {
  return <>{useWhere(props.on, props.on.startsWith('slack:msg:'))}</>
}

export interface TopicListProps {
  topics: Entity[]
  /** Those put down until something comes into what they wait on. */
  waiting: string[]
  /** The topic open, if one is. */
  open: string | null
  /** The name being typed for a new one. */
  draft: string
  busy: boolean
  onDraft(text: string): void
  onOpen(id: string): void
}

/**
 * A topic's git buttons, for where it works now (its newest path, or else
 * where it last ran): **Check out** its worktree's commit in the repo (only
 * with a worktree), **Master** there, **Push** its branch (not while Claude
 * answers). What only git can tell, the core checks, and says why not.
 */
function TopicGit(props: { props: OverviewProps }) {
  const chat = props.props
  if (!chat.onTopicGit) return null
  const paths = chat.shown.rows.flatMap((row) => (row.kind === 'entity' && row.row.depth === 1 && row.row.entity.type === 'path' ? [row.row.entity.data] : []))
  const where = paths.at(-1) ?? chat.entity.data
  const has = (key: string) => typeof where[key] === 'string' && Boolean(where[key])
  const running = typeof chat.entity.data.runningSince === 'number'
  const noPath = !has('cwd') && 'No path: choose one first'
  const busy = (action: string) => chat.working?.includes(action)
  return (
    <>
      <Button label="Check out" busy={busy('checkout')} disabled={noPath || (!has('worktree') && 'No worktree to check out')} onClick={() => chat.onTopicGit?.('checkout')} />
      <Button label="Master" busy={busy('master')} disabled={noPath} onClick={() => chat.onTopicGit?.('master')} />
      <Button label="Push" busy={busy('push')} disabled={noPath || (running && 'Claude is answering: wait for it to finish')} onClick={() => chat.onTopicGit?.('push')} />
    </>
  )
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
          <TopicCard key={topic.id} entity={topic} open={topic.id === props.open} waiting={props.waiting.includes(topic.id)} onOpen={props.onOpen} />
        ))}
      </div>
    </nav>
  )
}

/** A topic in the list: a line, as a module is in the classic sidebar, the open one on the selection's fill. */
const TopicCard = memo(function TopicCard(props: { entity: Entity; open: boolean; waiting: boolean; onOpen(id: string): void }) {
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    if (props.open) ref.current?.scrollIntoView({ block: 'nearest' })
  }, [props.open])
  return (
    <div ref={ref} className={`topic${props.open ? ' open' : ''}`} onClick={() => props.onOpen(props.entity.id)}>
      {running(props.entity) !== null && <ClaudeMark />}
      <span className="topic-name">{nameOf(props.entity)}</span>
      {props.waiting && (
        <span className="topic-waiting" title="Waiting: `.` passes it by until something comes">
          ⧗
        </span>
      )}
      <span className="topic-time">
        <When entity={props.entity} />
      </span>
    </div>
  )
})
