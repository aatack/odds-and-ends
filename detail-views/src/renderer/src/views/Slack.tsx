import { memo, useMemo } from 'react'
import ReactMarkdown from 'react-markdown'
import type { Components } from 'react-markdown'
import remarkBreaks from 'remark-breaks'
import remarkGfm from 'remark-gfm'
import { mentionScheme } from '../../../core/types.ts'
import type { Entity } from '../../../core/types.ts'
import { authorColour, fullTime, shortTime } from '../format.ts'
import { Composer, Link, Row, Status } from './primitives.tsx'
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
/** A time is shown unless the message above already shows the same one. */
function showsTime(messages: Entity[], index: number): boolean {
  return index === 0 || shortTime(String(messages[index].data.ts)) !== shortTime(String(messages[index - 1].data.ts))
}

function showsAuthor(messages: Entity[], index: number): boolean {
  if (index === 0) return true
  const previous = messages[index - 1].data
  const current = messages[index].data
  return previous.author !== current.author || Number(current.ts) - Number(previous.ts) > 300
}

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

const MessageRow = memo(function MessageRow(props: {
  entity: Entity
  author: boolean
  time: boolean
  selected: boolean
  onSelect(id: string): void
  onOpen(id: string): void
  onImage(ref: string): void
}) {
  return (
    <Row
      id={props.entity.id}
      selected={props.selected}
      className={`message${props.entity.data.quiet ? ' quiet' : ''}`}
      onSelect={props.onSelect}
     
    >
      <MessageBody entity={props.entity} author={props.author} time={props.time} onOpen={props.onOpen} onImage={props.onImage} />
    </Row>
  )
})

interface Image {
  name: string
  thumb: string
  full: string
  width?: number
  height?: number
}

/** Fits a thumbnail in 360×240, sized up front so the list doesn't jump. */
function thumbSize(image: Image): { width?: number; height?: number } {
  if (!image.width || !image.height) return {}
  const scale = Math.min(1, 360 / image.width, 240 / image.height)
  return { width: Math.round(image.width * scale), height: Math.round(image.height * scale) }
}

interface Reaction {
  emoji: string
  count: number
  mine: boolean
}

/**
 * One line of chat: time in the gutter and name in colour where a speaker's
 * run starts, then the text, reactions and thread.
 */
function MessageBody(props: {
  entity: Entity
  author: boolean
  time: boolean
  replies?: boolean
  onOpen(id: string): void
  onImage(ref: string): void
}) {
  const data = props.entity.data
  const replies = props.replies === false ? 0 : Number(data.replyCount ?? 0)
  const reactions = (data.reactions as Reaction[] | undefined) ?? []
  const images = (data.images as Image[] | undefined) ?? []
  return (
    <div className={`body${props.author ? ' head' : ''}`}>
      <span className="gutter">
        <span className={`stamp${props.time ? '' : ' repeat'}`} data-full={fullTime(String(data.ts))}>
          {shortTime(String(data.ts))}
        </span>
      </span>
      <div className="content">
        <div className="text">
          {props.author && (
            <Person
              name={String(data.author)}
              target={(data.authorTarget as string | null | undefined) ?? null}
              colour={authorColour(String(data.authorKey))}
              strong
              onOpen={props.onOpen}
            />
          )}
          <Markdown text={String(data.markdown ?? data.text)} onOpen={props.onOpen} />
          {reactions.length > 0 && (
            <span className="reactions">
              {reactions.map((reaction) => (
                <span key={reaction.emoji} className={`reaction${reaction.mine ? ' mine' : ''}`}>
                  {reaction.emoji} <span className="reaction-count">{reaction.count}</span>
                </span>
              ))}
            </span>
          )}
        </div>
        {images.length > 0 && (
          <div className="images">
            {images.map((image) => (
              <img
                key={image.thumb}
                className="thumb"
                src={`slack-image://${image.thumb}`}
                alt={image.name}
                {...thumbSize(image)}
                onClick={() => props.onImage(image.full)}
              />
            ))}
          </div>
        )}
        {replies > 0 && (
          <div className="replies">
            {replies} {replies === 1 ? 'reply' : 'replies'} · {shortTime(data.latestReply as string | undefined)}
          </div>
        )}
      </div>
    </div>
  )
}

/**
 * A person or channel named in Slack: an author or a mention. Hovering shows
 * a background and a click pushes their conversation, when the app has one.
 */
function Person(props: {
  name: string
  target: string | null
  colour?: string
  strong?: boolean
  onOpen(id: string): void
}) {
  const { target, onOpen } = props
  return (
    <span
      className={`person${target ? ' live' : ''}${props.strong ? ' strong' : ''}`}
      style={props.colour ? { color: props.colour } : undefined}
      onMouseDown={(event) => {
        if (!target) return
        event.stopPropagation()
        onOpen(target)
      }}
    >
      {props.name}
    </span>
  )
}

const plugins = [remarkGfm, remarkBreaks]

/** Mentions keep their scheme; other links only if they are web or mail. */
function keepUrl(url: string): string {
  return /^(mention:|https?:|mailto:)/.test(url) ? url : ''
}

const Markdown = memo(function Markdown(props: { text: string; onOpen(id: string): void }) {
  const { onOpen } = props
  const components = useMemo<Components>(
    () => ({
      a: ({ href, children }) => {
        if (href?.startsWith(mentionScheme)) {
          const [key, ...rest] = href.slice(mentionScheme.length).split('/')
          const target = rest.join('/') || null
          const label = String(Array.isArray(children) ? children.join('') : children)
          const channel = label.startsWith('#')
          return (
            <Person
              name={channel ? label : `@${label}`}
              target={target}
              colour={channel ? undefined : authorColour(key)}
              onOpen={onOpen}
            />
          )
        }
        return href ? <Link href={href}>{children}</Link> : <>{children}</>
      },
    }),
    [onOpen],
  )
  return (
    <ReactMarkdown remarkPlugins={plugins} urlTransform={keepUrl} components={components}>
      {props.text}
    </ReactMarkdown>
  )
})
