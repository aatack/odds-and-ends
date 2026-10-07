import { memo, useMemo } from 'react'
import ReactMarkdown from 'react-markdown'
import type { Components } from 'react-markdown'
import remarkBreaks from 'remark-breaks'
import remarkGfm from 'remark-gfm'
import { mentionScheme } from '../../../core/types.ts'
import type { Entity } from '../../../core/types.ts'
import { authorColour, fullTime, shortTime } from '../format.ts'
import type { RowProps } from './kindTypes.ts'
import { Link, usePeek } from './primitives.tsx'

/** Chat-style messages, shared by every module that has a discussion. */

/** A speaker's run starts where the speaker changes, or five minutes pass. */
export function startsRun(above: Entity | null, current: Entity): boolean {
  if (!above || above.type !== current.type) return true
  return above.data.author !== current.data.author || Number(current.data.ts) - Number(above.data.ts) > 300
}

/** A message as a row: grouped with the one above it, like Slack, unless told to say who and when. */
export const MessageRow = memo(function MessageRow(props: RowProps & { always?: boolean }) {
  return (
    <div className="message">
      <MessageBody
        entity={props.entity}
        author={props.always || startsRun(props.above, props.entity)}
        onOpen={props.onOpen}
        onImage={props.onImage}
      />
    </div>
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
export function MessageBody(props: {
  entity: Entity
  author: boolean
  replies?: boolean
  onOpen(id: string): void
  onImage(ref: string | null): void
}) {
  const data = props.entity.data
  const replies = props.replies === false ? 0 : Number(data.replyCount ?? 0)
  const reactions = (data.reactions as Reaction[] | undefined) ?? []
  const images = (data.images as Image[] | undefined) ?? []
  return (
    <div className={`body${props.author ? ' head' : ''}`}>
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
          {props.author && data.verdict ? (
            <span className={`verdict ${String(data.state ?? '').toLowerCase()}`}>{String(data.verdict)} </span>
          ) : null}
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
        {replies > 0 && <Replies id={props.entity.id} count={replies} latest={data.latestReply as string | undefined} />}
      </div>
      {/* On the right, and only while the row is hovered, so every message starts at the left edge. */}
      <span className="stamp" title={fullTime(String(data.ts))}>
        {shortTime(String(data.ts))}
      </span>
    </div>
  )
}

/**
 * A person or channel named in Slack: an author or a mention. Hovering shows
 * a background and a click pushes their conversation, when the app has one.
 */
export function Person(props: {
  name: string
  target: string | null
  colour?: string
  strong?: boolean
  onOpen(id: string): void
}) {
  const { target, onOpen } = props
  const peek = usePeek(target ? { kind: 'entity', id: target } : null)
  return (
    <span
      {...peek}
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

export const Markdown = memo(function Markdown(props: { text: string; onOpen(id: string): void }) {
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
      // Remote images aren't loaded; they peek like any link.
      img: ({ src, alt }) => {
        const href = typeof src === 'string' ? src : ''
        return href ? <Link href={href}>{alt || 'image'}</Link> : null
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

/** The thread under a message; hovering peeks at it. */
function Replies(props: { id: string; count: number; latest: string | undefined }) {
  const peek = usePeek({ kind: 'entity', id: props.id })
  return (
    <div className="replies">
      <span {...peek}>
        {props.count} {props.count === 1 ? 'reply' : 'replies'} · {shortTime(props.latest)}
      </span>
    </div>
  )
}
