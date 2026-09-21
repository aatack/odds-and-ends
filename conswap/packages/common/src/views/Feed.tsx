import { memo, useEffect, useRef } from 'react'
import type { ReactNode } from 'react'
import { accentFor, ago, attribution, clock } from '../format'
import type { FeedRow } from '../state'
import type { TopicDetail, TopicId } from '../types'
import { Empty, Key } from './primitives'

interface FeedProps {
  detail: TopicDetail | null
  rows: FeedRow[]
  hidden: number
  cursorKey: string | null
  onSelectRow(path: TopicId[]): void
  onToggleRow(id: TopicId): void
  onFocusTopic(id: TopicId): void
  onPromote(id: TopicId): void
  onShowEarlier(): void
}

export function Feed({
  detail,
  rows,
  hidden,
  cursorKey,
  onSelectRow,
  onToggleRow,
  onFocusTopic,
  onPromote,
  onShowEarlier,
}: FeedProps): ReactNode {
  const scroller = useRef<HTMLDivElement>(null)
  const bottom = useRef<HTMLDivElement>(null)

  // A topic opens at the bottom, where the newest thing is.
  useEffect(() => {
    bottom.current?.scrollIntoView({ block: 'end' })
  }, [detail?.topic.id])

  useEffect(() => {
    if (!cursorKey) return
    scroller.current?.querySelector(`[data-row="${CSS.escape(cursorKey)}"]`)?.scrollIntoView({ block: 'nearest' })
  }, [cursorKey])

  if (!detail) return null

  return (
    <div ref={scroller} className="scroller min-h-0 flex-1 overflow-y-auto">
      <div className="mx-auto w-full max-w-[760px] px-6 py-4">
        {hidden > 0 && (
          <button
            onClick={onShowEarlier}
            className="mb-3 w-full rounded-md border border-line py-1.5 text-[11.5px] text-muted hover:bg-raised"
          >
            {hidden} earlier {hidden === 1 ? 'entry' : 'entries'}
          </button>
        )}

        {rows.length === 0 ? (
          <Empty title="Nothing here yet">
            Press <Key>&#8629;</Key> to write the first note, <Key>c</Key> to put Claude on it, or <Key>s</Key> to say
            something in Slack.
          </Empty>
        ) : (
          rows.map((row) => (
            <Row
              key={row.key}
              row={row}
              selected={row.key === cursorKey}
              onSelectRow={onSelectRow}
              onToggleRow={onToggleRow}
              onFocusTopic={onFocusTopic}
              onPromote={onPromote}
            />
          ))
        )}
        <div ref={bottom} />
      </div>
    </div>
  )
}

interface RowProps {
  row: FeedRow
  selected: boolean
  onSelectRow(path: TopicId[]): void
  onToggleRow(id: TopicId): void
  onFocusTopic(id: TopicId): void
  onPromote(id: TopicId): void
}

const Row = memo(function Row({
  row,
  selected,
  onSelectRow,
  onToggleRow,
  onFocusTopic,
  onPromote,
}: RowProps): ReactNode {
  const topic = row.node.topic
  const pending = topic.metadata.pending === true
  const permalink = typeof topic.metadata.permalink === 'string' ? topic.metadata.permalink : null
  const url = typeof topic.metadata.url === 'string' ? topic.metadata.url : null
  const link = permalink ?? url

  return (
    <div
      data-row={row.key}
      onClick={() => onSelectRow(row.path)}
      style={{ marginLeft: row.depth * 16 }}
      className={`group relative mb-px flex gap-3 rounded-md py-1.5 pr-2 pl-[18px] ${
        selected ? 'bg-accent-soft' : 'hover:bg-panel'
      } ${pending ? 'opacity-55' : ''}`}
    >
      <div className="absolute top-2 bottom-2 left-0 w-[2px] rounded-full" style={{ background: accentFor(topic.type) }} />

      <div className="min-w-0 flex-1">
        <div className="flex items-baseline gap-2">
          <span className="truncate text-[11.5px] font-medium text-muted">{attribution(topic)}</span>
          <span className="shrink-0 text-[10.5px] text-faint" title={new Date(topic.createdAt).toLocaleString()}>
            {clock(topic.createdAt)}
          </span>
          {topic.resolved && <span className="text-[10.5px] text-ok">signed off</span>}
          {!topic.open && topic.type === 'topic' && !topic.resolved && (
            <span className="text-[10.5px] text-faint">put down</span>
          )}
          <span className="ml-auto flex shrink-0 items-center gap-1 opacity-0 group-hover:opacity-100">
            {link && (
              <a
                href={link}
                target="_blank"
                rel="noreferrer"
                onClick={(event) => event.stopPropagation()}
                className="rounded px-1 text-[10.5px] text-faint hover:text-accent"
              >
                open
              </a>
            )}
            {row.node.childCount > 0 && (
              <button
                onClick={(event) => {
                  event.stopPropagation()
                  onFocusTopic(topic.id)
                }}
                className="rounded px-1 text-[10.5px] text-faint hover:text-accent"
              >
                focus
              </button>
            )}
            <button
              onClick={(event) => {
                event.stopPropagation()
                onPromote(topic.id)
              }}
              className="rounded px-1 text-[10.5px] text-faint hover:text-accent"
              title="open this as work of its own and wait for it"
            >
              take on
            </button>
          </span>
        </div>

        <div className="text-[13px] leading-[1.55] break-words whitespace-pre-wrap">{topic.text}</div>

        {row.expandable && (
          <button
            onClick={(event) => {
              event.stopPropagation()
              onToggleRow(topic.id)
            }}
            className="mt-0.5 inline-flex items-center gap-1 rounded text-[11px] text-faint hover:text-accent"
          >
            <span className="inline-block w-2">{row.expanded ? '▾' : '▸'}</span>
            {row.node.childCount} {row.node.childCount === 1 ? 'entry' : 'entries'}
            <span className="text-faint">· {ago(topic.updatedAt)}</span>
          </button>
        )}
      </div>
    </div>
  )
})
