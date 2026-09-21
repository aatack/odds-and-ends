import { useState } from 'react'
import type { ReactNode } from 'react'
import { accentFor, ago, metadataPairs, typeNames, until } from '../format'
import type { Blocker, TopicDetail, TopicId } from '../types'
import { Dot, Key, Separator, Status } from './primitives'

interface HeaderProps {
  detail: TopicDetail
  details: boolean
  onFocusTopic(id: TopicId): void
  onCancelBlocker(id: string): void
  onSetMetadata(key: string, value: string): void
  onRunTool(id: string): void
}

function blockerText(blocker: Blocker): string {
  const value = blocker.config.until
  if (typeof value === 'string') return `${blocker.label} · ${until(value)} left`
  return blocker.label
}

export function TopicHeader({
  detail,
  details,
  onFocusTopic,
  onCancelBlocker,
  onSetMetadata,
  onRunTool,
}: HeaderProps): ReactNode {
  const topic = detail.topic

  return (
    <header className="shrink-0 border-b border-line bg-panel">
      <div className="drag flex h-10 items-center gap-2 px-4">
        <div className="no-drag flex min-w-0 flex-1 items-center gap-1.5 text-[11.5px] text-muted">
          {detail.parents.slice(0, 3).map((parent) => (
            <button
              key={parent.id}
              onClick={() => onFocusTopic(parent.id)}
              className="max-w-[220px] truncate rounded px-1 py-0.5 hover:bg-raised hover:text-ink"
            >
              {parent.text || '(untitled)'}
            </button>
          ))}
          {detail.parents.length > 0 && <span className="text-faint">/</span>}
          <span className="text-faint">{typeNames[topic.type] ?? topic.type}</span>
        </div>
        <div className="no-drag flex shrink-0 items-center gap-1.5">
          <button
            onClick={() => onRunTool('topic.details')}
            className="rounded px-1.5 py-0.5 text-[11px] text-faint hover:bg-raised hover:text-muted"
          >
            details <Key>i</Key>
          </button>
        </div>
      </div>

      <div className="mx-auto w-full max-w-[760px] px-6 pb-4">
        <div className="flex items-start gap-3">
          <Dot color={accentFor(topic.type)} />
          <button
            onClick={() => onRunTool('topic.rename')}
            title="rename"
            className="-mt-1 min-w-0 flex-1 text-left text-[19px] leading-snug font-semibold tracking-[-0.01em] hover:text-accent"
          >
            {topic.text || '(untitled)'}
          </button>
        </div>

        <div className="mt-1.5 flex flex-wrap items-center gap-x-2.5 gap-y-1 pl-[18px]">
          <Status color={topic.open ? 'var(--accent)' : 'var(--line-strong)'}>{topic.open ? 'open' : 'put down'}</Status>
          {topic.resolved && (
            <>
              <Separator />
              <Status color="var(--ok)">signed off</Status>
            </>
          )}
          <Separator />
          <Status title={new Date(topic.updatedAt).toLocaleString()}>{ago(topic.updatedAt)}</Status>
          {detail.blockers.map((blocker) => (
            <span key={blocker.id} className="inline-flex items-center gap-2.5">
              <Separator />
              <button
                onClick={() => onCancelBlocker(blocker.id)}
                title={blocker.lastError ?? 'drop this one'}
                className="group/blocker inline-flex items-center gap-1.5 text-[11.5px] text-muted hover:text-bad"
              >
                {blocker.lastError && <Dot color="var(--bad)" />}
                {blockerText(blocker)}
                <span className="text-faint opacity-0 group-hover/blocker:opacity-100">×</span>
              </button>
            </span>
          ))}
        </div>

        {details && <Details detail={detail} onSetMetadata={onSetMetadata} />}
      </div>
    </header>
  )
}

function Details({
  detail,
  onSetMetadata,
}: {
  detail: TopicDetail
  onSetMetadata(key: string, value: string): void
}): ReactNode {
  const [editing, setEditing] = useState<string | null>(null)
  const [draft, setDraft] = useState('')
  const [newKey, setNewKey] = useState('')
  const pairs = metadataPairs(detail.topic)

  const commit = (key: string): void => {
    onSetMetadata(key, draft)
    setEditing(null)
  }

  return (
    <div className="mt-3 ml-[18px] rounded-lg bg-raised px-3 py-2.5">
      <div className="grid grid-cols-[120px_1fr] gap-x-3 gap-y-1 text-[11.5px]">
        <div className="text-faint">id</div>
        <div className="truncate font-mono text-[11px] text-muted">{detail.topic.id}</div>
        {pairs.map(([key, value]) => (
          <div key={key} className="contents">
            <div className="truncate text-faint">{key}</div>
            {editing === key ? (
              <input
                autoFocus
                value={draft}
                onChange={(event) => setDraft(event.target.value)}
                onBlur={() => commit(key)}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') commit(key)
                  if (event.key === 'Escape') setEditing(null)
                  event.stopPropagation()
                }}
                className="w-full rounded border border-accent/50 px-1"
              />
            ) : (
              <button
                onClick={() => {
                  setEditing(key)
                  setDraft(value)
                }}
                className="truncate text-left hover:text-accent"
                title={value}
              >
                {value}
              </button>
            )}
          </div>
        ))}
        <input
          value={newKey}
          placeholder="add a field"
          onChange={(event) => setNewKey(event.target.value)}
          onKeyDown={(event) => {
            if (event.key === 'Enter' && newKey.trim().length > 0) {
              onSetMetadata(newKey.trim(), '')
              setEditing(newKey.trim())
              setDraft('')
              setNewKey('')
            }
            event.stopPropagation()
          }}
          className="text-faint placeholder:text-faint"
        />
        <div />
      </div>
    </div>
  )
}
