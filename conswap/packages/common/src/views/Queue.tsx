import type { ReactNode } from 'react'
import { accentFor, ago, until } from '../format'
import type { QueueView, ServerStatus, TopicId } from '../types'
import { Dot } from './primitives'

interface QueueProps {
  queue: QueueView | null
  status: ServerStatus | null
  focus: TopicId | null
  onFocusTopic(id: TopicId): void
}

function blockerSummary(label: string, config: Record<string, unknown>): string {
  const value = config.until
  if (typeof value === 'string') return `${label.split(' for ')[0] ?? label} · ${until(value)}`
  return label
}

export function Queue({ queue, status, focus, onFocusTopic }: QueueProps): ReactNode {
  const open = queue?.open ?? []
  const waiting = queue?.waiting ?? []

  return (
    <aside className="flex w-[268px] shrink-0 flex-col bg-panel">
      <div className="drag flex h-10 shrink-0 items-center px-4 text-[12px] font-semibold tracking-tight">
        conswap
        <span className="ml-auto text-[11px] font-normal text-faint">{open.length} waiting on you</span>
      </div>

      <div className="scroller flex-1 overflow-y-auto px-2 pb-4">
        <Section title="Open" count={open.length} />
        {open.length === 0 ? (
          <div className="px-2 py-1 text-[12px] text-faint">nothing needs you</div>
        ) : (
          open.map((entry) => (
            <button
              key={entry.topic.id}
              onClick={() => onFocusTopic(entry.topic.id)}
              className={`block w-full rounded-md px-2 py-[7px] text-left ${
                focus === entry.topic.id ? 'bg-accent-soft' : 'hover:bg-raised'
              }`}
            >
              <div className="flex items-center gap-2">
                <Dot color={accentFor(entry.topic.type)} />
                <span className="min-w-0 flex-1 truncate text-[12.5px] font-medium">
                  {entry.topic.text || '(untitled)'}
                </span>
                <span className="shrink-0 text-[10.5px] text-faint">{ago(entry.topic.updatedAt)}</span>
              </div>
              <div className="truncate pl-[14px] text-[11.5px] text-muted">{entry.reason}</div>
            </button>
          ))
        )}

        {waiting.length > 0 && (
          <>
            <Section title="Waiting" count={waiting.length} />
            {waiting.map((entry) => (
              <button
                key={entry.topic.id}
                onClick={() => onFocusTopic(entry.topic.id)}
                className={`block w-full rounded-md px-2 py-[7px] text-left ${
                  focus === entry.topic.id ? 'bg-accent-soft' : 'hover:bg-raised'
                }`}
              >
                <div className="flex items-center gap-2">
                  <Dot color="var(--line-strong)" />
                  <span className="min-w-0 flex-1 truncate text-[12.5px] text-muted">
                    {entry.topic.text || '(untitled)'}
                  </span>
                </div>
                <div className="truncate pl-[14px] text-[11.5px] text-faint">
                  {entry.blockers.map((blocker) => blockerSummary(blocker.label, blocker.config)).join(' · ') ||
                    'put down'}
                </div>
              </button>
            ))}
          </>
        )}
      </div>

      <div className="flex shrink-0 flex-wrap items-center gap-x-3 gap-y-1 px-3 py-2.5 text-[10.5px] text-faint">
        {(status?.integrations ?? []).map((integration) => (
          <span key={integration.name} className="inline-flex items-center gap-1.5" title={integration.detail}>
            <Dot
              color={
                !integration.enabled
                  ? 'var(--line-strong)'
                  : integration.state === 'error'
                    ? 'var(--bad)'
                    : integration.state === 'polling'
                      ? 'var(--warn)'
                      : 'var(--ok)'
              }
            />
            {integration.name}
          </span>
        ))}
      </div>
    </aside>
  )
}

function Section({ title, count }: { title: string; count: number }): ReactNode {
  return (
    <div className="flex items-center gap-2 px-2 pt-4 pb-1 text-[10.5px] font-semibold tracking-wide text-faint uppercase">
      {title}
      <span className="font-normal">{count}</span>
    </div>
  )
}
