import { useEffect, useRef } from 'react'
import type { ReactNode } from 'react'
import { slackTarget } from '../format'
import type { Composer as ComposerState, ComposerMode } from '../state'
import type { TopicDetail } from '../types'
import { Key } from './primitives'

interface ComposerProps {
  composer: ComposerState | null
  detail: TopicDetail | null
  onComposerText(text: string): void
  onComposerMode(mode: ComposerMode): void
  onSubmit(): void
  onCancel(): void
}

const modes: { mode: ComposerMode; label: string; key: string }[] = [
  { mode: 'note', label: 'note', key: '⌃d' },
  { mode: 'slack', label: 'slack', key: '⌃s' },
  { mode: 'claude', label: 'claude', key: '⌃p' },
  { mode: 'subtopic', label: 'subtopic', key: '⌃t' },
]

function hintFor(mode: ComposerMode, detail: TopicDetail | null): string {
  if (mode === 'slack') {
    const target = slackTarget(detail)
    return target ? `posts in ${target}` : 'this topic is not attached to a Slack conversation'
  }
  if (mode === 'claude') return 'runs in this topic’s own worktree, on opus'
  if (mode === 'subtopic') return 'makes a subtopic and watches it'
  if (mode === 'topic') return 'starts something new, unattached'
  if (mode === 'rename') return 'renames this topic'
  return 'kept on the topic as a note'
}

export function Composer({
  composer,
  detail,
  onComposerText,
  onComposerMode,
  onSubmit,
  onCancel,
}: ComposerProps): ReactNode {
  const input = useRef<HTMLTextAreaElement>(null)

  useEffect(() => {
    if (composer) input.current?.focus()
  }, [composer !== null])

  useEffect(() => {
    const element = input.current
    if (!element) return
    element.style.height = '0px'
    element.style.height = `${Math.min(element.scrollHeight, 240)}px`
  }, [composer?.text, composer?.mode])

  if (!composer) {
    return (
      <div className="shrink-0 border-t border-line bg-panel px-6 py-2">
        <div className="mx-auto flex w-full max-w-[760px] items-center gap-2 text-[11.5px] text-faint">
          <Key>&#8629;</Key> note
          <Key>s</Key> slack
          <Key>c</Key> claude
          <Key>b</Key> put down until
          <Key>r</Key> sign off
          <span className="ml-auto flex items-center gap-1.5">
            <Key>.</Key> next open topic
          </span>
        </div>
      </div>
    )
  }

  const showModes = composer.mode !== 'rename' && composer.mode !== 'topic'

  return (
    <div className="shrink-0 border-t border-line bg-panel px-6 pt-2 pb-3">
      <div className="mx-auto w-full max-w-[760px]">
        <div className="rounded-lg border border-line bg-raised px-3 py-2">
          <textarea
            ref={input}
            rows={1}
            value={composer.text}
            placeholder={
              composer.mode === 'rename' ? 'a better name' : composer.mode === 'topic' ? 'what needs doing?' : 'say something'
            }
            onChange={(event) => onComposerText(event.target.value)}
            onKeyDown={(event) => {
              if (event.key === 'Enter' && event.shiftKey) return
              if (event.key === 'Enter') {
                event.preventDefault()
                onSubmit()
              }
            }}
            className="w-full bg-transparent text-[13.5px] placeholder:text-faint"
          />
        </div>

        <div className="mt-1.5 flex items-center gap-2 text-[11px] text-faint">
          {showModes &&
            modes.map((entry) => (
              <button
                key={entry.mode}
                onClick={() => onComposerMode(entry.mode)}
                className={`inline-flex items-center gap-1 rounded px-1.5 py-0.5 ${
                  composer.mode === entry.mode ? 'bg-accent-soft text-accent' : 'hover:bg-raised hover:text-muted'
                }`}
              >
                {entry.label}
                <span className="text-faint">{entry.key}</span>
              </button>
            ))}
          <span className="ml-auto flex items-center gap-2">
            <span>{hintFor(composer.mode, detail)}</span>
            <button onClick={onCancel} className="hover:text-muted">
              <Key>esc</Key>
            </button>
          </span>
        </div>
      </div>
    </div>
  )
}
