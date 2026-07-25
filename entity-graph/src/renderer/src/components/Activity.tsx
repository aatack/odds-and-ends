import React, { useEffect, useState } from 'react'
import { X } from '@untitledui/icons'
import { relativeTime } from '../helpers/time'
import { useCalls } from '../state/hooks'
import { updateUi } from '../state/ui'
import type { CallOutcome, RecordedCall } from '../state/types'
import { clearCalls, editRecordedCall, isRunnable, rerunRecordedCall } from '../tools/call'
import { findTool } from '../tools/registry'
import { formatArg } from '../tools/args'
import { argsOf } from '../tools/types'
import { Badge, type BadgeColor } from './ui/Badge'
import { Button } from './ui/Button'
import { CodeBlock } from './ui/CodeBlock'
import { Popup } from './ui/Popup'

const STATUS: Record<CallOutcome['kind'], { label: string; color: BadgeColor }> = {
  success: { label: 'Done', color: 'success' },
  error: { label: 'Failed', color: 'error' },
  cancelled: { label: 'Cancelled', color: 'gray' },
}

/**
 * The call trail: every call that was abandoned with arguments half-entered, plus
 * every one that reached outside the app. Each can be run again as it stands, or
 * reopened with its arguments to be edited first.
 *
 * It opens as a popup rather than a sidebar: it is something you consult and
 * dismiss, like the palette, not a panel to work alongside — and a sidebar would
 * reflow the frames underneath it every time.
 */
export function Activity({ open }: { open: boolean }): React.JSX.Element | null {
  const calls = useCalls()
  const close = (): void => updateUi({ activityOpen: false })

  useEffect(() => {
    if (!open) return
    const onKey = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') close()
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [open])

  if (!open) return null

  return (
    <Popup onClose={close}>
      <div className="flex items-center gap-2 border-b border-gray-100 px-4 py-3">
        <h2 className="text-[13px] font-semibold text-gray-900">Activity</h2>
        <div className="flex-1" />
        {calls.length > 0 && (
          <button
            className="text-xs text-gray-400 hover:text-gray-700 focus:outline-none"
            onClick={clearCalls}
          >
            Clear
          </button>
        )}
        <button
          className="text-gray-400 hover:text-gray-700 focus:outline-none"
          onClick={close}
          aria-label="Close activity"
        >
          <X size={15} />
        </button>
      </div>

      {calls.length === 0 ? (
        <p className="px-4 py-6 text-[13px] text-gray-400">No calls yet.</p>
      ) : (
        // The same ceiling as the palette's tool list, so the two panels never
        // differ by more than their contents.
        <ul className="max-h-80 overflow-y-auto py-1">
          {calls.map((call) => (
            <CallRow key={call.callId} call={call} onClose={close} />
          ))}
        </ul>
      )}
    </Popup>
  )
}

/** What a call handed back, as JSON. Null when it handed back nothing. */
function resultText(call: RecordedCall): string | null {
  if (call.outcome.kind !== 'success' || call.outcome.data === undefined) return null
  try {
    return JSON.stringify(call.outcome.data, null, 2) ?? null
  } catch {
    return String(call.outcome.data)
  }
}

function CallRow({ call, onClose }: { call: RecordedCall; onClose: () => void }): React.JSX.Element {
  const tool = findTool(call.toolId)
  const status = STATUS[call.outcome.kind]
  const result = resultText(call)
  const [showResult, setShowResult] = useState(false)
  // A one-line précis of what it was called with, so the row is identifiable
  // without opening it.
  const summary = tool
    ? argsOf(tool)
        .map((a) => formatArg(call.args[a.name]))
        .filter(Boolean)
        .join(' · ')
    : ''

  return (
    <li className="px-4 py-2 hover:bg-gray-100/70">
      <div className="flex items-start gap-2">
        <div className="min-w-0 flex-1">
          <div className="truncate text-[13px] text-gray-800">{tool?.label ?? call.toolId}</div>
          {summary && <div className="truncate font-serif text-xs text-gray-500">{summary}</div>}
          <div className="mt-0.5 text-xs text-gray-400">
            {relativeTime(call.settledAt)}
            {call.outcome.kind === 'error' && (
              <span className="text-error-600"> · {call.outcome.message}</span>
            )}
            {call.fromCallId && <span> · replayed</span>}
          </div>
        </div>
        <div className="flex shrink-0 flex-col items-end gap-1">
          <Badge dot color={status.color}>
            {status.label}
          </Badge>
          <div className="flex gap-1">
            {/* The only place a call's result is visible. Tools that reach
                outside are the ones asked a question, and the log is where the
                answer stays. */}
            {result && (
              <Button size="sm" variant="tertiary" onClick={() => setShowResult((s) => !s)}>
                {showResult ? 'Hide' : 'Result'}
              </Button>
            )}
            {tool && (
              <>
                {/* Only offered when the call has everything it needs: a wizard
                    abandoned before its last argument has nothing to run. */}
                {isRunnable(call) && (
                  <Button size="sm" variant="tertiary" onClick={() => rerunRecordedCall(call.callId)}>
                    Run
                  </Button>
                )}
                <Button
                  size="sm"
                  variant="secondary"
                  onClick={() => {
                    onClose()
                    editRecordedCall(call.callId)
                  }}
                >
                  Edit
                </Button>
              </>
            )}
          </div>
        </div>
      </div>

      {/* Capped, and scrolling inside itself: a hundred pull requests must not
          push the rest of the log off the bottom of the panel. */}
      {result && showResult && (
        <div className="mt-1.5 max-h-64 overflow-y-auto">
          <CodeBlock code={result} language="json" />
        </div>
      )}
    </li>
  )
}

export const cancelledCount = (calls: RecordedCall[]): number =>
  calls.filter((c) => c.outcome.kind === 'cancelled').length
