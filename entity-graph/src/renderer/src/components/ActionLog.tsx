import React, { useEffect } from 'react'
import { X } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { relativeTime } from '../helpers/time'
import {
  clearActionLog,
  useActionLog,
  type ActionLogEntry,
  type ActionStatus,
} from '../helpers/actionLog'
import { openCommandPalette } from './CommandPalette'
import { Badge, type BadgeColor } from './ui/Badge'
import { Button } from './ui/Button'

const STATUS: Record<ActionStatus, { label: string; color: BadgeColor }> = {
  success: { label: 'Done', color: 'success' },
  error: { label: 'Failed', color: 'error' },
  cancelled: { label: 'Cancelled', color: 'gray' },
}

// The activity trail: field-bearing commands run from the palette, newest first.
// Cancelled ones reopen their wizard, prefilled from where they were abandoned.
// A right-side drawer toggled from the header, dismissed on Escape / outside click.
export function ActionLog({ open, onClose }: { open: boolean; onClose: () => void }): React.JSX.Element | null {
  const log = useActionLog()

  useEffect(() => {
    if (!open) return
    const onKey = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') onClose()
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [open, onClose])

  if (!open) return null

  const resume = (entry: ActionLogEntry): void => {
    onClose()
    openCommandPalette({
      resume: { key: entry.key, commandId: entry.commandId, values: entry.values },
    })
  }

  return (
    <div className="fixed inset-0 z-40" onClick={onClose}>
      <aside
        className="absolute right-0 top-0 flex h-full w-80 flex-col bg-white shadow-lg"
        onClick={(e) => e.stopPropagation()}
      >
        <div className="flex items-center gap-2 border-b border-gray-100 px-4 py-3">
          <h2 className="text-[13px] font-semibold text-gray-900">Activity</h2>
          <div className="flex-1" />
          {log.length > 0 && (
            <button
              className="text-xs text-gray-400 hover:text-gray-700 focus:outline-none"
              onClick={clearActionLog}
            >
              Clear
            </button>
          )}
          <button
            className="text-gray-400 hover:text-gray-700 focus:outline-none"
            onClick={onClose}
            aria-label="Close activity"
          >
            <X size={15} />
          </button>
        </div>

        <div className="min-h-0 flex-1 overflow-y-auto">
          {log.length === 0 ? (
            <p className="px-4 py-6 text-[13px] text-gray-400">No actions yet.</p>
          ) : (
            <ul className="py-1">
              {log.map((entry) => {
                const cancelled = entry.status === 'cancelled'
                return (
                  <li
                    key={entry.key}
                    onClick={cancelled ? () => resume(entry) : undefined}
                    title={cancelled ? 'Click to resume' : undefined}
                    className={cn('flex items-start gap-2 px-4 py-2', cancelled && 'hover:bg-gray-100/70')}
                  >
                    <div className="min-w-0 flex-1">
                      <div className="truncate text-[13px] text-gray-800">{entry.title}</div>
                      <div className="mt-0.5 text-xs text-gray-400">
                        {relativeTime(entry.at)}
                        {entry.error && <span className="text-error-600"> · {entry.error}</span>}
                      </div>
                    </div>
                    <Badge dot color={STATUS[entry.status].color}>
                      {STATUS[entry.status].label}
                    </Badge>
                    {cancelled && (
                      <Button
                        size="sm"
                        variant="secondary"
                        onClick={(e) => {
                          e.stopPropagation()
                          resume(entry)
                        }}
                      >
                        Resume
                      </Button>
                    )}
                  </li>
                )
              })}
            </ul>
          )}
        </div>
      </aside>
    </div>
  )
}
