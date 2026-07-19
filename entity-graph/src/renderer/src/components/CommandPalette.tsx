import React, { useEffect, useMemo, useRef, useState } from 'react'
import fuzzysort from 'fuzzysort'
import { cn } from '../helpers/cn'

export interface Command {
  id: string
  label: string
  /** Faint text on the right (e.g. a section or shortcut). */
  hint?: string
  run: () => void
}

// A Ctrl/⌘+K launcher: filter the given commands and run one. Closes on Escape,
// an outside click, or after running a command. Deliberately simple — commands
// take no arguments, unlike the orchestrator's argument wizard.
export function CommandPalette({
  open,
  commands,
  onClose,
}: {
  open: boolean
  commands: Command[]
  onClose: () => void
}): React.JSX.Element | null {
  const [query, setQuery] = useState('')
  const [activeIndex, setActiveIndex] = useState(0)
  const activeRef = useRef<HTMLButtonElement>(null)

  useEffect(() => {
    if (!open) {
      setQuery('')
      setActiveIndex(0)
    }
  }, [open])

  // With no query, keep the commands in their given order; otherwise fuzzy-match
  // against the label and sort by relevance.
  const matches = useMemo(() => {
    const q = query.trim()
    if (!q) return commands
    return fuzzysort.go(q, commands, { key: 'label' }).map((r) => r.obj)
  }, [query, commands])
  const active = matches.length ? Math.min(activeIndex, matches.length - 1) : 0

  useEffect(() => {
    activeRef.current?.scrollIntoView({ block: 'nearest' })
  }, [active])

  if (!open) return null

  const runAt = (index: number): void => {
    const command = matches[index]
    if (!command) return
    onClose()
    command.run()
  }

  const onKeyDown = (e: React.KeyboardEvent): void => {
    if (e.key === 'Escape') {
      e.preventDefault()
      onClose()
    } else if (e.key === 'Enter') {
      e.preventDefault()
      runAt(active)
    } else if (e.key === 'ArrowDown' && matches.length) {
      e.preventDefault()
      setActiveIndex((i) => (Math.min(i, matches.length - 1) + 1) % matches.length)
    } else if (e.key === 'ArrowUp' && matches.length) {
      e.preventDefault()
      setActiveIndex((i) => (Math.min(i, matches.length - 1) + matches.length - 1) % matches.length)
    }
  }

  return (
    <div
      className="fixed inset-0 z-50 flex items-start justify-center bg-gray-950/10 pt-32 backdrop-blur-xs"
      onClick={onClose}
    >
      <div
        className="w-full max-w-lg overflow-hidden rounded-xl bg-white shadow-lg"
        onClick={(e) => e.stopPropagation()}
      >
        <div className="border-b border-gray-100">
          <input
            autoFocus
            value={query}
            onChange={(e) => {
              setQuery(e.target.value)
              setActiveIndex(0)
            }}
            onKeyDown={onKeyDown}
            placeholder="Run a command…"
            className="w-full bg-transparent px-4 py-3.5 text-[13px] outline-none placeholder:text-gray-400"
          />
        </div>

        <ul className="max-h-80 overflow-y-auto py-1">
          {matches.length === 0 ? (
            <li className="px-4 py-3 text-[13px] text-gray-400">No matching commands.</li>
          ) : (
            matches.map((command, i) => (
              <li key={command.id}>
                <button
                  ref={i === active ? activeRef : undefined}
                  onClick={() => runAt(i)}
                  onMouseMove={() => setActiveIndex(i)}
                  className={cn(
                    'flex w-full items-center justify-between px-4 py-2.5 text-left text-[13px] focus:outline-none',
                    i === active && 'bg-gray-100/70',
                  )}
                >
                  <span className="font-medium text-gray-800">{command.label}</span>
                  {command.hint && <span className="text-xs text-gray-400">{command.hint}</span>}
                </button>
              </li>
            ))
          )}
        </ul>
      </div>
    </div>
  )
}
