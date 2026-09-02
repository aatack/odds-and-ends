/**
 * The search box that puts a node on the canvas: right-click empty space, or
 * let go of an output over it.
 *
 * Dumb — it is handed the matches and says which one was chosen. The keys it
 * does handle are its own (up, down, enter, escape); the app's router leaves
 * bare keys alone while something is being typed into.
 */

import { useEffect, useRef, useState } from 'react'
import { cn } from './ui'

export interface SearchItem {
  transform: string
  label: string
  category: string
  /** The input a dragged output would be joined to, when there is one. */
  input?: string
}

export function NodeSearch({
  at,
  items,
  query,
  onQuery,
  onPick,
  onClose,
  note,
}: {
  at: { x: number; y: number }
  items: SearchItem[]
  query: string
  onQuery: (query: string) => void
  onPick: (item: SearchItem) => void
  onClose: () => void
  note?: string
}) {
  const [active, setActive] = useState(0)
  const field = useRef<HTMLInputElement>(null)
  const shown = items.slice(0, 8)

  useEffect(() => field.current?.focus(), [])
  useEffect(() => setActive(0), [query])

  return (
    <>
      {/* Anywhere else closes it, including a click on the canvas behind. */}
      <div className="fixed inset-0 z-40" onPointerDown={onClose} onContextMenu={onClose} />
      <div
        className="absolute z-50 w-60 rounded-lg bg-panel p-1 shadow-lg ring-1 ring-line"
        style={{ left: at.x, top: at.y }}
      >
        {note && <div className="px-2 pt-1 pb-0.5 text-[10px] text-faint">{note}</div>}
        <input
          ref={field}
          value={query}
          placeholder="Add a transform"
          spellCheck={false}
          onChange={(event) => onQuery(event.target.value)}
          onKeyDown={(event) => {
            if (event.key === 'ArrowDown') {
              event.preventDefault()
              setActive((k) => Math.min(k + 1, shown.length - 1))
            } else if (event.key === 'ArrowUp') {
              event.preventDefault()
              setActive((k) => Math.max(k - 1, 0))
            } else if (event.key === 'Enter') {
              event.preventDefault()
              if (shown[active]) onPick(shown[active])
            } else if (event.key === 'Escape') {
              event.preventDefault()
              onClose()
            }
          }}
          className="h-7 w-full rounded bg-sunken px-2 text-xs text-ink placeholder:text-faint"
        />
        <div className="max-h-64 overflow-y-auto pt-1">
          {shown.map((item, index) => (
            <div
              key={item.transform}
              onPointerEnter={() => setActive(index)}
              onPointerDown={(event) => {
                event.preventDefault()
                onPick(item)
              }}
              className={cn(
                'flex items-baseline gap-2 rounded px-2 py-1',
                index === active ? 'bg-brand-50' : '',
              )}
            >
              <span className="grow truncate text-[12px]">{item.label}</span>
              <span className="shrink-0 text-[10px] text-faint">{item.category}</span>
            </div>
          ))}
          {shown.length === 0 && (
            <div className="px-2 py-2 text-[11px] text-faint">Nothing matches.</div>
          )}
        </div>
      </div>
    </>
  )
}
