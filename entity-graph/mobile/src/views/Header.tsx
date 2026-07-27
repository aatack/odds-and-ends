import React, { useEffect, useRef } from 'react'
import { ChevronLeft, DotsHorizontal, FlipBackward, X } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import * as A from '../state/actions'
import { useCanCall, useCrumbs, useView } from '../state/hooks'
import { topLevel } from '../state/types'
import { dispatch } from '../tools/dispatch'

// The top bar: where you are, one step back, and the two controls worth a permanent
// place — undo, because a thumb makes mistakes, and the menu, because everything
// else lives behind it.

export function Header(): React.JSX.Element {
  const view = useView()
  const crumbs = useCrumbs()
  const canUndo = useCanCall('popEvents')
  const here = crumbs[crumbs.length - 1]
  const trail = crumbs.slice(0, -1)

  return (
    <header className="shrink-0 bg-white pt-[var(--inset-top)] shadow-xs">
      <div className="flex items-center gap-1 px-1 py-1">
        <button
          type="button"
          aria-label="Back"
          disabled={view.stack.length < 2}
          onClick={() => dispatch('view.back')}
          className="flex size-11 shrink-0 items-center justify-center rounded-xl text-gray-500 active:bg-gray-100 disabled:opacity-25"
        >
          <ChevronLeft size={22} />
        </button>

        {/* The title is a button: tapping it opens the trail, which is the only way
            back to a level several steps up. */}
        <button
          type="button"
          onClick={() => dispatch('view.crumbs')}
          className="min-w-0 flex-1 rounded-xl px-2 py-1 text-left active:bg-gray-100"
        >
          {trail.length > 0 && (
            <span className="block truncate text-[11px] leading-tight text-gray-400">
              {trail.map((c) => c.label).join(' › ')}
            </span>
          )}
          <span className="block truncate text-[15px] font-semibold text-gray-900">
            {here?.label ?? '…'}
          </span>
        </button>

        {canUndo && (
          <button
            type="button"
            aria-label="Undo"
            onClick={() => dispatch('app.undo')}
            className="flex size-11 shrink-0 items-center justify-center rounded-xl text-gray-500 active:bg-gray-100"
          >
            <FlipBackward size={19} />
          </button>
        )}
        <button
          type="button"
          aria-label="Menu"
          onClick={() => dispatch('app.actions')}
          className="flex size-11 shrink-0 items-center justify-center rounded-xl text-gray-500 active:bg-gray-100"
        >
          <DotsHorizontal size={22} />
        </button>
      </div>

      {view.find != null && <FindField value={view.find} />}
      <Filters />
    </header>
  )
}

/** The find field. It edits `find` directly, so there is no draft to keep in step. */
function FindField({ value }: { value: string }): React.JSX.Element {
  const ref = useRef<HTMLInputElement>(null)
  useEffect(() => ref.current?.focus(), [])
  return (
    <div className="flex items-center gap-2 px-3 pb-2">
      <input
        ref={ref}
        value={value}
        onChange={(e) => A.setFind(e.target.value)}
        placeholder="Find in these rows…"
        // The soft keyboard's action key dismisses rather than submitting: the rows
        // filter as you type, so there is nothing to submit.
        enterKeyHint="done"
        onKeyDown={(e) => e.key === 'Enter' && e.currentTarget.blur()}
        className="min-w-0 flex-1 rounded-xl bg-gray-100 px-3 py-2.5 text-gray-900 outline-none placeholder:text-gray-400"
      />
      <button
        type="button"
        aria-label="Clear the search"
        onClick={() => dispatch('view.find.clear')}
        className="flex size-11 shrink-0 items-center justify-center rounded-xl text-gray-400 active:bg-gray-100"
      >
        <X size={18} />
      </button>
    </div>
  )
}

/** A pill per filter in force, so nothing is quietly hiding rows. */
function Filters(): React.JSX.Element | null {
  const view = useView()
  const level = topLevel(view)
  const pills: { label: string; onClear: () => void }[] = []
  if (view.sectionsOnly) {
    pills.push({ label: 'Sections only', onClear: () => dispatch('view.sections') })
  }
  if (level.direction === 'in') {
    pills.push({ label: 'Inbound links', onClear: () => dispatch('view.reverse') })
  }
  if (view.collapsed.length > 0) {
    pills.push({
      label: `${view.collapsed.length} folded`,
      onClear: () => dispatch('view.expandAll'),
    })
  }
  if (!pills.length) return null
  return (
    <div className="flex flex-wrap gap-1.5 px-3 pb-2">
      {pills.map((pill) => (
        <button
          key={pill.label}
          type="button"
          onClick={pill.onClear}
          className={cn(
            'flex items-center gap-1 rounded-full bg-gray-100 py-1 pr-1.5 pl-2.5',
            'text-[12px] text-gray-600 active:bg-gray-200',
          )}
        >
          {pill.label}
          <X size={12} />
        </button>
      ))}
    </div>
  )
}
