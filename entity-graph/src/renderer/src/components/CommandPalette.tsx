import React, { useEffect, useMemo, useRef, useState } from 'react'
import fuzzysort from 'fuzzysort'
import { Minimize01 } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { usePendingCall } from '../state/hooks'
import {
  advanceArg,
  cancelCall,
  chooseTool,
  minimisePending,
  retreatArg,
  setArg,
  setPendingQuery,
  submitCall,
} from '../tools/call'
import { formatArg, parseArg } from '../tools/args'
import { keyHint } from '../tools/keys'
import { findTool, listedTools } from '../tools/registry'
import { argsOf, kindOf, type ToolSpec } from '../tools/types'

const PANEL_WIDTH = 384 // w-96

/**
 * The palette is a view onto the pending call, not a thing with state of its own:
 * a launcher while no tool is chosen, and the current argument's field once one
 * is. The one input serves both, so nothing pops up underneath as you go.
 *
 * Anchored (from a right-click) it reads as a context menu at the cursor;
 * unanchored it is the centred launcher. Both are the same call, differing only
 * in where it's drawn.
 */
export function CommandPalette(): React.JSX.Element | null {
  const pending = usePendingCall()
  const visible = pending?.display.kind === 'palette' ? pending : null

  // A pending call can name a tool the list hides (a hotkey-only one) and its
  // arguments still need collecting, so this looks in the whole registry.
  const tool = useMemo(
    () => (visible?.toolId ? (findTool(visible.toolId) ?? null) : null),
    [visible?.toolId],
  )
  const args = tool ? argsOf(tool) : []
  const activeArg = args.find((a) => a.name === visible?.activeArg) ?? null

  // Transient input state: the text being typed, and where the arrow keys are.
  const [text, setText] = useState('')
  const [error, setError] = useState<string | null>(null)
  const [activeIndex, setActiveIndex] = useState(0)
  const inputRef = useRef<HTMLInputElement>(null)
  const activeRowRef = useRef<HTMLButtonElement>(null)

  // Reseed the buffer whenever the call moves to another argument (or another
  // call entirely). The stored value is the source of truth; this is just its
  // editable form.
  const seedKey = `${visible?.callId ?? ''}|${visible?.toolId ?? ''}|${visible?.activeArg ?? ''}`
  useEffect(() => {
    if (!visible) return
    setText(visible.activeArg ? formatArg(visible.args[visible.activeArg]) : '')
    setError(null)
    inputRef.current?.focus()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [seedKey])

  const query = visible?.query ?? ''
  const matches = useMemo(() => {
    const tools = listedTools()
    const q = query.trim()
    if (!q) return tools
    return fuzzysort
      .go(q, tools, { keys: ['label', (t) => t.aliases?.join(' ') ?? ''] })
      .map((r) => r.obj)
  }, [query])
  const active = matches.length ? Math.min(activeIndex, matches.length - 1) : 0

  useEffect(() => {
    if (!tool) activeRowRef.current?.scrollIntoView({ block: 'nearest' })
  }, [active, tool])

  if (!visible) return null

  /** Write the buffer into the call. Returns false when it doesn't parse. */
  const flush = (): boolean => {
    if (!activeArg) return true
    const parsed = parseArg(activeArg, text)
    if (!parsed.ok) {
      setError(parsed.message)
      return false
    }
    setArg(activeArg.name, parsed.value)
    return true
  }

  const onChange = (next: string): void => {
    setText(next)
    setError(null)
    if (!activeArg) {
      setPendingQuery(next)
      setActiveIndex(0)
      return
    }
    // Keep the stored value current as you type, so a call abandoned mid-word
    // still resumes with what was typed. Text that doesn't parse yet is simply
    // not written through; the error waits until Tab or Enter.
    const parsed = parseArg(activeArg, next)
    if (parsed.ok) setArg(activeArg.name, parsed.value)
  }

  const onKeyDown = (e: React.KeyboardEvent): void => {
    if (e.key === 'Escape') {
      e.preventDefault()
      cancelCall()
      return
    }
    if (tool) {
      // Argument entry. Enter is the only thing that can run the tool; Tab moves
      // on, and off the first argument Shift+Tab returns to the tool list.
      if (e.key === 'Enter') {
        e.preventDefault()
        if (flush()) setError(submitCall())
      } else if (e.key === 'Tab') {
        e.preventDefault()
        if (!flush()) return
        if (e.shiftKey) retreatArg()
        else advanceArg()
      }
      return
    }
    // Browsing the list.
    if (e.key === 'Tab' && e.shiftKey) {
      // Swallowed so focus never escapes to the page behind the palette.
      e.preventDefault()
      return
    }
    const target = matches[active]
    if ((e.key === 'Enter' || e.key === 'Tab') && target) {
      e.preventDefault()
      // Tab steps into a tool's arguments but never runs an argument-less one:
      // that still wants Enter as its confirmation.
      if (e.key === 'Enter' || argsOf(target).length > 0) chooseTool(target.id)
      return
    }
    if (e.key === 'ArrowDown' && matches.length) {
      e.preventDefault()
      setActiveIndex((i) => (Math.min(i, matches.length - 1) + 1) % matches.length)
    }
    if (e.key === 'ArrowUp' && matches.length) {
      e.preventDefault()
      setActiveIndex((i) => (Math.min(i, matches.length - 1) + matches.length - 1) % matches.length)
    }
  }

  const placeholder = activeArg
    ? kindOf(activeArg) === 'select'
      ? `${activeArg.label} (${(activeArg.options ?? []).join(' / ')})`
      : (activeArg.placeholder ?? activeArg.label)
    : 'Run a command…'
  const stepIndex = activeArg ? args.findIndex((a) => a.name === activeArg.name) : -1

  const anchor = visible.display.kind === 'palette' ? visible.display.anchor : null
  const panelStyle: React.CSSProperties = anchor
    ? {
        top: Math.min(anchor.y, window.innerHeight - 360),
        left: Math.min(anchor.x, window.innerWidth - PANEL_WIDTH - 8),
      }
    : { top: '8rem', left: '50%', transform: 'translateX(-50%)' }

  return (
    <div
      className={cn('fixed inset-0 z-50', !anchor && 'bg-gray-950/10 backdrop-blur-xs')}
      onClick={cancelCall}
      onContextMenu={(e) => {
        // A right-click on the backdrop closes; stop it reaching the window
        // handler, which would otherwise immediately reopen the palette.
        e.preventDefault()
        e.stopPropagation()
        cancelCall()
      }}
    >
      <div
        className="absolute w-96 max-w-[calc(100vw-1rem)] overflow-hidden rounded-xl bg-white shadow-lg"
        style={panelStyle}
        onClick={(e) => e.stopPropagation()}
        onContextMenu={(e) => e.stopPropagation()}
      >
        <div className="flex items-center border-b border-gray-100">
          <input
            ref={inputRef}
            autoFocus
            value={text}
            onChange={(e) => onChange(e.target.value)}
            onKeyDown={onKeyDown}
            placeholder={placeholder}
            // What the user types is entity content, so it takes the editor's
            // serif; the placeholder is UI chrome, so it stays on the sans.
            className="min-w-0 flex-1 bg-transparent px-4 py-3.5 font-serif text-[14px] text-gray-900 outline-none placeholder:font-sans placeholder:text-[13px] placeholder:text-gray-400"
          />
          {tool && (
            <span className="flex items-center gap-1.5 whitespace-nowrap px-3 text-xs font-medium text-gray-400">
              {tool.label}
              {args.length > 1 && (
                <span className="text-gray-300">
                  {stepIndex + 1}/{args.length}
                </span>
              )}
              <button
                className="text-gray-300 hover:text-gray-600 focus:outline-none"
                onClick={minimisePending}
                title="Minimise to the corner"
                aria-label="Minimise"
              >
                <Minimize01 size={13} />
              </button>
            </span>
          )}
        </div>

        {tool ? (
          error && <div className="px-4 py-2.5 text-xs text-error-600">{error}</div>
        ) : (
          <ul className="max-h-80 overflow-y-auto py-1">
            {matches.length === 0 ? (
              <li className="px-4 py-3 text-[13px] text-gray-400">No matching commands.</li>
            ) : (
              matches.map((candidate, i) => (
                <li key={candidate.id}>
                  <button
                    ref={i === active ? activeRowRef : undefined}
                    onClick={() => chooseTool(candidate.id)}
                    onMouseMove={() => setActiveIndex(i)}
                    className={cn(
                      'flex w-full items-center justify-between px-4 py-2.5 text-left text-[13px] focus:outline-none',
                      i === active && 'bg-gray-100/70',
                    )}
                  >
                    <span className="font-medium text-gray-800">{candidate.label}</span>
                    <Trailing tool={candidate} />
                  </button>
                </li>
              ))
            )}
          </ul>
        )}
      </div>
    </div>
  )
}

/** An ellipsis for tools that ask for something, else the hotkey or category. */
function Trailing({ tool }: { tool: ToolSpec }): React.JSX.Element | null {
  const label = argsOf(tool).length ? '…' : (keyHint(tool.keys) ?? tool.hint)
  return label ? <span className="text-xs text-gray-400">{label}</span> : null
}
