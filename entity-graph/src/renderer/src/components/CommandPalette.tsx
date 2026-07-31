import React, { useEffect, useMemo, useRef, useState } from 'react'
import fuzzysort from 'fuzzysort'
import { Minimize01 } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { Badge } from './ui/Badge'
import { Popup } from './ui/Popup'
import { useAtomValue, useCalls, usePendingCall } from '../state/hooks'
import { argValue, type ArgValue } from '../state/types'
import {
  advanceArg,
  cancelCall,
  chooseTool,
  fillActiveArg,
  lastArgValue,
  minimisePending,
  retreatArg,
  setArg,
  setPendingQuery,
  submitCall,
} from '../tools/call'
import { contextValue, formatArg, parseArg } from '../tools/args'
import { keyHint, matchesKey, type KeyBinding } from '../tools/keys'
import { integrationsAtom } from '../tools/integrationTools'
import { findTool, listedTools } from '../tools/registry'
import { argsOf, kindOf, type ToolSpec } from '../tools/types'

/**
 * The keys that take an offer. Declared as bindings rather than spelled out in
 * the handler, so the pill on a row and the key that fills it cannot drift — and
 * so both read through the same `matchesKey`/`keyHint` pair as every tool hotkey.
 */
const TAKE_CONTEXT: KeyBinding = { key: 'Enter', mod: true }
const TAKE_RECENT: KeyBinding = { key: 'Enter', shift: true }

/**
 * A value the palette can put in the field without it being typed. Both offers
 * are the same gesture — take what is already known — so they read as one list
 * under the field, each carrying the key that takes it.
 */
interface Offer {
  /** Where it came from, in the palette's own voice. */
  note: string
  key: KeyBinding
  /** As it will read in the field. */
  text: string
  value: unknown
}

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
  // The registry isn't fixed: the server's integrations join it once a source is
  // open, so the list is recomputed when they land as well as when the query
  // changes.
  const integrations = useAtomValue(integrationsAtom)
  const matches = useMemo(() => {
    const tools = listedTools()
    const q = query.trim()
    if (!q) return tools
    return fuzzysort
      .go(q, tools, { keys: ['label', (t) => t.aliases?.join(' ') ?? ''] })
      .map((r) => r.obj)
  }, [query, integrations])
  const active = matches.length ? Math.min(activeIndex, matches.length - 1) : 0

  // What this tool was last given for the argument being entered. Read straight
  // from the call log, so it is precisely the history the activity panel shows —
  // and read reactively, since a call settling behind the palette adds to it.
  const calls = useCalls()
  const remembered = useMemo(
    () =>
      visible?.toolId && visible.activeArg
        ? lastArgValue(calls, visible.toolId, visible.activeArg, visible.callId)
        : undefined,
    [calls, visible?.toolId, visible?.activeArg, visible?.callId],
  )

  useEffect(() => {
    if (!tool) activeRowRef.current?.scrollIntoView({ block: 'nearest' })
  }, [active, tool])

  if (!visible) return null

  // What could go in the field without being typed: where the user is, and what
  // this tool was given last time. Neither is applied silently — a context that
  // fills its own arguments has already done so, and the palette opened cold
  // leaves them blank on purpose — so both are offered as rows under the field.
  //
  // One that matches what is already there is not an offer: there would be
  // nothing to press it for. The same goes for a remembered value the context is
  // already offering, which would otherwise be the same row twice.
  const offers: Offer[] = []
  const push = (note: string, key: KeyBinding, value: unknown): void => {
    if (value === undefined) return
    const shown = formatArg(argValue(value))
    // Nothing to read is nothing to offer: a null reads as blank, and a blank row
    // with a key on it would be a promise of something that isn't there.
    if (shown === '' || shown === text || offers.some((o) => o.text === shown)) return
    offers.push({ note, key, text: shown, value })
  }
  if (activeArg) {
    push('From here', TAKE_CONTEXT, contextValue(activeArg, visible.context))
    push('Last used', TAKE_RECENT, remembered)
  }

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

  /** Put an offer in the field, rather than making it be typed out. */
  const take = (offer: Offer): void => {
    const applied = fillActiveArg(offer.value)
    if (!applied) return
    // The call may have stayed on this argument, in which case nothing reseeds
    // the buffer and it would still hold what was there before.
    setText(formatArg(applied))
    setError(null)
  }

  const onKeyDown = (e: React.KeyboardEvent): void => {
    if (e.key === 'Escape') {
      e.preventDefault()
      cancelCall()
      return
    }
    if (tool) {
      // Argument entry. Plain Enter is the only thing that can run the tool; Tab
      // moves on, and off the first argument Shift+Tab returns to the tool list.
      if (e.key === 'Enter' && (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey)) {
        // A modified Enter takes the offer that names it. With nothing to take it
        // does nothing at all — and in particular does not run, since a key
        // pressed for one thing must not quietly do another.
        e.preventDefault()
        const offer = offers.find((o) => matchesKey(o.key, e))
        if (offer) take(offer)
      } else if (e.key === 'Enter') {
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
      // Tab steps into a tool's arguments and stops there, whether or not any are
      // outstanding: running is Enter's, always. A tool with no arguments has
      // nothing to step into, so Tab leaves the list where it is.
      if (e.key === 'Enter') chooseTool(target.id)
      else if (argsOf(target).length > 0) chooseTool(target.id, { run: false })
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

  return (
    <Popup anchor={anchor} onClose={cancelCall}>
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
        <>
          {error && <div className="px-4 py-2.5 text-xs text-error-600">{error}</div>}
          {/* Where the tool list was, and read the same way: rows under the field,
              each one a value you can have for a keystroke. This is the whole of
              the affordance — there is no icon beside the field, because a value
              you can see is worth more than a symbol you have to remember. */}
          {offers.length > 0 && (
            <ul className="py-1">
              {offers.map((offer) => (
                <li key={offer.note}>
                  <button
                    // Keeps the caret where it is: the field is being filled in,
                    // not left.
                    onMouseDown={(e) => e.preventDefault()}
                    onClick={() => take(offer)}
                    className="flex w-full items-center gap-3 px-4 py-2.5 text-left focus:outline-none hover:bg-gray-100/70"
                  >
                    <span className="shrink-0 text-xs text-gray-400">{offer.note}</span>
                    {/* The value is the user's content, so it takes the serif —
                        and the room, since an entity id or a prompt is long. */}
                    <span className="min-w-0 flex-1 truncate font-serif text-[13px] text-gray-800">
                      {offer.text}
                    </span>
                    <Badge className="shrink-0">{keyHint([offer.key])}</Badge>
                  </button>
                </li>
              ))}
            </ul>
          )}
        </>
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
    </Popup>
  )
}

/** An ellipsis for tools that ask for something, else the hotkey or category. */
function Trailing({ tool }: { tool: ToolSpec }): React.JSX.Element | null {
  const label = argsOf(tool).length ? '…' : (keyHint(tool.keys) ?? tool.hint)
  return label ? <span className="text-xs text-gray-400">{label}</span> : null
}
