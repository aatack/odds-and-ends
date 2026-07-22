import React, { useEffect, useMemo, useRef, useState, useSyncExternalStore } from 'react'
import fuzzysort from 'fuzzysort'
import { cn } from '../helpers/cn'

// ---------------------------------------------------------------------------
// Command model
// ---------------------------------------------------------------------------

/**
 * One argument the palette prompts for before running a command. Its collected
 * (string) value is passed to the command's `run` keyed by {@link name}. A field
 * is prefilled from the palette's context when a context key shares its name —
 * so a command with an `entityId` field is auto-populated when the palette is
 * opened over an entity.
 */
export interface PaletteField {
  /** Argument name; also the context key that can prefill it (e.g. "entityId"). */
  name: string
  label: string
  /** How the value is entered. Defaults to "text". */
  kind?: 'text' | 'number' | 'select'
  /** Choices shown for a "select" field. */
  options?: readonly string[]
  /** Optional fields may be left blank; they're omitted from the collected values. */
  optional?: boolean
  placeholder?: string
}

export interface Command {
  id: string
  label: string
  /** Extra terms the fuzzy search matches against, e.g. synonyms for the label. */
  aliases?: string[]
  /** Faint text on the right (e.g. a section or shortcut). */
  hint?: string
  /**
   * Arguments prompted for, one at a time, before the command runs. Omitted for
   * commands that act on the current selection with no arguments.
   */
  fields?: PaletteField[]
  /** Collected argument values, keyed by field name (empty for fieldless commands). */
  run: (values: Record<string, string>) => void
}

// ---------------------------------------------------------------------------
// Global open-store — so a right-click anywhere can open the palette in-situ
// (at the cursor) with a context object, without threading a callback down the
// whole component tree. Mirrors the toast store's module-level pub/sub.
// ---------------------------------------------------------------------------

/** Values that can prefill matching fields (e.g. `{ entityId }` under an entity). */
export type PaletteContext = Record<string, string>

interface PaletteOpen {
  context: PaletteContext
  /** Screen position to anchor the palette at; null centres it (the ⌘P launcher). */
  anchor: { x: number; y: number } | null
}

let openState: PaletteOpen | null = null
const listeners = new Set<() => void>()
const emit = (): void => listeners.forEach((l) => l())

/** Open the palette, optionally anchored at a point and seeded with context. */
export function openCommandPalette(opts?: { context?: PaletteContext; anchor?: { x: number; y: number } }): void {
  openState = { context: opts?.context ?? {}, anchor: opts?.anchor ?? null }
  emit()
}

export function closeCommandPalette(): void {
  if (openState) {
    openState = null
    emit()
  }
}

/** Toggle the centred launcher (⌘P / the header button). */
export function toggleCommandPalette(): void {
  if (openState) closeCommandPalette()
  else openCommandPalette()
}

function usePaletteOpen(): PaletteOpen | null {
  return useSyncExternalStore(
    (l) => {
      listeners.add(l)
      return () => listeners.delete(l)
    },
    () => openState,
    () => openState,
  )
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Seed a command's wizard: each field starts from the matching context value
// (e.g. an `entityId` field from `context.entityId`), or blank.
function initialValues(command: Command, context: PaletteContext): Record<string, string> {
  const values: Record<string, string> = {}
  for (const field of command.fields ?? []) values[field.name] = context[field.name] ?? ''
  return values
}

// Collect the entered values, dropping blank optional fields.
function buildValues(command: Command, values: Record<string, string>): Record<string, string> {
  const out: Record<string, string> = {}
  for (const field of command.fields ?? []) {
    const raw = values[field.name] ?? ''
    if (field.optional && raw.trim() === '') continue
    out[field.name] = raw
  }
  return out
}

const PANEL_WIDTH = 384 // w-96

// ---------------------------------------------------------------------------
// Component
// ---------------------------------------------------------------------------

/**
 * A ⌘/Ctrl+P launcher and right-click menu in one. Browsing filters the given
 * commands; picking one either runs it (no arguments) or steps through its
 * arguments one at a time in the very same input — the input becomes each field
 * in turn, so nothing pops up underneath. Copied from the orchestrator's
 * argument wizard, with the "focus default" generalised to a context object so a
 * right-click can prefill (e.g.) an `entityId` field.
 */
export function CommandPalette({ commands }: { commands: Command[] }): React.JSX.Element | null {
  const open = usePaletteOpen()
  const context = open?.context ?? {}

  const [query, setQuery] = useState('')
  // The command being configured, or null while browsing the list.
  const [selected, setSelected] = useState<Command | null>(null)
  const [values, setValues] = useState<Record<string, string>>({})
  const [step, setStep] = useState(0)
  const [error, setError] = useState<string | null>(null)
  // Which list row the arrow keys have highlighted.
  const [activeIndex, setActiveIndex] = useState(0)
  const activeRef = useRef<HTMLButtonElement>(null)
  const inputRef = useRef<HTMLInputElement>(null)

  // With no query, keep the commands in their given order; otherwise fuzzy-match
  // against the label and any aliases, sorting by relevance.
  const matches = useMemo(() => {
    const q = query.trim()
    if (!q) return commands
    return fuzzysort
      .go(q, commands, { keys: ['label', (c) => c.aliases?.join(' ') ?? ''] })
      .map((r) => r.obj)
  }, [query, commands])
  // Clamp so the highlight stays valid as the filtered list shrinks.
  const active = matches.length ? Math.min(activeIndex, matches.length - 1) : 0

  const fields = selected?.fields ?? []
  const stepIndex = Math.min(step, Math.max(0, fields.length - 1))
  const field = selected ? fields[stepIndex] : null
  const isLast = stepIndex >= fields.length - 1

  // Reset everything whenever the palette closes.
  useEffect(() => {
    if (!open) {
      setQuery('')
      setSelected(null)
      setValues({})
      setStep(0)
      setError(null)
      setActiveIndex(0)
    }
  }, [open])

  // Keep focus in the one input as we swap between browsing and each step, and
  // keep the highlighted list row scrolled into view.
  useEffect(() => {
    if (open) inputRef.current?.focus()
  }, [open, selected, step])
  useEffect(() => {
    if (!selected) activeRef.current?.scrollIntoView({ block: 'nearest' })
  }, [active, selected])

  if (!open) return null

  const run = (command: Command, collected: Record<string, string>): void => {
    closeCommandPalette()
    command.run(collected)
  }

  // Enter the wizard (or run immediately when the command takes no arguments).
  const pick = (command: Command): void => {
    if (command.fields?.length) {
      setSelected(command)
      setValues(initialValues(command, context))
      setStep(0)
      setError(null)
    } else {
      run(command, {})
    }
  }

  const submit = (): void => {
    if (!selected) return
    // Require every non-optional field to be filled before running.
    const missing = (selected.fields ?? []).find(
      (f) => !f.optional && (values[f.name] ?? '').trim() === '',
    )
    if (missing) {
      setError(`${missing.label} is required`)
      return
    }
    run(selected, buildValues(selected, values))
  }

  const forward = (): void => {
    if (isLast) submit()
    else {
      setStep(stepIndex + 1)
      setError(null)
    }
  }
  const backward = (): void => {
    if (stepIndex === 0) {
      // Step back off the first field returns to browsing the command list.
      setSelected(null)
      setError(null)
    } else {
      setStep(stepIndex - 1)
      setError(null)
    }
  }

  const onKeyDown = (e: React.KeyboardEvent): void => {
    if (e.key === 'Escape') {
      e.preventDefault()
      closeCommandPalette()
      return
    }
    if (selected) {
      // Argument entry. Enter advances (running on the last step); Tab advances
      // too, except on the last step where only Enter may run it.
      if (e.key === 'Enter') {
        e.preventDefault()
        forward()
      } else if (e.key === 'Tab' && e.shiftKey) {
        e.preventDefault()
        backward()
      } else if (e.key === 'Tab') {
        e.preventDefault()
        if (!isLast) forward()
      }
      return
    }
    // Browsing the list. Shift+Tab is swallowed so focus never escapes the
    // palette to the page behind it.
    if (e.key === 'Tab' && e.shiftKey) {
      e.preventDefault()
      return
    }
    const target = matches[active]
    // Enter selects the highlighted command, running it if it takes no arguments.
    if (e.key === 'Enter' && target) {
      e.preventDefault()
      pick(target)
    }
    // Tab advances into a command's arguments, but never runs a fieldless one:
    // that still needs Enter for the final confirmation.
    if (e.key === 'Tab' && target) {
      e.preventDefault()
      if (target.fields?.length) pick(target)
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

  // The one input serves both modes: a search box while browsing, the current
  // argument's field while entering one.
  const placeholder = field
    ? field.kind === 'select'
      ? `${field.label} (${(field.options ?? []).join(' / ')})`
      : field.placeholder ?? field.label
    : 'Run a command…'
  const value = field ? values[field.name] ?? '' : query
  const onChange = (next: string): void => {
    if (field) {
      setValues((v) => ({ ...v, [field.name]: next }))
      setError(null)
    } else {
      setQuery(next)
      setActiveIndex(0)
    }
  }

  // Anchored (right-click) → position at the cursor, clamped to the viewport;
  // otherwise centre it near the top like a classic launcher.
  const anchor = open.anchor
  const panelStyle: React.CSSProperties = anchor
    ? {
        top: Math.min(anchor.y, window.innerHeight - 360),
        left: Math.min(anchor.x, window.innerWidth - PANEL_WIDTH - 8),
      }
    : { top: '8rem', left: '50%', transform: 'translateX(-50%)' }

  return (
    <div
      className={cn('fixed inset-0 z-50', !anchor && 'bg-gray-950/10 backdrop-blur-xs')}
      onClick={closeCommandPalette}
      onContextMenu={(e) => {
        // A right-click on the backdrop closes; stop it reaching the window
        // handler, which would otherwise immediately re-open the palette.
        e.preventDefault()
        e.stopPropagation()
        closeCommandPalette()
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
            value={value}
            onChange={(e) => onChange(e.target.value)}
            onKeyDown={onKeyDown}
            placeholder={placeholder}
            className="min-w-0 flex-1 bg-transparent px-4 py-3.5 text-[13px] outline-none placeholder:text-gray-400"
          />
          {selected && (
            <span className="whitespace-nowrap px-4 text-xs font-medium text-gray-400">
              {selected.label}
              {fields.length > 1 && <span className="ml-1 text-gray-300">{stepIndex + 1}/{fields.length}</span>}
            </span>
          )}
        </div>

        {selected ? (
          error && <div className="px-4 py-2.5 text-xs text-error-600">{error}</div>
        ) : (
          <ul className="max-h-80 overflow-y-auto py-1">
            {matches.length === 0 ? (
              <li className="px-4 py-3 text-[13px] text-gray-400">No matching commands.</li>
            ) : (
              matches.map((command, i) => (
                <li key={command.id}>
                  <button
                    ref={i === active ? activeRef : undefined}
                    onClick={() => pick(command)}
                    onMouseMove={() => setActiveIndex(i)}
                    className={cn(
                      'flex w-full items-center justify-between px-4 py-2.5 text-left text-[13px] focus:outline-none',
                      i === active && 'bg-gray-100/70',
                    )}
                  >
                    <span className="font-medium text-gray-800">{command.label}</span>
                    {(command.fields?.length || command.hint) && (
                      <span className="text-xs text-gray-400">
                        {command.fields?.length ? '…' : command.hint}
                      </span>
                    )}
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
