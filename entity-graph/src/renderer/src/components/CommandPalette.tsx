import React, { useEffect, useMemo, useRef, useState, useSyncExternalStore } from 'react'
import fuzzysort from 'fuzzysort'
import { cn } from '../helpers/cn'
import { logAction, logCancelled } from '../helpers/actionLog'

// ---------------------------------------------------------------------------
// Command model
// ---------------------------------------------------------------------------

/**
 * One argument the palette prompts for before running a command. Its collected
 * (string) value is passed to the command's `run` keyed by {@link name}, and
 * prefilled from the palette's context: a field is seeded from the context value
 * named by {@link fromContext} (defaulting to its own name). So an `entityId`
 * field auto-fills when the palette is opened over an entity, and a `sourceId`
 * field can opt into that same value with `fromContext: 'entityId'`.
 */
export interface PaletteField {
  /** Argument name; the key its value is passed to `run` under. */
  name: string
  label: string
  /** Context key that prefills this field. Defaults to {@link name}. */
  fromContext?: string
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
   * commands that act on the current selection with no arguments. Field-bearing
   * commands are the ones recorded in the activity log.
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

/** Reopen a specific command's wizard prefilled — used to resume a cancelled one. */
export interface PaletteResume {
  /** The log entry's id, carried back so re-cancelling updates it in place. */
  key: string
  commandId: string
  values: Record<string, string>
}

interface PaletteOpen {
  context: PaletteContext
  /** Screen position to anchor the palette at; null centres it (the ⌘P launcher). */
  anchor: { x: number; y: number } | null
  resume: PaletteResume | null
}

let openState: PaletteOpen | null = null
const listeners = new Set<() => void>()
const emit = (): void => listeners.forEach((l) => l())

/** Open the palette, optionally anchored, seeded with context, or resuming one. */
export function openCommandPalette(opts?: {
  context?: PaletteContext
  anchor?: { x: number; y: number }
  resume?: PaletteResume
}): void {
  openState = {
    context: opts?.context ?? {},
    anchor: opts?.anchor ?? null,
    resume: opts?.resume ?? null,
  }
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

// Seed a command's wizard: each field starts from its matching context value
// (by `fromContext`, defaulting to the field name), or blank.
function initialValues(command: Command, context: PaletteContext): Record<string, string> {
  const values: Record<string, string> = {}
  for (const field of command.fields ?? []) {
    values[field.name] = context[field.fromContext ?? field.name] ?? ''
  }
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

// The first field still blank, or -1 when every field is filled. Blank fields are
// the only ones the wizard stops on — prefilled ones are skipped.
function firstEmptyField(command: Command, values: Record<string, string>): number {
  return (command.fields ?? []).findIndex((f) => (values[f.name] ?? '').trim() === '')
}

const PANEL_WIDTH = 384 // w-96

// ---------------------------------------------------------------------------
// Component
// ---------------------------------------------------------------------------

/**
 * A ⌘/Ctrl+P launcher and right-click menu in one. Browsing filters the given
 * commands; picking one either runs it (no arguments) or steps through its
 * arguments one at a time in the very same input — the input becomes each field
 * in turn, so nothing pops up underneath. Ported from the orchestrator's argument
 * wizard: the "focus default" is generalised to a context object (so a right-click
 * prefills an `entityId` field), pre-filled fields are auto-skipped, and abandoned
 * wizards are logged to the activity trail and can be resumed.
 */
export function CommandPalette({ commands }: { commands: Command[] }): React.JSX.Element | null {
  const open = usePaletteOpen()
  const context = open?.context ?? {}

  const [query, setQuery] = useState('')
  // The command being configured, or null while browsing the list.
  const [selected, setSelected] = useState<Command | null>(null)
  const [values, setValues] = useState<Record<string, string>>({})
  // The id minted when this wizard opened, and the pristine values it started
  // from — used to log a cancellation under a stable id, and only when the
  // arguments were actually touched.
  const [wizardKey, setWizardKey] = useState<string | null>(null)
  const [defaults, setDefaults] = useState<Record<string, string>>({})
  const [step, setStep] = useState(0)
  const [error, setError] = useState<string | null>(null)
  // Which list row the arrow keys have highlighted.
  const [activeIndex, setActiveIndex] = useState(0)
  const activeRef = useRef<HTMLButtonElement>(null)
  const inputRef = useRef<HTMLInputElement>(null)
  // The resume request already applied, so it isn't re-applied every render.
  const consumedResume = useRef<string | null>(null)

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
  // The next field after this one that still needs a value; -1 when this is the
  // last thing to fill (Enter then runs).
  const nextEmpty = fields.findIndex((f, i) => i > stepIndex && (values[f.name] ?? '').trim() === '')

  const run = (command: Command, collected: Record<string, string>): void => {
    closeCommandPalette()
    command.run(collected)
  }

  // Run a field-bearing command, recording it in the activity log first.
  const commit = (command: Command, collected: Record<string, string>, key: string): void => {
    logAction({
      key,
      commandId: command.id,
      title: command.label,
      status: 'success',
      error: null,
      values: collected,
    })
    run(command, collected)
  }

  // Escape / outside click. If a wizard's arguments were touched, log it as
  // cancelled (or update the resumed entry) so it can be resumed from the log.
  const cancel = (): void => {
    if (selected && wizardKey) {
      const touched = fields.some((f) => (values[f.name] ?? '') !== (defaults[f.name] ?? ''))
      if (touched) logCancelled(wizardKey, { id: selected.id, title: selected.label }, values)
    }
    closeCommandPalette()
  }

  // Reset everything whenever the palette closes.
  useEffect(() => {
    if (!open) {
      setQuery('')
      setSelected(null)
      setValues({})
      setWizardKey(null)
      setDefaults({})
      setStep(0)
      setError(null)
      setActiveIndex(0)
      consumedResume.current = null
    }
  }, [open])

  // Resuming a cancelled action: jump straight into its wizard, prefilled, and
  // reuse its log id so re-cancelling updates that entry instead of adding one.
  useEffect(() => {
    if (!open?.resume || consumedResume.current === open.resume.key) return
    const command = commands.find((c) => c.id === open.resume!.commandId)
    consumedResume.current = open.resume.key
    if (!command) return
    const pristine = initialValues(command, open.context)
    const vals = { ...pristine, ...open.resume.values }
    setSelected(command)
    setValues(vals)
    setDefaults(pristine)
    setWizardKey(open.resume.key)
    const empty = firstEmptyField(command, vals)
    setStep(empty >= 0 ? empty : Math.max(0, (command.fields?.length ?? 1) - 1))
    setError(null)
  }, [open, commands])

  // Keep focus in the one input as we swap between browsing and each step, and
  // keep the highlighted list row scrolled into view.
  useEffect(() => {
    if (open) inputRef.current?.focus()
  }, [open, selected, step])
  useEffect(() => {
    if (!selected) activeRef.current?.scrollIntoView({ block: 'nearest' })
  }, [active, selected])

  if (!open) return null

  // Enter the wizard, skipping fields already satisfied by context — and running
  // straight away when nothing is left to fill (or the command takes no args).
  const pick = (command: Command): void => {
    if (!command.fields?.length) {
      run(command, {})
      return
    }
    const init = initialValues(command, context)
    const empty = firstEmptyField(command, init)
    if (empty < 0) {
      commit(command, buildValues(command, init), crypto.randomUUID())
      return
    }
    setSelected(command)
    setValues(init)
    setDefaults(init)
    setWizardKey(crypto.randomUUID())
    setStep(empty)
    setError(null)
  }

  const submit = (): void => {
    if (!selected) return
    // Require every non-optional field to be filled before running.
    const missing = fields.find((f) => !f.optional && (values[f.name] ?? '').trim() === '')
    if (missing) {
      setError(`${missing.label} is required`)
      return
    }
    commit(selected, buildValues(selected, values), wizardKey ?? crypto.randomUUID())
  }

  const forward = (): void => {
    if (nextEmpty >= 0) {
      setStep(nextEmpty)
      setError(null)
    } else {
      submit()
    }
  }
  const backward = (): void => {
    if (stepIndex === 0) {
      // Step back off the first field returns to browsing (not a cancellation).
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
      cancel()
      return
    }
    if (selected) {
      // Argument entry. Enter advances (running once nothing is left); Tab
      // advances too, except when there's nothing left, where only Enter runs it.
      if (e.key === 'Enter') {
        e.preventDefault()
        forward()
      } else if (e.key === 'Tab' && e.shiftKey) {
        e.preventDefault()
        backward()
      } else if (e.key === 'Tab') {
        e.preventDefault()
        if (nextEmpty >= 0) forward()
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
      onClick={cancel}
      onContextMenu={(e) => {
        // A right-click on the backdrop closes; stop it reaching the window
        // handler, which would otherwise immediately re-open the palette.
        e.preventDefault()
        e.stopPropagation()
        cancel()
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
              {fields.length > 1 && (
                <span className="ml-1 text-gray-300">
                  {stepIndex + 1}/{fields.length}
                </span>
              )}
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
