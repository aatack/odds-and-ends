import {
  DEFAULT_ARG,
  EMPTY_ARG,
  argValue,
  type ArgValue,
  type ArgValues,
  type CallContext,
} from '../state/types'
import { argsOf, kindOf, type ArgSpec, type ToolSpec } from './types'

// Argument values: seeding them from a context, parsing what the user types into
// them, and the navigation rules over them. Pure — the call machine in ./call
// applies these to state.

export type ParseResult = { ok: true; value: ArgValue } | { ok: false; message: string }

/**
 * Parse typed text into a value. Blank clears back to `default` where the tool
 * has one and `empty` otherwise, which is what makes "leave it as it was" and
 * "I haven't said yet" distinguishable in state.
 */
export function parseArg(arg: ArgSpec, text: string): ParseResult {
  const trimmed = text.trim()
  if (trimmed === '') return { ok: true, value: arg.hasDefault ? DEFAULT_ARG : EMPTY_ARG }
  switch (kindOf(arg)) {
    case 'number': {
      const n = Number(trimmed)
      if (!Number.isFinite(n)) return { ok: false, message: `${arg.label} must be a number` }
      return { ok: true, value: argValue(n) }
    }
    case 'boolean': {
      const t = trimmed.toLowerCase()
      if (['true', 'yes', 'on', '1'].includes(t)) return { ok: true, value: argValue(true) }
      if (['false', 'no', 'off', '0'].includes(t)) return { ok: true, value: argValue(false) }
      return { ok: false, message: `${arg.label} must be true or false` }
    }
    case 'select': {
      const match = (arg.options ?? []).find((o) => o.toLowerCase() === trimmed.toLowerCase())
      if (!match) return { ok: false, message: `${arg.label} must be one of ${(arg.options ?? []).join(', ')}` }
      return { ok: true, value: argValue(match) }
    }
    case 'json': {
      try {
        return { ok: true, value: argValue(JSON.parse(trimmed)) }
      } catch {
        return { ok: false, message: `${arg.label} must be valid JSON` }
      }
    }
    default:
      // Strings keep their whitespace; only the emptiness test is trimmed.
      return { ok: true, value: argValue(text) }
  }
}

/** How a stored value reads back into the input when the user lands on it. */
export function formatArg(value: ArgValue | undefined): string {
  if (!value || value.kind !== 'value') return ''
  const v = value.value
  if (typeof v === 'string') return v
  if (v == null) return ''
  return typeof v === 'object' ? JSON.stringify(v) : String(v)
}

/**
 * What the context has to say about an argument, whether or not it applies
 * itself — the value the palette offers on a context that doesn't autofill.
 */
export const contextValue = (arg: ArgSpec, context: CallContext): unknown =>
  arg.fromContext != null ? context.values[arg.fromContext] : undefined

/** True when the context supplied this argument, so Tab should skip past it. */
export const filledFromContext = (arg: ArgSpec, context: CallContext): boolean =>
  context.autofill !== false && contextValue(arg, context) !== undefined

/** Every argument's starting value: from the context, then from the tool's default. */
export function seedArgs(tool: ToolSpec, context: CallContext): ArgValues {
  const out: ArgValues = {}
  for (const arg of argsOf(tool)) {
    const fromContext = filledFromContext(arg, context) ? contextValue(arg, context) : undefined
    out[arg.name] =
      fromContext !== undefined
        ? argValue(fromContext)
        : arg.hasDefault
          ? DEFAULT_ARG
          : EMPTY_ARG
  }
  return out
}

const nameAt = (tool: ToolSpec, index: number): string | null => argsOf(tool)[index]?.name ?? null

const indexOf = (tool: ToolSpec, name: string | null): number =>
  argsOf(tool).findIndex((a) => a.name === name)

/** The first argument still awaiting a value. */
export function firstEmpty(tool: ToolSpec, args: ArgValues): string | null {
  return argsOf(tool).find((a) => (args[a.name] ?? EMPTY_ARG).kind === 'empty')?.name ?? null
}

/** The next empty argument after `from` — where Enter goes. */
export function nextEmptyAfter(tool: ToolSpec, args: ArgValues, from: string | null): string | null {
  const at = indexOf(tool, from)
  return (
    argsOf(tool)
      .slice(at + 1)
      .find((a) => (args[a.name] ?? EMPTY_ARG).kind === 'empty')?.name ?? null
  )
}

/**
 * Where Tab goes: the next argument the context didn't fill. It still lands on
 * ones carrying a default, so those can be overridden. Null when there is
 * nothing further, in which case Tab does nothing — it never runs the tool.
 */
export function nextStopAfter(
  tool: ToolSpec,
  context: CallContext,
  from: string | null,
): string | null {
  const at = indexOf(tool, from)
  return (
    argsOf(tool)
      .slice(at + 1)
      .find((a) => !filledFromContext(a, context))?.name ?? null
  )
}

/**
 * Where Shift+Tab goes: strictly one argument back, including over ones the
 * context filled, so they can be inspected. Null on the first argument, which
 * the caller reads as "return to the tool list".
 */
export function prevStopBefore(tool: ToolSpec, from: string | null): string | null {
  const at = indexOf(tool, from)
  return at <= 0 ? null : nameAt(tool, at - 1)
}

/** The first required argument still empty, if any. */
export function missingRequired(tool: ToolSpec, args: ArgValues): ArgSpec | null {
  return (
    argsOf(tool).find((a) => !a.optional && (args[a.name] ?? EMPTY_ARG).kind === 'empty') ?? null
  )
}

/**
 * The plain object handed to `run`: values as entered, `default` as null (the
 * source contract's "use the default"), and empty optional arguments omitted.
 */
export function resolveArgs(tool: ToolSpec, args: ArgValues): Record<string, unknown> {
  const out: Record<string, unknown> = {}
  for (const arg of argsOf(tool)) {
    const v = args[arg.name] ?? EMPTY_ARG
    if (v.kind === 'empty') continue
    out[arg.name] = v.kind === 'default' ? null : v.value
  }
  return out
}
