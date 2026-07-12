import { z } from 'zod'

/**
 * How risky a tool is to call.
 * - `pure`: only reads; no effects.
 * - `safe-mutating`: only writes to the underlying event store, so its effects
 *   can be trivially undone.
 * - `dangerous`: interacts with the outside world in unpredictable ways
 *   (shell, HTTP, etc.).
 */
export type Safety = 'pure' | 'safe-mutating' | 'dangerous'

/** Ordering used by capability filters (`maxSafety`). Lower = safer. */
export const SAFETY_RANK: Record<Safety, number> = {
  pure: 0,
  'safe-mutating': 1,
  dangerous: 2,
}

/**
 * A single action a source can perform. Args are described by a zod schema
 * (one source of truth: validation at call time, JSON Schema for `/tools` and
 * MCP). Handlers take JSON and return JSON.
 */
export interface ToolDef<A = any, R = any> {
  /** Stable id; used for dedup (Combined) and call routing. */
  id: string
  /** Human-readable label. */
  name: string
  /** Markdown documentation. */
  description: string
  /** Argument schema. A `null` value for a field means "use the default". */
  args: z.ZodType<A>
  /**
   * Pre-computed JSON Schema for the args, used verbatim by `/tools` and MCP
   * when present (instead of converting `args`). Set by proxying sources like
   * Remote so a remote tool's original schema survives the round trip.
   */
  jsonSchema?: Record<string, unknown>
  safety: Safety
  handler: (args: A) => Promise<R>
}

/**
 * A source is just a set of tools, optionally exposed at a URL. Event-sourcing
 * is merely what some tools do under the hood — it is not part of this contract.
 */
export interface Source {
  id: string
  label: string
  /** The tool registry. Kept deliberately small on base sources. */
  tools(): ToolDef[]
  /** Look up a tool by id, validate args, invoke it. */
  call(toolId: string, args: unknown): Promise<unknown>
}

export class ToolNotFoundError extends Error {
  constructor(public toolId: string) {
    super(`No tool with id "${toolId}"`)
    this.name = 'ToolNotFoundError'
  }
}

/**
 * Drop keys whose value is explicitly `null` so that schema defaults /
 * `.optional()` apply. A required field (no default) that was null therefore
 * becomes "missing" and zod raises a validation error — matching the rule
 * "passing null means default; null for a required arg throws".
 */
export function stripNulls(raw: unknown): unknown {
  if (raw && typeof raw === 'object' && !Array.isArray(raw)) {
    const out: Record<string, unknown> = {}
    for (const [k, v] of Object.entries(raw as Record<string, unknown>)) {
      if (v !== null) out[k] = v
    }
    return out
  }
  return raw
}

/** Validate `rawArgs` against a tool's schema and invoke its handler. */
export async function invokeTool<R>(tool: ToolDef<any, R>, rawArgs: unknown): Promise<R> {
  const parsed = tool.args.parse(stripNulls(rawArgs ?? {}))
  return tool.handler(parsed)
}

/**
 * Base class implementing `call` in terms of `tools()`. Concrete sources only
 * need to supply `id`, `label`, and `tools()`.
 */
export abstract class ToolSource implements Source {
  abstract id: string
  abstract label: string
  abstract tools(): ToolDef[]

  async call(toolId: string, args: unknown): Promise<unknown> {
    const tool = this.tools().find((t) => t.id === toolId)
    if (!tool) throw new ToolNotFoundError(toolId)
    return invokeTool(tool, args)
  }
}
