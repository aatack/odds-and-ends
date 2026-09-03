import { z } from 'zod'
import { zodToJsonSchema } from 'zod-to-json-schema'
import { ToolNotFoundError, type Safety, type ToolMeta } from './types'

// A tool as the side *implementing* it holds one: the schema is a zod type, so
// there is one source of truth for validation at call time and for the JSON
// Schema a caller is prompted from. `./types` holds the half that crosses a
// wire, which is this with the functions taken off.

/**
 * A single action a pensive can perform. Handlers take JSON and return JSON.
 */
export interface ToolDef<A = any, R = any> {
  /** Stable id; used for routing a call and for naming one in a script. */
  id: string
  /** Human-readable label. */
  name: string
  /** Markdown documentation. */
  description: string
  /** Argument schema. A `null` value for a field means "use the default". */
  args: z.ZodType<A>
  /**
   * Pre-computed JSON Schema for the args, used verbatim instead of converting
   * `args`. Set by a pensive that proxies another one, so a tool's original
   * schema survives the round trip.
   */
  jsonSchema?: Record<string, unknown>
  safety: Safety
  handler: (args: A) => Promise<R>
}

/** JSON Schema for a tool's args: the pre-computed one if present, else derived. */
export function argsJsonSchema(tool: ToolDef): Record<string, unknown> {
  if (tool.jsonSchema) return tool.jsonSchema
  return zodToJsonSchema(tool.args, { target: 'jsonSchema7' }) as Record<string, unknown>
}

/** A tool as a caller sees it. */
export function toolMeta(tool: ToolDef): ToolMeta {
  return {
    id: tool.id,
    name: tool.name,
    description: tool.description,
    safety: tool.safety,
    args: argsJsonSchema(tool),
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

/** Find a tool by id in a list, validate the arguments, and run it. */
export function callInList(tools: ToolDef[], toolId: string, args: unknown): Promise<unknown> {
  const tool = tools.find((t) => t.id === toolId)
  if (!tool) throw new ToolNotFoundError(toolId)
  return invokeTool(tool, args)
}
