import { zodToJsonSchema } from 'zod-to-json-schema'
import type { ToolDef } from './types'

/** JSON Schema for a tool's args: the pre-computed one if present, else derived. */
export function argsJsonSchema(tool: ToolDef): Record<string, unknown> {
  if (tool.jsonSchema) return tool.jsonSchema
  return zodToJsonSchema(tool.args, { target: 'jsonSchema7' }) as Record<string, unknown>
}

/** Metadata shape returned by the `/tools` endpoint. */
export interface ToolMeta {
  id: string
  name: string
  description: string
  safety: string
  args: Record<string, unknown>
}

export function toolMeta(tool: ToolDef): ToolMeta {
  return {
    id: tool.id,
    name: tool.name,
    description: tool.description,
    safety: tool.safety,
    args: argsJsonSchema(tool),
  }
}
