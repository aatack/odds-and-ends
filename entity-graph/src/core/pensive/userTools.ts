import { z } from 'zod'
import { TOOL_ID } from '../builtins'
import { toolArgumentsSchema } from '../toolArguments'
import { entityWrapper } from './tools'
import type { ToolDef } from './tool'
import { SAFETY_RANK, type Pensive, type Safety } from './types'

/** The reserved entity whose direct children are user-defined tools. */
export const TOOLS_ENTITY_ID = '@tools'

export interface UserToolOptions {
  defaultAuthor?: string
}

function isSafety(v: unknown): v is Safety {
  return typeof v === 'string' && v in SAFETY_RANK
}

/**
 * Discover user-defined tools stored in the database. The reserved entity
 * `@tools` has one direct child per tool; a child is tool-shaped when it says
 * `type: tool` and its values carry a `text` to be called by, a `description`,
 * and `arguments`. Each becomes a `ToolDef` spliced alongside the default tools.
 *
 * Execution is not implemented yet — the eventual Lua handlers are mocked, so
 * calling a user-defined tool throws. If `@tools` is absent or childless this
 * returns `[]`.
 */
export async function loadUserTools(
  pensive: Pensive,
  _opts: UserToolOptions = {}
): Promise<ToolDef[]> {
  const wrapper = entityWrapper(pensive)
  const root = (await wrapper.readEntities([TOOLS_ENTITY_ID])).get(TOOLS_ENTITY_ID)
  const childIds = root?.outboundLinks ?? []
  if (childIds.length === 0) return []

  const children = await wrapper.readEntities(childIds)
  const tools: ToolDef[] = []
  const seen = new Set<string>()

  for (const id of childIds) {
    const entity = children.get(id)
    if (!entity) continue
    const { id: declaredId, text: name, description, arguments: args, safety } = entity.values
    if (entity.values.type !== TOOL_ID) continue
    if (typeof name !== 'string' || typeof description !== 'string' || args == null) continue
    // The id a definition asks to be called by, with the name standing in when it
    // doesn't ask — the same rule the client reads, so a tool answers to one word
    // in a script and to the same word over MCP.
    const toolId = typeof declaredId === 'string' && declaredId ? declaredId : name
    if (seen.has(toolId)) continue
    seen.add(toolId)

    tools.push({
      id: toolId,
      name,
      description,
      safety: isSafety(safety) ? safety : 'dangerous',
      args: z.any(),
      // What a definition writes is a list of arguments, which is not a schema —
      // and this one is published verbatim, so it is converted rather than passed
      // on. A definition that already holds a schema comes through untouched.
      jsonSchema: toolArgumentsSchema(args),
      handler: async () => {
        throw new Error(`user-defined tool "${toolId}" is not yet executable`)
      },
    })
  }

  return tools
}
