import { z } from 'zod'
import { entityWrapper } from './defaultTools'
import type { Permissions } from './permissions'
import { SAFETY_RANK, type Safety, type ToolDef } from './types'

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
 * `@tools` has one direct child per tool; a child is tool-shaped when its
 * values carry `name`, `description`, and `arguments` (a JSON Schema for the
 * arguments). Each becomes a `ToolDef` spliced alongside the default tools.
 *
 * Execution is not implemented yet — the eventual Lua handlers are mocked, so
 * calling a user-defined tool throws. If `@tools` is absent or childless this
 * returns `[]`.
 */
export async function loadUserTools(
  perms: Permissions,
  _opts: UserToolOptions = {}
): Promise<ToolDef[]> {
  const wrapper = entityWrapper(perms)
  const root = (await wrapper.readEntities([TOOLS_ENTITY_ID])).get(TOOLS_ENTITY_ID)
  const childIds = root?.outboundLinks ?? []
  if (childIds.length === 0) return []

  const children = await wrapper.readEntities(childIds)
  const tools: ToolDef[] = []
  const seen = new Set<string>()

  for (const id of childIds) {
    const entity = children.get(id)
    if (!entity) continue
    const { name, description, arguments: args, safety } = entity.values
    if (typeof name !== 'string' || typeof description !== 'string' || args == null) continue
    if (seen.has(name)) continue
    seen.add(name)

    tools.push({
      id: name,
      name,
      description,
      safety: isSafety(safety) ? safety : 'dangerous',
      args: z.any(),
      jsonSchema: args as Record<string, unknown>,
      handler: async () => {
        throw new Error(`user-defined tool "${name}" is not yet executable`)
      },
    })
  }

  return tools
}
