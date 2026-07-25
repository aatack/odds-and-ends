import { APP_TOOLS, GROUP_TOOLS } from './appTools'
import { ENTITY_TOOLS } from './entityTools'
import { FRAME_TOOLS } from './frameTools'
import { UNDO_TOOLS } from './undoTools'
import type { ToolScope, ToolSpec } from './types'

// The one registry. Order matters twice: it's the order the palette lists tools
// in with no search text (entity tools first, since they act on what the user is
// looking at), and the order the key router resolves collisions in within a
// scope.

export const TOOLS: ToolSpec[] = [
  ...ENTITY_TOOLS,
  ...FRAME_TOOLS,
  ...GROUP_TOOLS,
  ...UNDO_TOOLS,
  ...APP_TOOLS,
]

const byId = new Map(TOOLS.map((t) => [t.id, t]))

export const findTool = (id: string): ToolSpec | undefined => byId.get(id)

/** Tools offered in the palette's list. */
export const listedTools = (): ToolSpec[] => TOOLS.filter((t) => t.listed !== false)

export const toolsInScope = (scope: ToolScope): ToolSpec[] => TOOLS.filter((t) => t.scope === scope)
