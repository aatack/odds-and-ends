import { APP_TOOLS, GROUP_TOOLS } from './appTools'
import { ENTITY_TOOLS } from './entityTools'
import { FRAME_TOOLS } from './frameTools'
import { integrationsAtom } from './integrationTools'
import { RESOURCE_TOOLS } from './resourceTools'
import { UNDO_TOOLS } from './undoTools'
import type { ToolScope, ToolSpec } from './types'

// The one registry. Order matters twice: it's the order the palette lists tools
// in with no search text (entity tools first, since they act on what the user is
// looking at), and the order the key router resolves collisions in within a
// scope.
//
// Most of it is fixed at build time. The integrations are not — they are
// declared on the server and arrive when a source is opened — so the list is a
// function rather than a constant, and the palette re-reads it when they land.

const BUILT_IN: ToolSpec[] = [
  ...ENTITY_TOOLS,
  ...FRAME_TOOLS,
  ...RESOURCE_TOOLS,
  ...GROUP_TOOLS,
  ...UNDO_TOOLS,
  ...APP_TOOLS,
]

/** Everything invocable right now. Integrations trail the built-ins. */
export const allTools = (): ToolSpec[] => [...BUILT_IN, ...integrationsAtom.get()]

const byId = new Map(BUILT_IN.map((t) => [t.id, t]))

export const findTool = (id: string): ToolSpec | undefined =>
  byId.get(id) ?? integrationsAtom.get().find((t) => t.id === id)

/** Tools offered in the palette's list. */
export const listedTools = (): ToolSpec[] => allTools().filter((t) => t.listed !== false)

export const toolsInScope = (scope: ToolScope): ToolSpec[] =>
  allTools().filter((t) => t.scope === scope)
