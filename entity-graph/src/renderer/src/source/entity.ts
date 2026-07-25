import type { AppEvent } from '../../../core/events'
import type { QueryPage, StackFrame } from '../../../core/wrapper'
import { callSource, currentUser } from './transport'

// Typed wrappers over the source's tools. Thin by design: the argument shapes
// live in core/source/defaultTools.ts and this is just the calling convention,
// including the author stamped onto writes.

export const query = (
  rootId: string,
  opts: { maxDepth?: number; collapsed?: string[]; limit?: number; continuationStack?: StackFrame[] } = {},
): Promise<QueryPage> => callSource('query', { rootId, ...opts }) as Promise<QueryPage>

export const writeValue = (entityId: string, key: string, value: unknown): Promise<unknown> =>
  callSource('writeValue', { entityId, key, value, author: currentUser() })

// The tool stamps its own author on the events it writes, so unlike writeValue
// this one takes none.
export const createEntity = (
  values: Record<string, unknown>,
  parentId?: string,
): Promise<string> => callSource('createEntity', { values, parentId }) as Promise<string>

export const moveEntity = (
  entityId: string,
  fromParentId: string,
  toParentId: string,
): Promise<unknown> => callSource('moveEntity', { entityId, fromParentId, toParentId })

/** Link actions, as the `writeLink` tool numbers them. */
export const LINK_ADD = 0
export const LINK_REMOVE = 1

export const writeLink = (
  sourceId: string,
  destinationId: string,
  action: number,
): Promise<unknown> =>
  callSource('writeLink', { sourceId, destinationId, action, author: currentUser() })

export const link = (sourceId: string, destinationId: string): Promise<unknown> =>
  writeLink(sourceId, destinationId, LINK_ADD)

export const unlink = (parentId: string, childId: string): Promise<unknown> =>
  writeLink(parentId, childId, LINK_REMOVE)

// --- Raw events (undo / redo) ----------------------------------------------

/**
 * Take the most recent event, and any within `windowMs` of it, back off the
 * store. Returns what was removed, oldest first.
 */
export const popEvents = (windowMs = 100): Promise<AppEvent[]> =>
  callSource('popEvents', { windowMs }) as Promise<AppEvent[]>

/** Put events back verbatim, timestamps and authors intact. */
export const writeEvents = (events: AppEvent[]): Promise<unknown> =>
  callSource('writeEvents', { events })
