import type {
  AppEvent,
  Entity,
  LinkDirection,
  QueryPage,
  ResourceRecord,
  StackFrame,
} from '../core/types'
import { uuid } from '../helpers/uuid'
import { callSource, currentUser } from './connection'

// Typed wrappers over the source's tools. Thin, except where a structural edit
// needs several events to land together — see `writeEvents` below.

export const query = (
  rootId: string,
  opts: {
    maxDepth?: number
    collapsed?: string[]
    limit?: number
    continuationStack?: StackFrame[]
    direction?: LinkDirection
  } = {},
): Promise<QueryPage> => callSource('query', { rootId, ...opts }) as Promise<QueryPage>

export const readEntities = (entityIds: string[]): Promise<Record<string, Entity>> =>
  callSource('readEntities', { entityIds }) as Promise<Record<string, Entity>>

export const writeValue = (entityId: string, key: string, value: unknown): Promise<unknown> =>
  callSource('writeValue', { entityId, key, value, author: currentUser() })

/** Link actions, as the `writeLink` tool numbers them. */
export const LINK_ADD = 0
export const LINK_REMOVE = 1
export const LINK_FORWARD = 2

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

/** Append raw events verbatim, keeping the timestamps and authors they carry. */
export const writeEvents = (events: AppEvent[]): Promise<unknown> =>
  callSource('writeEvents', { events })

/**
 * Take the most recent event, and any within `windowMs` of it, back off the store
 * and return them. Absent on a source that can't remove events, which is how the
 * client knows undo is unavailable.
 */
export const popEvents = (windowMs = 100): Promise<AppEvent[]> =>
  callSource('popEvents', { windowMs }) as Promise<AppEvent[]>

export const readResource = (id: string): Promise<ResourceRecord | null> =>
  callSource('readResource', { id }) as Promise<ResourceRecord | null>

// ---------------------------------------------------------------------------
// Structural edits
// ---------------------------------------------------------------------------

/**
 * How many "move toward index 0" events put a freshly appended child directly
 * after `afterId` among `siblings` (the parent's child order *before* the
 * insert). Zero when the new child belongs at the end, or when the row it should
 * follow isn't among them after all.
 *
 * This is what makes "add another" on a phone put the next line where the eye
 * expects it rather than at the bottom of a long list. Outbound links are the
 * only ordered thing in the store, and the only way to reorder them is one step
 * at a time — so the steps are computed here and written as one batch.
 */
export function forwardSteps(siblings: readonly string[], afterId: string | null): number {
  if (afterId == null) return 0
  const at = siblings.indexOf(afterId)
  if (at < 0) return 0
  return siblings.length - at - 1
}

const now = (): number => Date.now()

/** The events that move an already-linked child forward `steps` places. */
const moves = (parentId: string, childId: string, steps: number, at: number): AppEvent[] =>
  Array.from({ length: Math.max(0, steps) }, () => ({
    type: 'link' as const,
    timestamp: at,
    author: currentUser(),
    sourceId: parentId,
    destinationId: childId,
    action: LINK_FORWARD as 2,
  }))

/**
 * Create an entity and put it in its place, in one call.
 *
 * One call rather than the source's own `createEntity` followed by reordering,
 * for two reasons: a phone on mobile data feels every round trip, and undo takes
 * off "the most recent event and anything within 100ms of it" — so the values,
 * the link and the reordering have to be written at one instant if a single
 * back-press is to undo the whole thing rather than half of it.
 *
 * `after` names the sibling the new entity should follow; null appends it.
 * `siblings` is the parent's child order as the caller last saw it — the rows on
 * screen, which is the order the user is looking at.
 */
export async function createEntity(
  values: Record<string, unknown>,
  parentId: string | null,
  position: { siblings: readonly string[]; after: string | null } = { siblings: [], after: null },
): Promise<string> {
  const id = uuid()
  const at = now()
  const author = currentUser()
  const events: AppEvent[] = Object.entries(values).map(([key, value]) => ({
    type: 'value',
    timestamp: at,
    author,
    entityId: id,
    key,
    value,
  }))
  if (parentId) {
    events.push({
      type: 'link',
      timestamp: at,
      author,
      sourceId: parentId,
      destinationId: id,
      action: LINK_ADD,
    })
    events.push(...moves(parentId, id, forwardSteps(position.siblings, position.after), at))
  }
  await writeEvents(events)
  return id
}

/**
 * Re-parent an entity and place it among its new siblings — the move behind
 * indent, outdent and "move to…". Again one batch, so one undo takes it back.
 *
 * `siblings` is the *destination* parent's child order before the move.
 */
export async function reparent(
  entityId: string,
  fromParentId: string,
  toParentId: string,
  position: { siblings: readonly string[]; after: string | null } = { siblings: [], after: null },
): Promise<void> {
  const at = now()
  const author = currentUser()
  const events: AppEvent[] = [
    {
      type: 'link',
      timestamp: at,
      author,
      sourceId: fromParentId,
      destinationId: entityId,
      action: LINK_REMOVE,
    },
    {
      type: 'link',
      timestamp: at,
      author,
      sourceId: toParentId,
      destinationId: entityId,
      action: LINK_ADD,
    },
  ]
  // The entity may already be a child of the destination (an outdent inside the
  // same parent, say), in which case re-adding it is a no-op and it keeps the
  // index it had. Excluding it from the sibling order handles both cases.
  const siblings = position.siblings.filter((s) => s !== entityId)
  events.push(...moves(toParentId, entityId, forwardSteps(siblings, position.after), at))
  await writeEvents(events)
}

/** Nudge a child one place up or down among its siblings. */
export const nudge = (parentId: string, childId: string, direction: 'up' | 'down'): Promise<unknown> =>
  writeLink(parentId, childId, direction === 'up' ? 2 : 3)
