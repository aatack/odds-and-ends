import type { EventScan } from '../../../src/core/source/defaultTools'
import type { AppEvent, LinkAction, ResourceRecord } from '../core/types'
import { uuid } from '../helpers/uuid'
import { callSource, currentUser } from './connection'

// Typed wrappers over the source's tools. Thin, except where a structural edit
// needs several events to land together — see `writeEvents` below.

/**
 * The app's one read of the store: events for a list of ids, plus a couple of
 * layers of whatever they link to, since the client is almost always about to
 * walk down. Everything on screen is rolled up from what this brings back —
 * there is no server-side query behind any of it, which is what makes folding
 * and navigating instant on a connection that isn't.
 */
export const scanEvents = (entityIds: string[]): Promise<EventScan> =>
  callSource('scanEvents', { entityIds }) as Promise<EventScan>

/**
 * A write is announced here before it is sent, so the cache can show it at once
 * and the round trip can happen behind it — which on a phone is the difference
 * between typing and waiting. The events are the ones the store will hold,
 * timestamps and authors included, so what is shown now and what comes back
 * later are the same events rather than a guess to be reconciled.
 */
export interface WriteObserver {
  applied: (events: AppEvent[]) => void
  removed: (events: AppEvent[]) => void
}

let observer: WriteObserver | null = null

export const setWriteObserver = (next: WriteObserver | null): void => {
  observer = next
}

/** Send events the store will keep verbatim, showing them immediately. */
async function write(events: AppEvent[], send: () => Promise<unknown>): Promise<unknown> {
  observer?.applied(events)
  try {
    return await send()
  } catch (e) {
    // The store never took them, so neither should the cache.
    observer?.removed(events)
    throw e
  }
}

export function writeValue(entityId: string, key: string, value: unknown): Promise<unknown> {
  // The event, less the discriminator the tool's name already carries. Both the
  // timestamp and the author are named rather than left to the server, so the
  // event shown now and the event the store keeps are the same event.
  const written = {
    entityId,
    key,
    value: value ?? null,
    author: currentUser(),
    timestamp: Date.now(),
  }
  return write([{ type: 'value', ...written }], () => callSource('writeValue', written))
}

/** Link actions, as the `writeLink` tool numbers them. */
export const LINK_ADD = 0
export const LINK_REMOVE = 1
export const LINK_FORWARD = 2

export function writeLink(
  sourceId: string,
  destinationId: string,
  action: number,
): Promise<unknown> {
  const written = {
    sourceId,
    destinationId,
    action,
    author: currentUser(),
    timestamp: Date.now(),
  }
  return write([{ type: 'link', ...written, action: action as LinkAction }], () =>
    callSource('writeLink', written),
  )
}

export const link = (sourceId: string, destinationId: string): Promise<unknown> =>
  writeLink(sourceId, destinationId, LINK_ADD)

export const unlink = (parentId: string, childId: string): Promise<unknown> =>
  writeLink(parentId, childId, LINK_REMOVE)

/** Append raw events verbatim, keeping the timestamps and authors they carry. */
export const writeEvents = (events: AppEvent[]): Promise<unknown> =>
  write(events, () => callSource('writeEvents', { events }))

/**
 * Take the most recent event, and any within `windowMs` of it, back off the store
 * and return them. Absent on a source that can't remove events, which is how the
 * client knows undo is unavailable.
 *
 * What came off is taken out of the cache too, which is all undo needs to do to
 * the view: the entities those events belonged to roll up again without them.
 */
export async function popEvents(windowMs = 100): Promise<AppEvent[]> {
  const events = (await callSource('popEvents', { windowMs })) as AppEvent[]
  observer?.removed(events)
  return events
}

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
