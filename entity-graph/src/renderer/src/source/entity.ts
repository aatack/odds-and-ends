import type { AppEvent, LinkAction } from '../../../core/events'
import type { EventScan } from '../../../core/source/defaultTools'
import type { ResourceRecord } from '../../../core/source/permissions'
import { callSource, currentUser } from './transport'

// Typed wrappers over the source's tools. Thin by design: the argument shapes
// live in core/source/defaultTools.ts and this is just the calling convention,
// including the author stamped onto writes.

/**
 * The app's one read of the store: events for a list of ids, plus a couple of
 * layers of whatever they link to, since the client is almost always about to
 * walk down. Everything shown is rolled up from what this brings back — there is
 * no server-side query behind any of it.
 */
export const scanEvents = (entityIds: string[]): Promise<EventScan> =>
  callSource('scanEvents', { entityIds }) as Promise<EventScan>

// --- Writes -----------------------------------------------------------------

/**
 * A write is announced here before it is sent, so the cache can show it at once
 * and the round trip can happen behind it. The events are the ones the store
 * will hold, timestamps and authors included, so what is shown now and what
 * comes back later are the same events — not an optimistic guess to reconcile.
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
  // Everything but the type, which is the tool's rather than the event's. The
  // timestamp and author are named explicitly so the event written here and the
  // one the store keeps are the same event.
  const written = {
    entityId,
    key,
    // The tool spends null on "no value", and so does the rollup.
    value: value ?? null,
    author: currentUser(),
    timestamp: Date.now(),
  }
  return write([{ type: 'value', ...written }], () => callSource('writeValue', written))
}

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

// --- Resources (pasted images and other blobs) ------------------------------

/**
 * Store bytes under an entity id. The entity describing them — conventionally
 * `type: 'file'` — is the key, so there is no reference to keep in step.
 */
export const writeResource = (
  id: string,
  mimeType: string,
  data: string,
  name: string | null = null,
): Promise<unknown> =>
  callSource('writeResource', { id, mimeType, data, name, author: currentUser() })

/** Null when the source has nothing stored under that id. */
export const readResource = (id: string): Promise<ResourceRecord | null> =>
  callSource('readResource', { id }) as Promise<ResourceRecord | null>

// --- Raw events (undo / redo) ----------------------------------------------

/**
 * Take the most recent event, and any within `windowMs` of it, back off the
 * store. Returns what was removed, oldest first — empty once the last write is
 * older than the store's age limit (`POP_AGE_LIMIT_MS`), which nothing here can
 * override.
 *
 * What came off is taken out of the cache too, which is all undo needs to do to
 * the view: the entities those events belonged to roll up again without them.
 */
export async function popEvents(windowMs = 100): Promise<AppEvent[]> {
  const events = (await callSource('popEvents', { windowMs })) as AppEvent[]
  observer?.removed(events)
  return events
}

/** Put events back verbatim, timestamps and authors intact. */
export const writeEvents = (events: AppEvent[]): Promise<unknown> =>
  write(events, () => callSource('writeEvents', { events }))
