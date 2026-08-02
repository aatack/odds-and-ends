import type { Entity } from '../../../core/entity'
import type { AppEvent, LinkAction } from '../../../core/events'
import { outlineMarkdown } from '../../../core/markdown'
import type { QueryPage } from '../../../core/query'
import type { EventScan } from '../../../core/source/defaultTools'
import type { ResourceRecord } from '../../../core/source/permissions'
import { rowsOfPage } from '../../../core/tree'
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

/**
 * Entities rolled up by the store, exactly the ones asked for. The other read —
 * and pointedly not the one everything on screen goes through, which answers from
 * the cache at once and improves as events land.
 *
 * This is for the callers that have nowhere to put "not yet": a script, and the
 * tool loader. They ask, they wait, and what comes back is what the store holds.
 *
 * The rollup is the store's own, so it is the events and nothing else: no type
 * defaults laid in behind, and no `events` script run over the top. Both of those
 * are the cache's doing, and this does not go through it.
 */
export const readEntities = (entityIds: string[]): Promise<Record<string, Entity>> =>
  callSource('readEntities', { entityIds }) as Promise<Record<string, Entity>>

/** How much of a subtree one {@link readOutline} takes, absent a reason to differ. */
const OUTLINE_LIMIT = 400

/**
 * A subtree as markdown, walked by the *store* rather than by the cache.
 *
 * The export in `frameTools` reads the rows a frame is drawing, which is the
 * right answer for "copy what I am looking at" and the wrong one for everything
 * else: it can only reach a tree that is on screen, it honours folding, and it
 * has nothing to say about a branch nobody has expanded. This asks the store for
 * the walk and renders what comes back, so it works on any id at all — including
 * one whose entities have never been read.
 *
 * One page. A subtree that outruns the limit comes back truncated rather than
 * paged, because both callers are composing a prompt: the rules an agent is
 * given, and the description of a pull request. Neither is improved by being
 * unbounded, and a set of notes that long has a shape problem the paging would
 * only hide.
 */
export async function readOutline(entityId: string, limit = OUTLINE_LIMIT): Promise<string> {
  const page = (await callSource('query', { path: entityId, limit })) as QueryPage
  return outlineMarkdown(rowsOfPage(page.rows))
}

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
  // The event, less the discriminator the tool's name already carries. Both the
  // timestamp and the author are named rather than left to the server, so the
  // event shown now and the event the store keeps are the same event.
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
// this one takes none — and the id is the server's, so there is nothing to show
// ahead of the answer either. These two land in the cache with the refresh that
// follows any write, rather than on their way out.
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
