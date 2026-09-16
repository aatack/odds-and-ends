import { bucketEvents, rollupEntity } from '../../core/entity'
import type { AppEvent } from '../../core/events'
import type { Pensive } from '../../core/pensive/index'

// The one way a feed puts something in the store, and the reason the catch-up
// rules are allowed to be as blunt as they are.
//
// **An entity's id is made from the thing's own id**, so reading the same
// message, pull request or comment a second time writes to the same entity
// rather than to a new one. Everything else follows from that: a cursor may be
// wound back, a window may overlap, a socket and a poll may both deliver the
// same message, and none of it shows — the second write has nothing to say,
// because it compares what it is about to write with what is there and writes
// only the difference.
//
// So "run it twice over the same period and nothing changes" is not a property
// to be careful about. It is what this file is.

/** Where a note nobody has sorted yet goes. */
export const INBOX_ID = '@inbox'

/** One entity as a feed wants it to read, once it has been written. */
export interface EntityDraft {
  /** Made from the thing's own id, and the same every time it is read. */
  id: string
  /**
   * The values it should hold. A key whose value is `undefined` is left alone —
   * "I did not find this out" is not the same as "this is empty", and a poll
   * that knows less than the socket did must not erase what the socket wrote.
   */
  values: Record<string, unknown>
  /** Where it hangs in the outline: the thread, the channel, the pull request. */
  parentId?: string
}

/** What one batch actually changed, which is what a feed reports it did. */
export interface WriteReport {
  /** Entities that had nothing written to them before this. */
  created: number
  /** Entities something was written to, new or not. */
  touched: number
  events: number
}

/**
 * Two values are the same when they read the same. Everything a feed writes is
 * built here from a JSON response, so key order is ours and stable; this is
 * comparing what we are about to write with what we wrote last time, not with
 * something a person typed.
 */
const same = (a: unknown, b: unknown): boolean => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)

export class EntityWriter {
  constructor(
    private pensive: Pensive,
    /** Recorded on every event, so history says which feed wrote it. */
    private author: string,
  ) {}

  /**
   * Write a batch, and hand back what changed. One read and at most one write,
   * whatever the batch holds: the drafts are looked up together, the difference
   * is worked out in memory, and the events go in as one action.
   */
  async write(drafts: EntityDraft[]): Promise<WriteReport> {
    if (!drafts.length) return { created: 0, touched: 0, events: 0 }

    // A draft named twice in one batch — an edit arriving beside the message it
    // edits — is folded, later winning, so the read below sees each id once.
    const merged = new Map<string, EntityDraft>()
    for (const draft of drafts) {
      const before = merged.get(draft.id)
      merged.set(draft.id, {
        id: draft.id,
        values: { ...before?.values, ...draft.values },
        parentId: draft.parentId ?? before?.parentId,
      })
    }

    const ids = [...merged.keys()]
    const buckets = bucketEvents(ids, await this.pensive.readEvents(ids))
    const timestamp = Date.now()
    const events: AppEvent[] = []
    let created = 0
    let touched = 0

    for (const draft of merged.values()) {
      const existing = rollupEntity(draft.id, buckets.get(draft.id) ?? [])
      // Nothing has ever been written here, so this is the first reading of it,
      // and only then does it go in the inbox: a note filed somewhere by hand
      // must not be dragged back the next time the thing it names is mentioned.
      // Values rather than events, so an entity that exists only because a reply
      // was hung off it still counts as unread when the message itself turns up.
      const fresh = Object.keys(existing.values).length === 0
      const before = events.length

      for (const [key, value] of Object.entries(draft.values)) {
        if (value === undefined) continue
        if (same(existing.values[key], value)) continue
        events.push({ type: 'value', entityId: draft.id, key, value, author: this.author, timestamp })
      }

      const parents = new Set(existing.inboundLinks)
      const link = (parentId: string): void => {
        if (parents.has(parentId)) return
        parents.add(parentId)
        events.push({
          type: 'link',
          sourceId: parentId,
          destinationId: draft.id,
          action: 0,
          author: this.author,
          timestamp,
        })
      }
      if (draft.parentId) link(draft.parentId)
      if (fresh) link(INBOX_ID)

      if (fresh) created++
      if (events.length > before) touched++
    }

    if (events.length) await this.pensive.writeEvents(events)
    return { created, touched, events: events.length }
  }
}
