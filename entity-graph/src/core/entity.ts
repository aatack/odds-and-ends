import type { AppEvent } from './events'

// What an entity *is*, and how events become one. Deliberately free of
// dependencies — no store, no uuid, no browser — because every client rolls up
// the same way the server does, or the two disagree about what is on screen.

export interface Entity {
  id: string
  createdAt: number
  editedAt: number
  createdBy: string
  editedBy: string
  values: Record<string, unknown>
  inboundLinks: string[]
  /** Ordered — this is the child order an outline reads top to bottom. */
  outboundLinks: string[]
}

/**
 * Which way a traversal follows links. `out` walks outbound links — the ordinary
 * reading of the graph as a tree of children. `in` walks inbound ones, so the
 * same query answers "what links to this?" and the tree grows towards the
 * entities that reference the root rather than away from it.
 */
export type LinkDirection = 'out' | 'in'

/**
 * Fold an entity's events into its current state. The events need not be sorted;
 * they are sorted here, since order is what a rollup *is*.
 */
export function rollupEntity(id: string, events: AppEvent[]): Entity {
  const sorted = [...events].sort((a, b) => a.timestamp - b.timestamp)

  let createdAt = Infinity
  let editedAt = -Infinity
  let createdBy = ''
  let editedBy = ''
  const values: Record<string, unknown> = {}
  const outbound: string[] = []
  // sourceId → currently active?
  const inboundState = new Map<string, boolean>()

  for (const e of sorted) {
    if (e.timestamp < createdAt) { createdAt = e.timestamp; createdBy = e.author }
    if (e.timestamp > editedAt)  { editedAt  = e.timestamp; editedBy  = e.author }

    if (e.type === 'value') {
      values[e.key] = e.value
    } else {
      if (e.sourceId === id) {
        const dest = e.destinationId
        const idx  = outbound.indexOf(dest)
        if (e.action === 0) {
          if (idx === -1) outbound.push(dest)
        } else if (e.action === 1) {
          if (idx !== -1) outbound.splice(idx, 1)
        } else if (e.action === 2) {
          // move forward (toward index 0) by one position
          if (idx > 0) { outbound.splice(idx, 1); outbound.splice(idx - 1, 0, dest) }
        } else if (e.action === 3) {
          // move backward (toward end) by one position
          if (idx !== -1 && idx < outbound.length - 1) {
            outbound.splice(idx, 1); outbound.splice(idx + 1, 0, dest)
          }
        }
      }
      if (e.destinationId === id) {
        if (e.action === 0) inboundState.set(e.sourceId, true)
        else if (e.action === 1) inboundState.set(e.sourceId, false)
      }
    }
  }

  return {
    id,
    createdAt: isFinite(createdAt) ? createdAt : Date.now(),
    editedAt:  isFinite(editedAt)  ? editedAt  : Date.now(),
    createdBy,
    editedBy,
    values,
    outboundLinks: outbound,
    inboundLinks:  [...inboundState.entries()].filter(([, v]) => v).map(([k]) => k),
  }
}

/**
 * Split a flat event list into the per-entity buckets a rollup takes. A value
 * event lands under its `entityId`; a link event under both endpoints, since it
 * is part of what each of them is.
 *
 * Only the ids asked for get a bucket, so this doubles as the filter a rollup
 * needs: one scan covers many entities, and folding all of its events into one of
 * them would give that entity every value in the batch.
 */
export function bucketEvents(ids: readonly string[], events: AppEvent[]): Map<string, AppEvent[]> {
  const map = new Map<string, AppEvent[]>()
  for (const id of ids) map.set(id, [])
  for (const e of events) {
    if (e.type === 'value') {
      map.get(e.entityId)?.push(e)
    } else {
      map.get(e.sourceId)?.push(e)
      if (e.destinationId !== e.sourceId) map.get(e.destinationId)?.push(e)
    }
  }
  return map
}

/** An entity nothing is known about: present, empty, and safe to render. */
export const emptyEntity = (id: string): Entity => ({
  id,
  createdAt: 0,
  editedAt: 0,
  createdBy: '',
  editedBy: '',
  values: {},
  inboundLinks: [],
  outboundLinks: [],
})

// --- What an entity says about itself in passing ----------------------------

/**
 * The little that is known about an entity away from its own row: enough to name
 * it in a tab or a breadcrumb, and enough for a pill to know what shape it should
 * be. The same three values a row carries, which is not a coincidence — a row is
 * this plus where it sits.
 */
export interface EntitySummary {
  text?: string
  /** The entity's `type` value, if any (e.g. `'code'` for a runnable block). */
  type?: string
  /** For `type: 'file'`, what the stored bytes are. */
  mimeType?: string
}

/** A value as display text — absent when null or blank, so `??` reaches past it. */
export const str = (v: unknown): string | undefined =>
  v == null || v === '' ? undefined : String(v)

/** What a set of entity values says about the entity in passing. */
export const summaryOf = (values: Record<string, unknown>): EntitySummary => ({
  text: str(values.text),
  type: str(values.type),
  mimeType: str(values.mimeType),
})
