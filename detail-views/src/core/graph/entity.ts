import type { AppEvent } from './events.ts'

/** What a set of events rolls up to. Shared with the renderer. */
export interface GraphEntity {
  id: string
  createdAt: number
  editedAt: number
  values: Record<string, unknown>
  /** Ordered: the order children are read in. */
  outboundLinks: string[]
  inboundLinks: string[]
}

/**
 * Fold an entity's events into its current state. Sorted here, stably, so
 * events sharing a timestamp keep the order they were handed over in: the
 * stores hand over fetched events before mine, so on a tie mine win.
 */
export function rollupEntity(id: string, events: readonly AppEvent[]): GraphEntity {
  const sorted = [...events].sort((a, b) => a.timestamp - b.timestamp)
  let createdAt = Infinity
  let editedAt = -Infinity
  const values: Record<string, unknown> = {}
  const outbound: string[] = []
  const inbound = new Map<string, boolean>()

  for (const e of sorted) {
    createdAt = Math.min(createdAt, e.timestamp)
    editedAt = Math.max(editedAt, e.timestamp)
    if (e.type === 'value') {
      if (e.entityId === id) values[e.key] = e.value
      continue
    }
    if (e.sourceId === id) {
      const at = outbound.indexOf(e.destinationId)
      if (e.action === 0) {
        if (at === -1) outbound.push(e.destinationId)
      } else if (e.action === 1) {
        if (at !== -1) outbound.splice(at, 1)
      } else if (e.action === 2) {
        if (at > 0) outbound.splice(at - 1, 0, ...outbound.splice(at, 1))
      } else if (e.action === 3) {
        if (at !== -1 && at < outbound.length - 1) outbound.splice(at + 1, 0, ...outbound.splice(at, 1))
      }
    }
    if (e.destinationId === id) {
      if (e.action === 0) inbound.set(e.sourceId, true)
      else if (e.action === 1) inbound.set(e.sourceId, false)
    }
  }

  return {
    id,
    createdAt: Number.isFinite(createdAt) ? createdAt : 0,
    editedAt: Number.isFinite(editedAt) ? editedAt : 0,
    values,
    outboundLinks: outbound,
    inboundLinks: [...inbound].filter(([, active]) => active).map(([source]) => source),
  }
}

/**
 * Split a flat event list into per-entity buckets. Only the ids asked for get
 * one, so a scan covering many entities rolls each up from its own events.
 */
export function bucketEvents(ids: readonly string[], events: readonly AppEvent[]): Map<string, AppEvent[]> {
  const map = new Map<string, AppEvent[]>()
  for (const id of ids) map.set(id, [])
  for (const e of events) {
    if (e.type === 'value') map.get(e.entityId)?.push(e)
    else {
      map.get(e.sourceId)?.push(e)
      if (e.destinationId !== e.sourceId) map.get(e.destinationId)?.push(e)
    }
  }
  return map
}

/** An entity nothing is known about: present, empty, and safe to render. */
export const emptyEntity = (id: string): GraphEntity => ({
  id,
  createdAt: 0,
  editedAt: 0,
  values: {},
  outboundLinks: [],
  inboundLinks: [],
})
