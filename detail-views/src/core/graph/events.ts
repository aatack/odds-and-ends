/**
 * Everything the app knows is a list of these. An entity is whatever its
 * events roll up to (`entity.ts`), read from both stores at once: what I wrote
 * and what was fetched, sorted by timestamp, so the later one wins.
 *
 * Shared with the renderer: nothing here may import node, Electron or React.
 */

/** 0 = add, 1 = remove, 2 = move forward (toward index 0), 3 = move backward. */
export type LinkAction = 0 | 1 | 2 | 3

interface BaseEvent {
  /** Unix ms. For fetched data, when it happened elsewhere; 0 for what has no date. */
  timestamp: number
  author: string
}

export interface ValueEvent extends BaseEvent {
  type: 'value'
  entityId: string
  key: string
  value: unknown
}

export interface LinkEvent extends BaseEvent {
  type: 'link'
  sourceId: string
  destinationId: string
  action: LinkAction
}

export type AppEvent = ValueEvent | LinkEvent

/** Complete events for a set of ids: what one read of the stores hands back. */
export interface Scan {
  /** Every entity whose events are complete in `events`. */
  entityIds: string[]
  events: AppEvent[]
}

/** Which ids changed, or null for "anything may have". */
export type Changed = string[] | null

/** Events carry no id, so equality is what identifies one. */
export function eventKey(e: AppEvent): string {
  return e.type === 'value'
    ? ['v', e.entityId, e.key, e.timestamp, e.author, JSON.stringify(e.value ?? null)].join(' ')
    : ['l', e.sourceId, e.destinationId, e.action, e.timestamp, e.author].join(' ')
}

/** Which entities an event belongs to: one for a value, both ends for a link. */
export function byEntity(events: readonly AppEvent[]): Map<string, AppEvent[]> {
  const out = new Map<string, AppEvent[]>()
  const push = (id: string, e: AppEvent): void => {
    const list = out.get(id)
    if (list) list.push(e)
    else out.set(id, [e])
  }
  for (const e of events) {
    if (e.type === 'value') push(e.entityId, e)
    else {
      push(e.sourceId, e)
      if (e.destinationId !== e.sourceId) push(e.destinationId, e)
    }
  }
  return out
}

export const value = (entityId: string, key: string, v: unknown, timestamp: number, author: string): ValueEvent => ({
  type: 'value',
  entityId,
  key,
  value: v ?? null,
  timestamp,
  author,
})

export const link = (
  sourceId: string,
  destinationId: string,
  timestamp: number,
  author: string,
  action: LinkAction = 0,
): LinkEvent => ({ type: 'link', sourceId, destinationId, action, timestamp, author })

/** Every field of `values` as a value event on `entityId`, all at one time. */
export function values(entityId: string, fields: Record<string, unknown>, timestamp: number, author: string): ValueEvent[] {
  return Object.entries(fields)
    .filter(([, v]) => v !== undefined)
    .map(([key, v]) => value(entityId, key, v, timestamp, author))
}
