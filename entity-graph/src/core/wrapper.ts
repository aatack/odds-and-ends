import { v4 as uuidv4 } from 'uuid'
import type { AppEvent } from './events'
import { rollupEntity, type Entity } from './entity'
import type { EntityInterface } from './interface/index'

// The store-facing half of the entity model: reading entities out of a backing,
// and the two structural writes that need more than one event. The model itself
// — what an entity is, and how events fold into one — is in ./entity, which has
// no dependencies so that every client can share it.
//
// Note what is *not* here any more: the traversal. It lives in ./query, as a
// stepper over a synchronous `getEntities`, so the same walk runs on a server
// with a database behind it and on a client with a cache behind it.

export class EntityWrapper {
  constructor(
    private iface: EntityInterface,
    private getAuthor: () => string,
  ) {}

  async readEntities(ids: string[]): Promise<Map<string, Entity>> {
    const eventsMap = await this.iface.readEvents(ids)
    const out = new Map<string, Entity>()
    for (const [id, events] of eventsMap) {
      out.set(id, rollupEntity(id, events))
    }
    return out
  }

  /** Creates a new entity with the given values and optional parent link. Returns the new entity ID. */
  async createEntity(
    values: Record<string, unknown>,
    parentId?: string,
  ): Promise<string> {
    const id     = uuidv4()
    const now    = Date.now()
    const author = this.getAuthor()
    const events: AppEvent[] = []

    for (const [key, value] of Object.entries(values)) {
      events.push({ type: 'value', timestamp: now, author, entityId: id, key, value })
    }

    if (parentId !== undefined) {
      events.push({ type: 'link', timestamp: now, author, sourceId: parentId, destinationId: id, action: 0 })
    }

    await this.iface.writeEvents(events)
    return id
  }

  /** Removes the link from fromParentId → entityId and adds one from toParentId → entityId. */
  async moveEntity(entityId: string, fromParentId: string, toParentId: string): Promise<void> {
    const now    = Date.now()
    const author = this.getAuthor()
    await this.iface.writeEvents([
      { type: 'link', timestamp: now, author, sourceId: fromParentId, destinationId: entityId, action: 1 },
      { type: 'link', timestamp: now, author, sourceId: toParentId,   destinationId: entityId, action: 0 },
    ])
  }
}
