import { v4 as uuidv4 } from 'uuid'
import type { AppEvent } from './events'
import type { EntityInterface } from './interface/index'

export interface Entity {
  id: string
  createdAt: number
  editedAt: number
  createdBy: string
  editedBy: string
  values: Record<string, unknown>
  inboundLinks: string[]
  outboundLinks: string[] // ordered
}

export interface QueryResult {
  entity: Entity
  depth: number
  parentId: string | null
}

export interface QueryPage {
  results: QueryResult[]
  /** Non-null when the limit was reached; pass as `resumePath` to continue. */
  continuationPath: string[] | null
}

// ---------------------------------------------------------------------------
// Internal rollup
// ---------------------------------------------------------------------------

function rollupEntity(id: string, events: AppEvent[]): Entity {
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

// ---------------------------------------------------------------------------
// Wrapper
// ---------------------------------------------------------------------------

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

  /**
   * Depth-first traversal from rootId.
   * Avoids cycles by tracking the current path (not globally visited, so the
   * same entity can appear in different branches — it just can't be its own
   * ancestor in a given path).
   *
   * If limit is reached the continuationPath contains the ancestry path to
   * the frontier node so the caller can restart from there.
   */
  async resolveQuery(
    rootId: string,
    options: {
      maxDepth?: number
      collapsed?: string[]
      limit?: number
      resumePath?: string[]
    } = {},
  ): Promise<QueryPage> {
    const { maxDepth, collapsed = [], limit = 1000 } = options
    const collapsedSet = new Set(collapsed)
    const results: QueryResult[] = []

    type Frame = { id: string; depth: number; parentId: string | null; path: string[] }
    const stack: Frame[] = [{ id: rootId, depth: 0, parentId: null, path: [rootId] }]

    while (stack.length > 0) {
      if (results.length >= limit) {
        const top = stack[stack.length - 1]
        return { results, continuationPath: top.path }
      }

      const { id, depth, parentId, path } = stack.pop()!

      const entityMap = await this.readEntities([id])
      const entity = entityMap.get(id)
      if (!entity) continue

      results.push({ entity, depth, parentId })

      const shouldExpand = !collapsedSet.has(id) && (maxDepth === undefined || depth < maxDepth)
      if (shouldExpand) {
        // Push in reverse so the first outbound link is processed first (DFS left-to-right)
        for (const childId of [...entity.outboundLinks].reverse()) {
          if (!path.includes(childId)) {
            stack.push({ id: childId, depth: depth + 1, parentId: id, path: [...path, childId] })
          }
        }
      }
    }

    return { results, continuationPath: null }
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
