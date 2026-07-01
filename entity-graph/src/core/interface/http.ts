import type { AppEvent } from '../events'
import type { EntityInterface } from './index'

/**
 * REST backend. Expects a server that implements:
 *   GET  /events?entity=<id>&entity=<id>...  → { [entityId]: AppEvent[] }
 *   POST /events                              body: AppEvent[]
 */
export class HttpInterface implements EntityInterface {
  constructor(private baseUrl: string) {}

  async readEvents(entityIds: string[]): Promise<Map<string, AppEvent[]>> {
    if (entityIds.length === 0) return new Map()
    const params = new URLSearchParams()
    for (const id of entityIds) params.append('entity', id)

    const res = await fetch(`${this.baseUrl}/events?${params.toString()}`)
    if (!res.ok) throw new Error(`HTTP ${res.status}: ${await res.text()}`)

    const data = (await res.json()) as Record<string, AppEvent[]>
    return new Map(Object.entries(data))
  }

  async writeEvents(events: AppEvent[]): Promise<void> {
    const res = await fetch(`${this.baseUrl}/events`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(events),
    })
    if (!res.ok) throw new Error(`HTTP ${res.status}: ${await res.text()}`)
  }
}
