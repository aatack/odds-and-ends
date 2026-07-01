import type { AppEvent } from '../events'
import type { EntityInterface } from './index'

/**
 * Merges reads from all sources; writes go only to the first source.
 */
export class ComboInterface implements EntityInterface {
  constructor(private sources: EntityInterface[]) {
    if (sources.length === 0) throw new Error('ComboInterface requires at least one source')
  }

  async readEvents(entityIds: string[]): Promise<Map<string, AppEvent[]>> {
    const merged = new Map<string, AppEvent[]>()
    for (const id of entityIds) merged.set(id, [])

    const results = await Promise.all(this.sources.map((s) => s.readEvents(entityIds)))

    for (const result of results) {
      for (const [id, events] of result) {
        const bucket = merged.get(id)
        if (bucket) bucket.push(...events)
      }
    }

    return merged
  }

  async writeEvents(events: AppEvent[]): Promise<void> {
    await this.sources[0].writeEvents(events)
  }
}
