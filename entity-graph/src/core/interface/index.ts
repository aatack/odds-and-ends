import type { AppEvent } from '../events'

export interface EntityInterface {
  /**
   * Returns a map of entityId → events affecting that entity.
   * Link events appear in the lists for both source and destination.
   */
  readEvents(entityIds: string[]): Promise<Map<string, AppEvent[]>>
  writeEvents(events: AppEvent[]): Promise<void>
}
