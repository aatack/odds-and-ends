import type { AppEvent } from '../events'

export interface EntityInterface {
  /**
   * Returns a map of entityId → events affecting that entity.
   * Link events appear in the lists for both source and destination.
   */
  readEvents(entityIds: string[]): Promise<Map<string, AppEvent[]>>
  writeEvents(events: AppEvent[]): Promise<void>
}

/**
 * Optional extension: read every event in the store, deduplicated.
 * Backs the `readEvents(null)` tool ("dump everything") and event-level
 * wrappers (Combined, Frozen). Implementations that can't enumerate all
 * events may omit this.
 */
export interface DumpableInterface extends EntityInterface {
  readAllEvents(): Promise<AppEvent[]>
}
