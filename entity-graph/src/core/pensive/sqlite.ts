import type { AppEvent } from '../events'
import { SqliteInterface } from '../interface/sqlite'
import { BasePensive } from './base'
import type { ResourceRecord } from './types'

/**
 * The base pensive: one SQLite file, holding events and resources.
 *
 * The whole of a store's configuration is a path and a name. Everything else a
 * pensive does — the tools, the query, the user's own tools — comes from
 * {@link BasePensive}, so this is the shortest an implementation gets.
 */
export class SqlitePensive extends BasePensive {
  private iface: SqliteInterface

  constructor(
    readonly id: string,
    readonly label: string,
    path: string,
    defaultAuthor?: string,
  ) {
    super()
    this.iface = new SqliteInterface(path)
    this.defaultAuthor = defaultAuthor
  }

  /**
   * Flat from the store rather than flattened from per-entity buckets: a link
   * belongs to both its ends, so collecting bucket by bucket pulls it forward to
   * wherever the first of those ends sits and silently reorders the links of the
   * other.
   */
  readEvents(entityIds?: string[]): Promise<AppEvent[]> {
    return entityIds === undefined
      ? this.iface.readAllEvents()
      : this.iface.readEventsFlat(entityIds)
  }

  writeEvents(events: AppEvent[]): Promise<void> {
    return this.iface.writeEvents(events)
  }

  popEvents(author?: string): Promise<AppEvent[]> {
    return this.iface.popLatestEvents(author)
  }

  readResource(id: string): Promise<ResourceRecord | null> {
    return this.iface.readResource(id)
  }

  writeResource(resource: ResourceRecord): Promise<void> {
    return this.iface.writeResource(resource)
  }

  close(): void {
    this.iface.close()
  }
}
