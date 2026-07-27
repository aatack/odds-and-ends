import { SqliteInterface } from '../interface/sqlite'
import { defaultTools } from './defaultTools'
import {
  dbPermissions,
  type EventBacking,
  type Permissions,
  type ResourceBacking,
} from './permissions'
import { loadUserTools } from './userTools'
import { ToolSource, type ToolDef } from './types'

/**
 * The base source: an event-sourced SQLite store. It grants the DB read/write
 * permissions (HTTP/CLI are stubbed) and exposes the default tools built on
 * them, plus any user-defined tools discovered under the `@tools` entity.
 */
export class SqliteSource extends ToolSource {
  private iface: SqliteInterface
  private perms: Permissions
  private defaultAuthor?: string
  private cached: ToolDef[]

  constructor(
    public id: string,
    public label: string,
    path: string,
    defaultAuthor?: string
  ) {
    super()
    this.iface = new SqliteInterface(path)
    this.defaultAuthor = defaultAuthor
    const backing: EventBacking & ResourceBacking = {
      // Flat from the store rather than flattened from per-entity buckets: a
      // link belongs to both its ends, so collecting bucket by bucket pulls it
      // forward to wherever the first of those ends sits and silently reorders
      // the links of the other.
      readEvents: (ids) => this.iface.readEventsFlat(ids),
      readAllEvents: () => this.iface.readAllEvents(),
      writeEvents: (events) => this.iface.writeEvents(events),
      popLatestEvents: (windowMs) => this.iface.popLatestEvents(windowMs),
      writeResource: (resource) => this.iface.writeResource(resource),
      readResource: (id) => this.iface.readResource(id),
    }
    this.perms = dbPermissions(backing)
    this.cached = defaultTools(this.perms, { defaultAuthor })
  }

  /** Reload user-defined tools from the `@tools` entity. */
  async refresh(): Promise<void> {
    const userTools = await loadUserTools(this.perms, { defaultAuthor: this.defaultAuthor })
    this.cached = [
      ...defaultTools(this.perms, { defaultAuthor: this.defaultAuthor }),
      ...userTools,
    ]
  }

  tools(): ToolDef[] {
    return this.cached
  }

  close(): void {
    this.iface.close()
  }
}
