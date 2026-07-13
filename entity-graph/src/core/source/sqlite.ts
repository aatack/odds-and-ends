import type { AppEvent } from '../events'
import { SqliteInterface } from '../interface/sqlite'
import { defaultTools } from './defaultTools'
import { dbPermissions, type EventBacking, type Permissions } from './permissions'
import { loadUserTools } from './userTools'
import { ToolSource, type ToolDef } from './types'

/**
 * Flatten a `Map<id, events>` into a deduplicated event array. Link events are
 * stored under both their source and destination buckets as the same object
 * reference, so a reference Set removes those duplicates.
 */
function flatten(map: Map<string, AppEvent[]>): AppEvent[] {
  const seen = new Set<AppEvent>()
  const out: AppEvent[] = []
  for (const events of map.values()) {
    for (const e of events) {
      if (!seen.has(e)) {
        seen.add(e)
        out.push(e)
      }
    }
  }
  return out
}

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
    const backing: EventBacking = {
      readEvents: async (ids) => flatten(await this.iface.readEvents(ids)),
      readAllEvents: () => this.iface.readAllEvents(),
      writeEvents: (events) => this.iface.writeEvents(events),
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
