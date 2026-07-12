import type { AppEvent } from '../events'
import { SqliteInterface } from '../interface/sqlite'
import { standardTools, type EventBacking } from './standardTools'
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
 * The base source: an event-sourced SQLite store exposing only the standard
 * tools. Every other source is built by wrapping this (or a Remote).
 */
export class SqliteSource extends ToolSource {
  private iface: SqliteInterface
  private cached: ToolDef[]

  constructor(
    public id: string,
    public label: string,
    path: string,
    defaultAuthor?: string
  ) {
    super()
    this.iface = new SqliteInterface(path)
    const backing: EventBacking = {
      readEvents: async (ids) => flatten(await this.iface.readEvents(ids)),
      readAllEvents: () => this.iface.readAllEvents(),
      writeEvents: (events) => this.iface.writeEvents(events),
    }
    this.cached = standardTools(backing, { defaultAuthor })
  }

  tools(): ToolDef[] {
    return this.cached
  }

  close(): void {
    this.iface.close()
  }
}
