import type { AppEvent } from '../src/core/events'
import { defaultTools } from '../src/core/source/defaultTools'
import { dbPermissions, POP_AGE_LIMIT_MS, type EventBacking } from '../src/core/source/permissions'
import { invokeTool, type ToolDef } from '../src/core/source/types'

// A source the desktop client can be pointed at without a database or a server:
// the real default tools — the real `scanEvents`, overscan and all — over an
// array of events. A stand-in for SQLite rather than for the tools, because
// better-sqlite3 is built against Electron's ABI in this install and rebuilding
// it for node would break the app it shares that install with.

const touches = (e: AppEvent, ids: Set<string>): boolean =>
  e.type === 'value' ? ids.has(e.entityId) : ids.has(e.sourceId) || ids.has(e.destinationId)

class MemoryBacking implements EventBacking {
  events: AppEvent[] = []

  async readEvents(entityIds: string[]): Promise<AppEvent[]> {
    const ids = new Set(entityIds)
    return this.events.filter((e) => touches(e, ids))
  }

  async readAllEvents(): Promise<AppEvent[]> {
    return [...this.events]
  }

  async writeEvents(events: AppEvent[]): Promise<void> {
    this.events.push(...events)
  }

  async popLatestEvents(windowMs: number): Promise<AppEvent[]> {
    if (!this.events.length) return []
    const latest = Math.max(...this.events.map((e) => e.timestamp))
    if (latest < Date.now() - POP_AGE_LIMIT_MS) return []
    const from = latest - windowMs
    const taken = this.events.filter((e) => e.timestamp >= from)
    this.events = this.events.filter((e) => e.timestamp < from)
    return taken.sort((a, b) => a.timestamp - b.timestamp)
  }
}

export class MemorySource {
  readonly backing = new MemoryBacking()
  private tools: ToolDef[]
  /** Tool calls served — what "without a round trip" is measured in. */
  calls = 0
  /** Every `scanEvents` call's ids, so the overscan can be seen working. */
  scans: string[][] = []

  constructor() {
    this.tools = defaultTools(dbPermissions(this.backing), { defaultAuthor: 'test' })
  }

  call = async (toolId: string, args: unknown): Promise<unknown> => {
    this.calls++
    if (toolId === 'scanEvents') this.scans.push([...(args as { entityIds: string[] }).entityIds])
    const tool = this.tools.find((t) => t.id === toolId)
    if (!tool) throw new Error(`No tool with id "${toolId}"`)
    return invokeTool(tool, args)
  }

  /** Write events straight in, as a fixture rather than as the client would. */
  given(...events: Partial<AppEvent>[]): void {
    for (const e of events) {
      this.backing.events.push({
        timestamp: 1,
        author: 'fixture',
        ...(e as AppEvent),
      })
    }
  }

  /** `parent → [children]`, as link events. */
  tree(links: Record<string, string[]>): void {
    for (const [sourceId, children] of Object.entries(links)) {
      for (const destinationId of children) {
        this.given({ type: 'link', sourceId, destinationId, action: 0 })
      }
    }
  }

  /** `id → { key: value }`, as value events. */
  values(entities: Record<string, Record<string, unknown>>): void {
    for (const [entityId, values] of Object.entries(entities)) {
      for (const [key, value] of Object.entries(values)) {
        this.given({ type: 'value', entityId, key, value })
      }
    }
  }
}
