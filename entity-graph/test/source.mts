import type { AppEvent } from '../src/core/events'
import { BasePensive } from '../src/core/pensive/base'
import { callInList, type ToolDef } from '../src/core/pensive/tool'
import { POP_AGE_LIMIT_MS, POP_GROUP_MS, type ResourceRecord } from '../src/core/pensive/types'

// A pensive the tests can be pointed at without a database: the real tools — the
// real `scanEvents`, overscan and all — over an array of events. A stand-in for
// SQLite rather than for the tools, because better-sqlite3 has to be built
// against Electron's ABI for the app to start, and rebuilding it for node to run
// a test would break the app it shares this install with.

const touches = (e: AppEvent, ids: Set<string>): boolean =>
  e.type === 'value' ? ids.has(e.entityId) : ids.has(e.sourceId) || ids.has(e.destinationId)

export class MemorySource extends BasePensive {
  readonly id = 'memory'
  readonly label = 'Memory'
  events: AppEvent[] = []
  resources = new Map<string, ResourceRecord>()
  /** Tool calls served — what "without a round trip" is measured in. */
  calls = 0
  /** Every `scanEvents` call's ids, so the overscan can be seen working. */
  scans: string[][] = []

  constructor() {
    super()
    this.defaultAuthor = 'test'
  }

  async readEvents(entityIds?: string[]): Promise<AppEvent[]> {
    if (entityIds === undefined) return [...this.events]
    const ids = new Set(entityIds)
    return this.events.filter((e) => touches(e, ids))
  }

  async writeEvents(events: AppEvent[]): Promise<void> {
    this.events.push(...events)
  }

  async popEvents(author?: string): Promise<AppEvent[]> {
    const mine = (e: AppEvent): boolean => author === undefined || e.author === author
    const theirs = this.events.filter(mine)
    if (!theirs.length) return []
    const latest = Math.max(...theirs.map((e) => e.timestamp))
    if (latest < Date.now() - POP_AGE_LIMIT_MS) return []
    const from = latest - POP_GROUP_MS
    const goes = (e: AppEvent): boolean => e.timestamp >= from && mine(e)
    const taken = this.events.filter(goes)
    this.events = this.events.filter((e) => !goes(e))
    return taken.sort((a, b) => a.timestamp - b.timestamp)
  }

  async readResource(id: string): Promise<ResourceRecord | null> {
    return this.resources.get(id) ?? null
  }

  async writeResource(resource: ResourceRecord): Promise<void> {
    this.resources.set(resource.id, resource)
  }

  /** The transport's seam: one tool call, counted. */
  call = async (toolId: string, args: unknown): Promise<unknown> => {
    this.calls++
    if (toolId === 'scanEvents') this.scans.push([...(args as { entityIds: string[] }).entityIds])
    return this.callTool(toolId, args)
  }

  /** Straight at the tool list, for a test that wants one by hand. */
  tool(id: string): ToolDef | undefined {
    return this.tools().find((t) => t.id === id)
  }

  callAny(tools: ToolDef[], toolId: string, args: unknown): Promise<unknown> {
    return callInList(tools, toolId, args)
  }

  /** Write events straight in, as a fixture rather than as the client would. */
  given(...events: Partial<AppEvent>[]): void {
    for (const e of events) {
      this.events.push({
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
