import type { AppEvent } from '../events'
import { callInList, toolMeta, type ToolDef } from './tool'
import { pensiveTools } from './tools'
import type { Pensive, ResourceRecord, ToolMeta } from './types'

/**
 * A pensive that gets its tools by being one.
 *
 * Everything below the tool layer — the five calls — is left abstract; what this
 * supplies is the layer above: {@link pensiveTools} over those calls, and the
 * routing that turns a tool id and some JSON into a call. So an implementation
 * says how events are stored and gains the whole vocabulary.
 */
export abstract class BasePensive implements Pensive {
  abstract readonly id: string
  abstract readonly label: string

  /** Author recorded on a write that names none. */
  protected defaultAuthor?: string

  /** Built once: the vocabulary is the same for every pensive. */
  private cached: ToolDef[] | null = null

  abstract readEvents(entityIds?: string[]): Promise<AppEvent[]>
  abstract writeEvents(events: AppEvent[]): Promise<void>
  abstract popEvents(author?: string): Promise<AppEvent[]>
  abstract readResource(id: string): Promise<ResourceRecord | null>
  abstract writeResource(resource: ResourceRecord): Promise<void>

  tools(): ToolDef[] {
    return (this.cached ??= pensiveTools(this, { defaultAuthor: this.defaultAuthor }))
  }

  async listTools(): Promise<ToolMeta[]> {
    return this.tools().map(toolMeta)
  }

  callTool(toolId: string, args: unknown): Promise<unknown> {
    return callInList(this.tools(), toolId, args)
  }
}
