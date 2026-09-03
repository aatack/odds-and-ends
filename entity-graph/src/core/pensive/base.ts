import type { AppEvent } from '../events'
import { callInList, toolMeta, type ToolDef } from './tool'
import { pensiveTools } from './tools'
import { loadUserTools } from './userTools'
import type { Pensive, ResourceRecord, ToolMeta } from './types'

/**
 * A pensive that gets its tools by being one.
 *
 * Everything below the tool layer — the five calls — is left abstract; what this
 * supplies is the layer above: {@link pensiveTools} over those calls, plus
 * whatever the store itself says is a tool (the notes under `@tools`), and the
 * routing that turns a tool id and some JSON into a call. So an implementation
 * says how events are stored and gains the whole vocabulary.
 */
export abstract class BasePensive implements Pensive {
  abstract readonly id: string
  abstract readonly label: string

  /** Author recorded on a write that names none. */
  protected defaultAuthor?: string

  /** The tools as they last stood. Rebuilt by {@link refresh}. */
  private cached: ToolDef[] | null = null
  /** Whether the store has been asked for its own tools yet. */
  private discovered = false

  abstract readEvents(entityIds?: string[]): Promise<AppEvent[]>
  abstract writeEvents(events: AppEvent[]): Promise<void>
  abstract popEvents(windowMs: number): Promise<AppEvent[]>
  abstract readResource(id: string): Promise<ResourceRecord | null>
  abstract writeResource(resource: ResourceRecord): Promise<void>

  /** The built-in vocabulary, without the user's own tools. */
  protected builtinTools(): ToolDef[] {
    return pensiveTools(this, { defaultAuthor: this.defaultAuthor })
  }

  tools(): ToolDef[] {
    return (this.cached ??= this.builtinTools())
  }

  /**
   * Read the user's own tools out of the store again. Tolerant of a store that
   * cannot be read — a broken pensive still answers with the built-ins, which is
   * what the sources page needs in order to say what is wrong with it.
   */
  async refresh(): Promise<void> {
    this.discovered = true
    const declared = await loadUserTools(this, { defaultAuthor: this.defaultAuthor }).catch(
      () => [] as ToolDef[],
    )
    this.cached = [...this.builtinTools(), ...declared]
  }

  /**
   * Everything callable. The user's own tools are read the first time somebody
   * asks, rather than only when the registry built this: a pensive wrapped per
   * request — which is what a bearer token makes of one — would otherwise publish
   * the built-ins and nothing the store itself defines.
   */
  async listTools(): Promise<ToolMeta[]> {
    if (!this.discovered) await this.refresh()
    return this.tools().map(toolMeta)
  }

  callTool(toolId: string, args: unknown): Promise<unknown> {
    return callInList(this.tools(), toolId, args)
  }
}
