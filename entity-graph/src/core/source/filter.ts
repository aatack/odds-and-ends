import { SAFETY_RANK, ToolNotFoundError, type Safety, type Source, type ToolDef } from './types'

export interface FilterOptions {
  /** If set, only tools whose id is in this list are exposed. */
  allow?: string[]
  /** Tool ids to remove. Applied after `allow`. */
  deny?: string[]
  /** Drop any tool whose safety is riskier than this. */
  maxSafety?: Safety
}

/**
 * Capability wrapper: narrows a source's tool registry by id and/or safety.
 * Filtered-out tools are absent from `tools()` and rejected by `call`.
 */
export class FilterSource implements Source {
  constructor(
    public id: string,
    public label: string,
    private child: Source,
    private opts: FilterOptions
  ) {}

  private permits(tool: ToolDef): boolean {
    if (this.opts.allow && !this.opts.allow.includes(tool.id)) return false
    if (this.opts.deny && this.opts.deny.includes(tool.id)) return false
    if (this.opts.maxSafety && SAFETY_RANK[tool.safety] > SAFETY_RANK[this.opts.maxSafety]) {
      return false
    }
    return true
  }

  tools(): ToolDef[] {
    return this.child.tools().filter((t) => this.permits(t))
  }

  async call(toolId: string, args: unknown): Promise<unknown> {
    const tool = this.child.tools().find((t) => t.id === toolId)
    if (!tool || !this.permits(tool)) throw new ToolNotFoundError(toolId)
    return this.child.call(toolId, args)
  }
}

/** A source with all mutating tools removed (reads only). */
export function readonly(id: string, label: string, child: Source): FilterSource {
  return new FilterSource(id, label, child, { maxSafety: 'pure' })
}
