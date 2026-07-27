import type { AppEvent } from '../events'
import { readEventsTool, scanEventsTool } from './defaultTools'
import { ToolSource, type Source, type ToolDef } from './types'

/**
 * Merge several sources into one.
 *
 * - `tools()` is the union of the children's tools, deduplicated by id
 *   (first child wins on a collision), EXCEPT the raw-event reads —
 *   `readEvents` and `scanEvents` — which are replaced by combined versions.
 * - `readEvents` returns `children.flatMap(readEvents)` — the raw union, in
 *   child order. Consumers sort/roll up as they see fit. `scanEvents` is built
 *   over that same union, so its overscan follows links across children.
 * - `writeValue` / `writeLink` are inherited from the first child, so writes
 *   land only in `children[0]` (or are absent if it is read-only).
 */
export class CombinedSource extends ToolSource {
  private combinedRead: ToolDef
  private combinedScan: ToolDef

  constructor(
    public id: string,
    public label: string,
    private children: Source[]
  ) {
    super()
    // Raw-event reads that union across children (raw, child order).
    this.combinedRead = readEventsTool((ids) => this.gather({ entityIds: ids }))
    this.combinedScan = scanEventsTool((ids) => this.gather({ entityIds: ids }))
  }

  private async gather(args: { entityIds?: string[] }): Promise<AppEvent[]> {
    const perChild = await Promise.all(
      this.children.map((c) => c.call('readEvents', args) as Promise<AppEvent[]>)
    )
    return perChild.flat()
  }

  private wrap(child: Source, t: ToolDef): ToolDef {
    return {
      id: t.id,
      name: t.name,
      description: t.description,
      args: t.args,
      jsonSchema: t.jsonSchema,
      safety: t.safety,
      handler: (args) => child.call(t.id, args),
    }
  }

  tools(): ToolDef[] {
    const byId = new Map<string, ToolDef>()
    for (const child of this.children) {
      for (const t of child.tools()) {
        if (!byId.has(t.id)) byId.set(t.id, this.wrap(child, t))
      }
    }
    byId.set('readEvents', this.combinedRead)
    byId.set('scanEvents', this.combinedScan)
    return [...byId.values()]
  }
}
