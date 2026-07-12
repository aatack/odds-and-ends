import type { AppEvent } from '../events'
import { standardTools, type EventBacking } from './standardTools'
import { ToolSource, type Source, type ToolDef } from './types'

/**
 * Merge several sources into one.
 *
 * - `tools()` is the union of the children's tools, deduplicated by id
 *   (first child wins on a collision), EXCEPT `readEvents`, which is replaced
 *   by a combined version.
 * - `readEvents` returns `children.flatMap(readEvents)` — the raw union, in
 *   child order. Consumers sort/roll up as they see fit.
 * - `writeValue` / `writeLink` are inherited from the first child, so writes
 *   land only in `children[0]` (or are absent if it is read-only).
 */
export class CombinedSource extends ToolSource {
  private combinedRead: ToolDef

  constructor(
    public id: string,
    public label: string,
    private children: Source[]
  ) {
    super()
    const readBacking: EventBacking = {
      readEvents: (ids) => this.gather({ entityIds: ids }),
      readAllEvents: () => this.gather({}),
      writeEvents: async () => {
        throw new Error('CombinedSource read backing cannot write')
      },
    }
    // standardTools returns [readEvents, writeValue, writeLink]; we only need read.
    this.combinedRead = standardTools(readBacking)[0]
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
    return [...byId.values()]
  }
}
