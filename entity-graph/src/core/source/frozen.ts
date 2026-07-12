import type { AppEvent } from '../events'
import { ToolSource, type Source, type ToolDef } from './types'

/**
 * A read-through snapshot: every tool is forwarded to the child unchanged,
 * except `readEvents`, whose output is filtered to events strictly before
 * `beforeTs`. Because filtering happens on the returned event list, it composes
 * over any source that exposes a `readEvents` tool (including Remote/Combined).
 */
export class FrozenSource extends ToolSource {
  constructor(
    public id: string,
    public label: string,
    private child: Source,
    private beforeTs: number
  ) {
    super()
  }

  tools(): ToolDef[] {
    return this.child.tools().map((t) => {
      const passthrough: ToolDef = {
        id: t.id,
        name: t.name,
        description: t.description,
        args: t.args,
        safety: t.safety,
        handler: (args) => this.child.call(t.id, args),
      }
      if (t.id !== 'readEvents') return passthrough
      return {
        ...passthrough,
        description: `${t.description} (frozen: only events before ${this.beforeTs}).`,
        handler: async (args) => {
          const events = (await this.child.call('readEvents', args)) as AppEvent[]
          return events.filter((e) => e.timestamp < this.beforeTs)
        },
      }
    })
  }
}
