import type { AppEvent } from '../events'
import { ToolSource, type Source, type ToolDef } from './types'

/**
 * A read-through snapshot: every tool is forwarded to the child unchanged,
 * except the two that hand back raw events — `readEvents` and `scanEvents` —
 * whose output is filtered to events strictly before `beforeTs`. Because
 * filtering happens on the returned event list, it composes over any source that
 * exposes them (including Remote/Combined).
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
        jsonSchema: t.jsonSchema,
        safety: t.safety,
        handler: (args) => this.child.call(t.id, args),
      }
      if (t.id !== 'readEvents' && t.id !== 'scanEvents') return passthrough
      const before = (events: AppEvent[]): AppEvent[] =>
        events.filter((e) => e.timestamp < this.beforeTs)
      return {
        ...passthrough,
        description: `${t.description} (frozen: only events before ${this.beforeTs}).`,
        handler: async (args) => {
          const result = await this.child.call(t.id, args)
          // `readEvents` hands back the list itself; `scanEvents` wraps it
          // alongside the entities it covers, which the freeze doesn't change.
          if (Array.isArray(result)) return before(result as AppEvent[])
          const scan = result as { entityIds: string[]; events: AppEvent[] }
          return { ...scan, events: before(scan.events) }
        },
      }
    })
  }
}
