import { z } from 'zod'
import type { AppEvent, LinkAction } from '../events'
import type { ToolDef } from './types'

/**
 * The minimal storage surface the standard tools need. `SqliteInterface`
 * satisfies this; Combined/Frozen provide their own implementations.
 */
export interface EventBacking {
  /** Read events for specific entities, deduplicated and flattened. */
  readEvents(entityIds: string[]): Promise<AppEvent[]>
  /** Read every event in the store, deduplicated. */
  readAllEvents(): Promise<AppEvent[]>
  writeEvents(events: AppEvent[]): Promise<void>
}

const linkAction = z
  .union([z.literal(0), z.literal(1), z.literal(2), z.literal(3)])
  .describe('0=add, 1=remove, 2=move toward index 0, 3=move toward end')

export interface StandardToolOptions {
  /** Author recorded on writes when the caller does not supply one. */
  defaultAuthor?: string
}

/**
 * Build the three standard tools (`readEvents`, `writeValue`, `writeLink`) over
 * an event backing. These are the only tools guaranteed to exist on a
 * data-backed source; consumers roll events up into entities client-side.
 */
export function standardTools(
  backing: EventBacking,
  opts: StandardToolOptions = {}
): ToolDef[] {
  const author = opts.defaultAuthor ?? 'anonymous'

  const readEvents: ToolDef = {
    id: 'readEvents',
    name: 'Read events',
    description:
      'Return raw events. With `entityIds`, only events touching those entities; ' +
      'omit or pass null to dump every event. No rollup is performed — roll up client-side.',
    safety: 'pure',
    args: z.object({
      entityIds: z
        .array(z.string())
        .optional()
        .describe('Entity ids to read; omit/null for all events.'),
    }),
    handler: async ({ entityIds }: { entityIds?: string[] }) =>
      entityIds === undefined
        ? backing.readAllEvents()
        : backing.readEvents(entityIds),
  }

  const writeValue: ToolDef = {
    id: 'writeValue',
    name: 'Write value',
    description: 'Append a value event setting `key` to `value` on an entity.',
    safety: 'safe-mutating',
    args: z.object({
      entityId: z.string(),
      key: z.string(),
      value: z.any(),
      author: z.string().optional(),
      timestamp: z.number().optional().describe('Unix ms; defaults to now.'),
    }),
    handler: async (a: {
      entityId: string
      key: string
      value: unknown
      author?: string
      timestamp?: number
    }) => {
      await backing.writeEvents([
        {
          type: 'value',
          entityId: a.entityId,
          key: a.key,
          value: a.value ?? null,
          author: a.author ?? author,
          timestamp: a.timestamp ?? Date.now(),
        },
      ])
      return { ok: true }
    },
  }

  const writeLink: ToolDef = {
    id: 'writeLink',
    name: 'Write link',
    description:
      'Append a link event between two entities. `action`: 0=add, 1=remove, ' +
      '2=move toward index 0, 3=move toward end.',
    safety: 'safe-mutating',
    args: z.object({
      sourceId: z.string(),
      destinationId: z.string(),
      action: linkAction.default(0),
      author: z.string().optional(),
      timestamp: z.number().optional().describe('Unix ms; defaults to now.'),
    }),
    handler: async (a: {
      sourceId: string
      destinationId: string
      action: LinkAction
      author?: string
      timestamp?: number
    }) => {
      await backing.writeEvents([
        {
          type: 'link',
          sourceId: a.sourceId,
          destinationId: a.destinationId,
          action: a.action,
          author: a.author ?? author,
          timestamp: a.timestamp ?? Date.now(),
        },
      ])
      return { ok: true }
    },
  }

  return [readEvents, writeValue, writeLink]
}
