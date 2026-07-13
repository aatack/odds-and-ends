import { z } from 'zod'
import type { AppEvent, LinkAction } from '../events'
import type { EntityInterface } from '../interface/index'
import { EntityWrapper } from '../wrapper'
import type { Permissions } from './permissions'
import type { ToolDef } from './types'

const linkAction = z
  .union([z.literal(0), z.literal(1), z.literal(2), z.literal(3)])
  .describe('0=add, 1=remove, 2=move toward index 0, 3=move toward end')

export interface DefaultToolOptions {
  /** Author recorded on writes when the caller does not supply one. */
  defaultAuthor?: string
}

/**
 * A `readEvents` tool over a raw read function. Shared by {@link defaultTools}
 * and `CombinedSource`, which supplies its own cross-child read.
 */
export function readEventsTool(
  read: (entityIds?: string[]) => Promise<AppEvent[]>
): ToolDef {
  return {
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
    handler: async ({ entityIds }: { entityIds?: string[] }) => read(entityIds),
  }
}

/**
 * Re-bucket a flat, deduplicated event list back into the `Map<id, events>`
 * shape `EntityInterface` (and therefore `EntityWrapper`) expects. A value
 * event lands under its `entityId`; a link event under both endpoints.
 */
function bucketEvents(ids: string[], events: AppEvent[]): Map<string, AppEvent[]> {
  const map = new Map<string, AppEvent[]>()
  for (const id of ids) map.set(id, [])
  for (const e of events) {
    if (e.type === 'value') {
      map.get(e.entityId)?.push(e)
    } else {
      map.get(e.sourceId)?.push(e)
      if (e.destinationId !== e.sourceId) map.get(e.destinationId)?.push(e)
    }
  }
  return map
}

/** An `EntityInterface` backed by the read/write permissions. */
function entityInterface(perms: Permissions): EntityInterface {
  return {
    readEvents: async (ids) => bucketEvents(ids, await perms.readEvents(ids)),
    writeEvents: (events) => perms.writeEvents(events),
  }
}

/** An `EntityWrapper` (rollup + query + create/move) over the permissions. */
export function entityWrapper(perms: Permissions, author = 'anonymous'): EntityWrapper {
  return new EntityWrapper(entityInterface(perms), () => author)
}

/**
 * Build the tools shipped by default, in terms of the fundamental
 * {@link Permissions}. Ordering keeps `readEvents` first (Combined picks it out
 * by index). The entity-level tools (`query`, `readEntities`, `createEntity`,
 * `moveEntity`) reuse `EntityWrapper`; `httpRequest`/`runCommand` delegate to
 * the (currently stubbed) IO permissions.
 */
export function defaultTools(perms: Permissions, opts: DefaultToolOptions = {}): ToolDef[] {
  const author = opts.defaultAuthor ?? 'anonymous'
  const wrapper = entityWrapper(perms, author)

  const readEvents = readEventsTool(perms.readEvents)

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
      await perms.writeEvents([
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
      await perms.writeEvents([
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

  const query: ToolDef = {
    id: 'query',
    name: 'Query from entity',
    description:
      'Depth-first traversal from `rootId` following outbound links, rolling ' +
      'events up into entities. Returns `{ results, continuationStack }`; pass ' +
      '`continuationStack` back to resume when the limit is hit.',
    safety: 'pure',
    args: z.object({
      rootId: z.string(),
      maxDepth: z.number().optional(),
      collapsed: z.array(z.string()).optional().describe('Ids whose children are not expanded.'),
      limit: z.number().optional().describe('Max results per page; defaults to 1000.'),
      continuationStack: z.array(z.any()).optional().describe('Resume token from a prior page.'),
    }),
    handler: (a: {
      rootId: string
      maxDepth?: number
      collapsed?: string[]
      limit?: number
      continuationStack?: unknown[]
    }) =>
      wrapper.resolveQuery(a.rootId, {
        maxDepth: a.maxDepth,
        collapsed: a.collapsed,
        limit: a.limit,
        continuationStack: a.continuationStack as never,
      }),
  }

  const readEntities: ToolDef = {
    id: 'readEntities',
    name: 'Read entities',
    description: 'Roll events up into entities. Returns a map of `entityId` → entity.',
    safety: 'pure',
    args: z.object({ entityIds: z.array(z.string()) }),
    handler: async ({ entityIds }: { entityIds: string[] }) =>
      Object.fromEntries(await wrapper.readEntities(entityIds)),
  }

  const createEntity: ToolDef = {
    id: 'createEntity',
    name: 'Create entity',
    description:
      'Create a new entity from `values`, optionally linked under `parentId`. ' +
      'Writes the relevant value/link events and returns the new entity id.',
    safety: 'safe-mutating',
    args: z.object({
      values: z.record(z.any()).describe('Key → value map for the new entity.'),
      parentId: z.string().optional().describe('Link the new entity under this parent.'),
    }),
    handler: ({ values, parentId }: { values: Record<string, unknown>; parentId?: string }) =>
      wrapper.createEntity(values, parentId),
  }

  const moveEntity: ToolDef = {
    id: 'moveEntity',
    name: 'Move entity',
    description: 'Re-parent an entity: remove the link from `fromParentId` and add one from `toParentId`.',
    safety: 'safe-mutating',
    args: z.object({
      entityId: z.string(),
      fromParentId: z.string(),
      toParentId: z.string(),
    }),
    handler: async (a: { entityId: string; fromParentId: string; toParentId: string }) => {
      await wrapper.moveEntity(a.entityId, a.fromParentId, a.toParentId)
      return { ok: true }
    },
  }

  const httpRequest: ToolDef = {
    id: 'httpRequest',
    name: 'HTTP request',
    description: 'Make an outbound HTTP request. (Not implemented yet.)',
    safety: 'dangerous',
    args: z.object({
      url: z.string(),
      method: z.string().optional(),
      headers: z.record(z.string()).optional(),
      body: z.string().optional(),
    }),
    handler: (a: {
      url: string
      method?: string
      headers?: Record<string, string>
      body?: string
    }) => perms.httpRequest(a),
  }

  const runCommand: ToolDef = {
    id: 'runCommand',
    name: 'Run command',
    description: 'Invoke a command line program. (Not implemented yet.)',
    safety: 'dangerous',
    args: z.object({
      command: z.string(),
      args: z.array(z.string()).optional(),
      cwd: z.string().optional(),
    }),
    handler: (a: { command: string; args?: string[]; cwd?: string }) => perms.runCommand(a),
  }

  return [
    readEvents,
    writeValue,
    writeLink,
    query,
    readEntities,
    createEntity,
    moveEntity,
    httpRequest,
    runCommand,
  ]
}
