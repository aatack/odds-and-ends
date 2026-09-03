import { z } from 'zod'
import { builtinEvents } from '../builtins'
import { bucketEvents, rollupEntity, type LinkDirection } from '../entity'
import type { AppEvent, LinkAction } from '../events'
import type { EntityInterface } from '../interface/index'
import { runQuery } from '../query'
import { EntityWrapper } from '../wrapper'
import type { ToolDef } from './tool'
import type { EventScan, Pensive } from './types'

const linkAction = z
  .union([z.literal(0), z.literal(1), z.literal(2), z.literal(3)])
  .describe('0=add, 1=remove, 2=move toward index 0, 3=move toward end')

/** A raw event, as `writeEvents` accepts it and `popEvents` hands it back. */
const appEvent = z.union([
  z.object({
    type: z.literal('value'),
    timestamp: z.number(),
    author: z.string(),
    entityId: z.string(),
    key: z.string(),
    value: z.any(),
  }),
  z.object({
    type: z.literal('link'),
    timestamp: z.number(),
    author: z.string(),
    sourceId: z.string(),
    destinationId: z.string(),
    action: linkAction,
  }),
])

export interface PensiveToolOptions {
  /** Author recorded on writes when the caller does not supply one. */
  defaultAuthor?: string
}

/**
 * A `readEvents` tool over a raw read function, so a pensive that reads across
 * several others can supply its own.
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
 * Events carry no id, so two reads that both turn up the same event hand back
 * two equal objects. This is what tells them apart from two genuinely separate
 * writes — which would differ in at least their timestamp. Duplicates matter:
 * a repeated link *move* would shift the link twice in a rollup.
 */
const eventKey = (e: AppEvent): string =>
  e.type === 'value'
    ? ['v', e.entityId, e.key, e.timestamp, e.author, JSON.stringify(e.value ?? null)].join('\0')
    : ['l', e.sourceId, e.destinationId, e.action, e.timestamp, e.author].join('\0')

/** Entities one `query` page walks over, unless the caller says otherwise. */
const QUERY_LIMIT = 200

/** How many entities per layer an overscan will read beyond the ones asked for. */
const SCAN_OVERSCAN = 32
/** How many layers of children an overscan follows. */
const SCAN_DEPTH = 2

/**
 * A `scanEvents` tool over a raw read function — the same seam
 * {@link readEventsTool} takes, so a read that spans several pensives gets a
 * scan that does too.
 */
export function scanEventsTool(read: (entityIds: string[]) => Promise<AppEvent[]>): ToolDef {
  return {
    id: 'scanEvents',
    name: 'Scan events',
    description:
      'Read the raw events for a set of entities, and — unless `depth` says ' +
      'otherwise — for the entities they link out to as well. Returns ' +
      '`{ entityIds, events }`, where `entityIds` is every entity whose events ' +
      'are complete in `events`: the ones asked for, plus whatever the overscan ' +
      'reached. Nothing is rolled up. This is what a client that keeps its own ' +
      'cache reads through: almost every query walks the graph downwards, so ' +
      'reading a little ahead turns a round trip per level into one per query.',
    safety: 'pure',
    args: z.object({
      entityIds: z.array(z.string()),
      depth: z
        .number()
        .optional()
        .describe(
          `Layers of children to read ahead; 0 reads only what was asked for. Defaults to ${SCAN_DEPTH}.`,
        ),
      overscan: z
        .number()
        .optional()
        .describe(`Most entities to read per layer of overscan. Defaults to ${SCAN_OVERSCAN}.`),
    }),
    handler: async (a: {
      entityIds: string[]
      depth?: number
      overscan?: number
    }): Promise<EventScan> => {
      const depth = a.depth ?? SCAN_DEPTH
      const overscan = a.overscan ?? SCAN_OVERSCAN
      // Every id whose events are complete in the result — which is the ids
      // actually read, not the ones the frontier wanted to reach: a layer clipped
      // by the overscan must not be reported as covered.
      const covered = new Set<string>()
      const events: AppEvent[] = []
      const seen = new Set<string>()

      let frontier = [...new Set(a.entityIds)]
      for (const id of frontier) covered.add(id)

      for (let layer = 0; frontier.length > 0; layer++) {
        const batch = await read(frontier)
        for (const e of batch) {
          const key = eventKey(e)
          // A link between two layers is read from both ends, so consecutive
          // layers overlap in their events even though they don't in their ids.
          if (seen.has(key)) continue
          seen.add(key)
          events.push(e)
        }
        if (layer >= depth) break

        const buckets = bucketEvents(frontier, batch)
        const next = new Set<string>()
        for (const id of frontier) {
          // Outbound only: the overscan is a bet that the client is about to walk
          // downwards, and a widely-referenced entity would drag its whole
          // neighbourhood in if inbound links counted too.
          for (const child of rollupEntity(id, buckets.get(id) ?? []).outboundLinks) {
            if (!covered.has(child)) next.add(child)
          }
        }
        frontier = [...next].slice(0, overscan)
        for (const id of frontier) covered.add(id)
      }

      return { entityIds: [...covered], events }
    },
  }
}

/**
 * Reading events, with the ones the store supplies rather than holds in front of
 * them — see `../builtins`. Every read of the store goes through here, so a
 * client's cache, a query and an agent over MCP all see the same `type` entity,
 * and none of them has to know it was never written down.
 *
 * They arrive first and timestamped 0, which is only tidiness: a rollup sorts,
 * so what is actually written wins whatever order it came back in.
 */
export const readWithBuiltins =
  (pensive: Pensive) =>
  async (entityIds?: string[]): Promise<AppEvent[]> => [
    ...builtinEvents(entityIds),
    ...(await pensive.readEvents(entityIds)),
  ]

/** An `EntityInterface` backed by a pensive's own reads and writes. */
function entityInterface(pensive: Pensive): EntityInterface {
  const read = readWithBuiltins(pensive)
  return {
    readEvents: async (ids) => bucketEvents(ids, await read(ids)),
    writeEvents: (events) => pensive.writeEvents(events),
  }
}

/** An `EntityWrapper` (rollup + query + create/move) over a pensive. */
export function entityWrapper(pensive: Pensive, author = 'anonymous'): EntityWrapper {
  return new EntityWrapper(entityInterface(pensive), () => author)
}

/**
 * The vocabulary every pensive wears, built out of the five calls it has to
 * implement. This is what makes {@link Pensive} worth being: a store answers
 * `query`, `createEntity` and the rest by being one, so the app, an agent over
 * MCP and a script all speak the same language to a SQLite file, to several of
 * them joined together, and to one on another machine.
 *
 * Nothing here reaches outside the store — no HTTP, no shell. Those belong to
 * the app, which holds them itself.
 */
export function pensiveTools(pensive: Pensive, opts: PensiveToolOptions = {}): ToolDef[] {
  const author = opts.defaultAuthor ?? 'anonymous'
  const wrapper = entityWrapper(pensive, author)

  /**
   * Who a write is recorded as. `author` names one outright — what undo does,
   * putting an event back exactly as it was. `via` instead marks the surface the
   * write arrived over, which is a suffix rather than a name because the person
   * is the same person: `alex` at the keyboard and `alex:mcp` through an agent
   * reading the same store, so history says which without losing whose.
   */
  const authorFor = (a: { author?: string; via?: string }): string =>
    a.author ?? (a.via ? `${author}:${a.via}` : author)

  const via = z
    .string()
    .optional()
    .describe('Surface the write came over; recorded as `author:via`, e.g. `mcp`.')

  const read = readWithBuiltins(pensive)
  const readEvents = readEventsTool(read)
  const scanEvents = scanEventsTool(read)

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
      via,
      timestamp: z.number().optional().describe('Unix ms; defaults to now.'),
    }),
    handler: async (a: {
      entityId: string
      key: string
      value: unknown
      author?: string
      via?: string
      timestamp?: number
    }) => {
      await pensive.writeEvents([
        {
          type: 'value',
          entityId: a.entityId,
          key: a.key,
          value: a.value ?? null,
          author: authorFor(a),
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
      via,
      timestamp: z.number().optional().describe('Unix ms; defaults to now.'),
    }),
    handler: async (a: {
      sourceId: string
      destinationId: string
      action: LinkAction
      author?: string
      via?: string
      timestamp?: number
    }) => {
      await pensive.writeEvents([
        {
          type: 'link',
          sourceId: a.sourceId,
          destinationId: a.destinationId,
          action: a.action,
          author: authorFor(a),
          timestamp: a.timestamp ?? Date.now(),
        },
      ])
      return { ok: true }
    },
  }

  const query: ToolDef = {
    id: 'query',
    name: 'Query the tree',
    description:
      'Read a slice of the graph as an outline: a depth-first walk from `path`, ' +
      'rolling events up into entities. Returns `{ rows, continuation, scanned }`, ' +
      'where each row is `{ path, entity }` — the last id in `path` is the entity ' +
      'and the one before it is the parent it hangs off, so the same entity ' +
      'appearing in two places is two rows.\n\n' +
      'When more remains than `limit` allowed, `continuation` is the path to ' +
      'resume from: pass it straight back as `path` for the next page. It is null ' +
      'once the walk has run out.\n\n' +
      '`find` and `sections` filter the rows *after* the walk, so `limit` always ' +
      'means "entities visited" (reported as `scanned`) and a narrow filter over ' +
      'a wide tree comes back quickly with few rows and a continuation, rather ' +
      'than reading the whole thing. `open` is the exception: it narrows the walk ' +
      'as well, since nothing under a ticked item is outstanding.',
    safety: 'pure',
    args: z.object({
      path: z
        .union([z.string(), z.array(z.string())])
        .describe(
          'Where to start: an entity id, or the full path to one as returned in ' +
            '`continuation`. Starting from a path rather than an id is what lets a ' +
            'walk resume in the middle of the tree.',
        ),
      limit: z
        .number()
        .optional()
        .describe(`Most entities to visit before stopping. Defaults to ${QUERY_LIMIT}.`),
      maxDepth: z
        .number()
        .optional()
        .describe(
          'Levels to descend below the first entity in `path`. Omit for no limit. ' +
            'Measured from there rather than from where the walk resumes, so passing ' +
            'a continuation back with the same `maxDepth` reads the rest of the same ' +
            'shape.',
        ),
      direction: z
        .enum(['out', 'in'])
        .optional()
        .describe('Which links to follow; "in" answers "what links to this?". Defaults to "out".'),
      find: z
        .string()
        .optional()
        .describe(
          'Keep only rows whose text contains this, plus their ancestors. With ' +
            '`sections`, it searches the table of contents rather than the tree: the ' +
            'two narrow together, so what comes back is the headings that say this.',
        ),
      sections: z
        .boolean()
        .optional()
        .describe('Keep only section rows — the tree read as a table of contents.'),
      open: z
        .boolean()
        .optional()
        .describe(
          'Keep only unticked tasks (`open: true`), and stop walking at ticked ' +
            'ones (`open: false`) — the tree read as what is left to do. A note ' +
            'that is not a task at all is neither, so the walk goes through it to ' +
            'whatever tasks hang off it.',
        ),
    }),
    handler: (a: {
      path: string | string[]
      limit?: number
      maxDepth?: number
      direction?: LinkDirection
      find?: string
      sections?: boolean
      open?: boolean
    }) => {
      const start = typeof a.path === 'string' ? [a.path] : a.path
      if (!start.length) throw new Error('path must name at least one entity')
      return runQuery(
        start,
        async (ids) => Object.fromEntries(await wrapper.readEntities(ids)),
        {
          direction: a.direction ?? 'out',
          // No collapse map on purpose: a caller reading the tree wants a slice
          // of it described by where it starts and how deep it goes, not an
          // arbitrary shape.
          collapsed: [],
          // The cap hangs off the *root* of the walk, not the entity the page
          // happens to resume at. Every path in every page begins with that root,
          // so the cap still applies to the branches a resumed walk pops back up
          // into — keyed at the resume point it would simply stop applying, since
          // a sibling branch has no ancestor that sets it.
          maxDepth: a.maxDepth == null ? {} : { [start[0]]: a.maxDepth },
        },
        a.limit ?? QUERY_LIMIT,
        { find: a.find, sections: a.sections, open: a.open },
      )
    },
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
      via,
    }),
    handler: (a: { values: Record<string, unknown>; parentId?: string; via?: string }) =>
      wrapper.createEntity(a.values, a.parentId, authorFor(a)),
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

  const writeEvents: ToolDef = {
    id: 'writeEvents',
    name: 'Write events',
    description:
      'Append raw events verbatim, keeping the timestamps and authors they carry. ' +
      'Backs redo — pass back what `popEvents` returned. Prefer `writeValue` / ' +
      '`writeLink` for ordinary edits, which stamp the current time for you.',
    safety: 'safe-mutating',
    args: z.object({ events: z.array(appEvent) }),
    handler: async ({ events }: { events: AppEvent[] }) => {
      await pensive.writeEvents(events)
      return { written: events.length }
    },
  }

  const popEvents: ToolDef = {
    id: 'popEvents',
    name: 'Pop latest events',
    description:
      'Remove the last action\'s events and return them. Backs undo: one user ' +
      'action often writes several events at the same instant, so they come off ' +
      'together. Hand the result to `writeEvents` to put them back.\n\n' +
      'How much counts as one action, and how far back this reaches at all, are ' +
      'the store\'s to decide — five minutes as standard, so on a store that has ' +
      'been idle that long this returns nothing. There is nothing to pass but ' +
      '`author`.',
    safety: 'safe-mutating',
    args: z.object({
      author: z
        .string()
        .optional()
        .describe(
          'Only take this person\'s own events off. Forced to whoever the bearer ' +
            'token belongs to when the store is reached over HTTP, so asking for ' +
            'somebody else is not a thing a remote client can do.',
        ),
    }),
    handler: ({ author }: { author?: string }) => pensive.popEvents(author),
  }

  const writeResource: ToolDef = {
    id: 'writeResource',
    name: 'Write resource',
    description:
      'Store bytes under an entity id, replacing anything already there. The id ' +
      'is the entity that describes the resource — conventionally one with ' +
      '`type: "file"` — so the bytes and their description share a key. Data is ' +
      'base64. Resources are not events: undo does not reach them.',
    safety: 'safe-mutating',
    args: z.object({
      id: z.string().describe('The entity id the bytes belong to.'),
      mimeType: z.string(),
      data: z.string().describe('Base64-encoded bytes.'),
      name: z.string().nullable().optional().describe('Original file name, if there was one.'),
      author: z.string().optional(),
      timestamp: z.number().optional().describe('Unix ms; defaults to now.'),
    }),
    handler: async (a: {
      id: string
      mimeType: string
      data: string
      name?: string | null
      author?: string
      timestamp?: number
    }) => {
      await pensive.writeResource({
        id: a.id,
        mimeType: a.mimeType,
        data: a.data,
        name: a.name ?? null,
        author: a.author ?? author,
        timestamp: a.timestamp ?? Date.now(),
      })
      return { ok: true }
    },
  }

  const readResource: ToolDef = {
    id: 'readResource',
    name: 'Read resource',
    description:
      'Return the bytes stored under an entity id as base64, with the mime type, ' +
      'name, author and timestamp. Null when there is nothing there.',
    safety: 'pure',
    args: z.object({ id: z.string() }),
    handler: ({ id }: { id: string }) => pensive.readResource(id),
  }

  return [
    readEvents,
    writeValue,
    writeLink,
    writeEvents,
    popEvents,
    writeResource,
    readResource,
    scanEvents,
    query,
    readEntities,
    createEntity,
    moveEntity,
  ]
}
