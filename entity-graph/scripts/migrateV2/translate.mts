// v2's rows as this store's events, and the proof that they say the same thing.
//
// Nothing here opens a file: rows in, events out, which is what makes the whole
// translation testable without a database and checkable without writing one.
//
// It is a translation of the *history* rather than a snapshot of the end of it,
// which is only possible because the two vocabularies agree operation for
// operation — `+` on something already linked is a no-op both sides, a `-` that
// empties a list leaves nothing behind either way, and v2's two reordering
// characters are v3's two move actions. Replay the old events in the old order
// and the new store rolls up to what the old one showed, with every timestamp
// and every intermediate state still in it.

import { isDeepStrictEqual } from 'node:util'
import { bucketEvents, rollupEntity } from '../../src/core/entity'
import type { AppEvent, LinkAction } from '../../src/core/events'
import type { ResourceRecord } from '../../src/core/source/permissions'
import type { V2Resource } from './read.mjs'
import { rollupV2, rootOf, v2Order, type Json, type V2Row } from './v2Reducer.mjs'

/**
 * Where the outline starts here. Both clients and the MCP server each name it
 * for themselves rather than share a constant, since it is a string in the data
 * rather than a setting; a script is no different.
 */
export const INDEX_ID = '@index'

/**
 * What v2's bytes are taken to be. v2 stored no mime type at all and its client
 * hard-coded `image/png` when copying one back out, which is also what pasting a
 * screenshot writes here — so this is the one guess in the whole migration, and
 * it is the same guess the old app was already making.
 */
export const RESOURCE_MIME = 'image/png'

export type WarningKind =
  | 'unreadableRow'
  | 'unknownOperation'
  | 'emptyLinkTarget'
  | 'imageNotTrue'
  | 'imageAndType'
  | 'imageWithoutBytes'
  | 'resourceWithoutFlag'
  | 'resourceWithoutEntity'
  | 'danglingLink'
  | 'inboundWithoutOutbound'

export interface Warning {
  kind: WarningKind
  detail: string
}

export interface Translation {
  /** The rows, in the order v2 read them — what a check has to fold. */
  ordered: V2Row[]
  events: AppEvent[]
  resources: ResourceRecord[]
  /** How many rows carried each key, which is the shape of the old store. */
  keyCounts: Map<string, number>
  /** Every entity the new store will know about. */
  ids: Set<string>
  warnings: Warning[]
}

/** `+` adds, `-` removes, `<` moves towards the front, `>` towards the back. */
const ACTIONS: Record<string, LinkAction> = { '+': 0, '-': 1, '<': 2, '>': 3 }

/**
 * The edits one write to `inbound`/`outbound` makes. An array applies its
 * elements left to right — v2's reducer recurses, so nesting is flattened the
 * same way — and anything that isn't a non-empty string changes nothing, which
 * is v2's own reading of it and so needs no event here either.
 */
function operations(value: Json): { operation: string; target: string }[] {
  if (Array.isArray(value)) return value.flatMap(operations)
  if (typeof value !== 'string' || value.length < 1) return []
  return [{ operation: value[0]!, target: value.slice(1) }]
}

/** Values marking an entity as holding bytes — what pasting a file writes here. */
const fileValues = (id: string, timestamp: number, author: string): AppEvent[] => [
  { type: 'value', timestamp, author, entityId: id, key: 'type', value: 'file' },
  { type: 'value', timestamp, author, entityId: id, key: 'mimeType', value: RESOURCE_MIME },
]

export function translate(
  store: { rows: V2Row[]; resources: V2Resource[]; unreadable?: { uuid: string; key: string }[] },
  author: string
): Translation {
  const ordered = v2Order(store.rows)
  const events: AppEvent[] = []
  const warnings: Warning[] = []
  const keyCounts = new Map<string, number>()
  const ids = new Set<string>()

  const withBytes = new Map(store.resources.map((resource) => [resource.uuid, resource]))
  const flaggedAsImage = new Set<string>()
  const typed = new Set<string>()
  const linkTargets = new Set<string>()

  for (const row of store.unreadable ?? []) {
    warnings.push({
      kind: 'unreadableRow',
      detail: `${row.uuid} · ${row.key} — the stored JSON wouldn't parse, so the row is dropped`,
    })
  }

  // The old root is linked under the index rather than renamed to it: no id
  // changes, so nothing has to be rewritten inside anybody's text. It goes in
  // first, stamped with the root's own age, so the index is as old as the store
  // rather than as old as the migration.
  const root = rootOf(ordered)
  if (root !== null) {
    const born = ordered.find((row) => row.uuid === root)!.timestamp
    events.push({
      type: 'link',
      timestamp: born,
      author,
      sourceId: INDEX_ID,
      destinationId: root,
      action: 0,
    })
  }

  for (const { timestamp, uuid, key, value } of ordered) {
    ids.add(uuid)
    keyCounts.set(key, (keyCounts.get(key) ?? 0) + 1)

    // The half of a link this store derives. Dropped rather than translated, and
    // then read once more at the end as the only cross-check the old format
    // offers for free.
    if (key === 'inbound') continue

    if (key === 'outbound') {
      for (const { operation, target } of operations(value)) {
        const action = ACTIONS[operation]
        if (action === undefined) {
          warnings.push({
            kind: 'unknownOperation',
            detail: `${uuid} wrote \`${operation}\` at its children, which meant nothing in v2 either`,
          })
          continue
        }
        if (target === '') {
          warnings.push({
            kind: 'emptyLinkTarget',
            detail: `${uuid} wrote \`${operation}\` naming no entity`,
          })
        }
        linkTargets.add(target)
        events.push({ type: 'link', timestamp, author, sourceId: uuid, destinationId: target, action })
      }
      continue
    }

    // v2 said "this entity is a picture" with a flag; here the entity says what
    // it is and what its bytes are, so the flag becomes those two values.
    if (key === 'image') {
      if (value === true) {
        flaggedAsImage.add(uuid)
        events.push(...fileValues(uuid, timestamp, author))
      } else {
        warnings.push({
          kind: 'imageNotTrue',
          detail: `${uuid} set \`image\` to ${JSON.stringify(value)}; there is no un-filing an entity here, so it is dropped`,
        })
      }
      continue
    }

    if (key === 'type') typed.add(uuid)
    events.push({ type: 'value', timestamp, author, entityId: uuid, key, value })
  }

  const resources: ResourceRecord[] = []
  for (const resource of store.resources) {
    resources.push({
      id: resource.uuid,
      timestamp: resource.timestamp,
      author,
      mimeType: RESOURCE_MIME,
      name: null,
      data: resource.bytes.toString('base64'),
    })

    if (!ids.has(resource.uuid)) {
      warnings.push({
        kind: 'resourceWithoutEntity',
        detail: `${resource.uuid} has bytes but nothing was ever written to it; they are carried over anyway, with nothing linking to them`,
      })
      ids.add(resource.uuid)
    }

    // Bytes with no flag would arrive as an entity that renders as nothing at
    // all, so it is marked as a file regardless — losing the picture is worse
    // than adding a value v2 forgot to write.
    if (!flaggedAsImage.has(resource.uuid)) {
      warnings.push({
        kind: 'resourceWithoutFlag',
        detail: `${resource.uuid} has bytes but no \`image\` flag; marked as a file anyway`,
      })
      events.push(...fileValues(resource.uuid, resource.timestamp, author))
    }
  }

  for (const id of flaggedAsImage) {
    if (typed.has(id)) {
      warnings.push({
        kind: 'imageAndType',
        detail: `${id} is both an image and a typed entity; one \`type\` has to win, and it will be whichever v2 wrote last`,
      })
    }
    if (!withBytes.has(id)) {
      warnings.push({
        kind: 'imageWithoutBytes',
        detail: `${id} is flagged as an image but the old store holds no bytes for it`,
      })
    }
  }

  for (const target of linkTargets) {
    if (!ids.has(target)) {
      warnings.push({
        kind: 'danglingLink',
        detail: `something links to ${target || '(nothing)'}, which nothing was ever written to`,
      })
    }
  }

  return { ordered, events, resources, keyCounts, ids, warnings }
}

// --- Checking the answer ----------------------------------------------------

export interface Discrepancy {
  id: string
  field: 'values' | 'children'
  expected: unknown
  actual: unknown
}

/** What the old entity's values become, given the rules above. */
function expectedValues(before: Record<string, Json>, hasBytes: boolean): Record<string, unknown> {
  const values: Record<string, unknown> = {}
  for (const [key, value] of Object.entries(before)) {
    if (key === 'inbound' || key === 'outbound' || key === 'image') continue
    values[key] = value
  }
  if (before.image === true || hasBytes) {
    values.type = 'file'
    values.mimeType = RESOURCE_MIME
  }
  return values
}

const asIds = (value: Json | undefined): string[] =>
  Array.isArray(value) ? value.filter((item): item is string => typeof item === 'string') : []

/**
 * Roll every entity up both ways and compare: v2's reducer over the old rows,
 * this store's over the events written. A run that reports nothing lost nothing.
 *
 * The events are bucketed here rather than read back, which is the same thing:
 * a rollup sorts by timestamp and is stable, and values and links don't interact
 * within one, so the order the store hands them back in cannot change the answer.
 */
export function verify(translation: Translation): {
  discrepancies: Discrepancy[]
  warnings: Warning[]
  checked: number
} {
  const before = rollupV2(translation.ordered)
  const withBytes = new Set(translation.resources.map((resource) => resource.id))
  const ids = [...new Set([...before.keys(), ...withBytes])]
  const buckets = bucketEvents(ids, translation.events)

  const discrepancies: Discrepancy[] = []
  const warnings: Warning[] = []

  for (const id of ids) {
    const old = before.get(id) ?? {}
    const now = rollupEntity(id, buckets.get(id) ?? [])

    const expected = expectedValues(old, withBytes.has(id))
    if (!isDeepStrictEqual(expected, now.values)) {
      discrepancies.push({ id, field: 'values', expected, actual: now.values })
    }

    const children = asIds(old.outbound)
    if (!isDeepStrictEqual(children, now.outboundLinks)) {
      discrepancies.push({ id, field: 'children', expected: children, actual: now.outboundLinks })
    }

    // v2 wrote both halves of every link, so anything claiming a parent that
    // never claimed it back is a hole in the old data rather than in this.
    for (const parent of asIds(old.inbound)) {
      if (!now.inboundLinks.includes(parent)) {
        warnings.push({
          kind: 'inboundWithoutOutbound',
          detail: `${id} says ${parent} links to it, but ${parent} never wrote that link`,
        })
      }
    }
  }

  return { discrepancies, warnings, checked: ids.length }
}
