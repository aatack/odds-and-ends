// Builders for write ops, so that writes read as
// `write([op.set('a', 'text', 'Hello'), op.link('root', 'a', 'child')])`.

import { TYPE_KEY } from './cache.js'
import type { EntityId, Op, Sort, Value } from './types.js'

export const op = {
  /** Set one value; `null` clears it. */
  set: (entity: EntityId, key: string, value: Value): Op => ({ set: { entity, key, value } }),
  /** Add or move a link. Without `sort`, it goes after the existing links (it sorts by its time). */
  link: (source: EntityId, destination: EntityId, linkType: string, sort: Sort = 'timestamp'): Op => ({
    link: { source, destination, link_type: linkType, sort },
  }),
  /** Remove a link. */
  unlink: (source: EntityId, destination: EntityId, linkType: string): Op => ({
    link: { source, destination, link_type: linkType, sort: 'removed' },
  }),
  /** Archive an entity: its type becomes null, and links stop showing it. */
  archive: (entity: EntityId): Op => ({ set: { entity, key: TYPE_KEY, value: null } }),
}

/** A sort key between two neighbours' keys, to put a link between them. */
export function between(before: number | undefined, after: number | undefined): Sort {
  if (before === undefined && after === undefined) return 'timestamp'
  if (before === undefined) return { explicit: after! - 1 }
  if (after === undefined) return { explicit: before + 1 }
  return { explicit: (before + after) / 2 }
}
