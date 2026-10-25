// The walk, computed in the browser from the cache. It reads the tree the same
// way as the server's `walk`: depth first, children in link order, never
// through an entity already on the path, never into an untyped entity.
//
// Rows are counted from an *anchor*: a path in the tree. Row 0 is the first
// row at or after the anchor; row -1 is the row just before it. A window asks
// for rows [offset, offset + limit). The engine steps forward from the anchor
// for the rows at and after it, and backward for the rows before it, so the
// cost of a window is its size, not its distance from the top of the tree.
//
// Where the cache lacks what a step needs, the engine does not wait: the row
// shows as loading, the walk carries on past it, and the window says where a
// preload must start (`missing`). See PARADIGM.md.

import { isTyped, otherEnd, rulesOf, passes, groupKey, TYPE_KEY, type EntityView } from './cache.js'
import type { Direction, EntityId, Filter, Follow, Path, Values } from './types.js'

export interface WalkQuery {
  /** Which links to follow out of an entity, by its type. */
  follow: Follow
  /** The walk goes below a node only when this matches it. */
  descend?: Filter
  /** A node is a row only when this matches it. */
  include?: Filter
  /** Paths that the walk does not go below (rows the user has folded). */
  collapsed?: readonly Path[]
}

export interface TreeRow {
  /** The path joined into one string: a stable key for React. */
  key: string
  path: EntityId[]
  id: EntityId
  /** Levels below the root: the root row is 0. */
  depth: number
  /** The entity's values; `undefined` while they are loading. */
  values: Values | undefined
  /** `loading` while the values are not in the cache. */
  status: 'ready' | 'loading'
  /** The walk goes below this row, but its links are not in the cache yet. */
  childrenLoading: boolean
  /** Whether the row has children to show if expanded; `undefined` when not known yet. */
  hasChildren: boolean | undefined
  /** Whether the row is in `collapsed`. */
  collapsed: boolean
}

export interface WindowRequest {
  /** The walk's root. Rows are its last entity and that entity's subtree. */
  root: Path
  /** Where rows are counted from. Default: the root. */
  anchor?: Path
  query: WalkQuery
  /** Index of the first row wanted, relative to the anchor. May be negative. */
  offset: number
  /** Number of rows wanted. */
  limit: number
  /** Rows past each end of the window to check for missing data. Default 50. */
  lookahead?: number
  /** Most nodes to step over in each direction. Default 100 000. */
  maxSteps?: number
}

export interface WalkWindow {
  /** The rows in [offset, offset + limit) that exist, in order. */
  rows: TreeRow[]
  /** Index of `rows[0]`, relative to the anchor. */
  first: number
  /** The anchor that the rows are counted from (the requested one, cut short where it no longer exists). */
  anchor: EntityId[]
  /** No rows exist before `rows[0]`. */
  atStart: boolean
  /** No rows exist after the last of `rows`. */
  atEnd: boolean
  /** Rows on each side of the anchor that the engine computed (window plus lookahead). */
  counted: { before: number; after: number }
  /** Where to start a preload, when the cache lacks data near the window. */
  missing: { forward?: EntityId[]; backward?: EntityId[] }
}

export const pathKey = (path: Path): string => path.join('\u0000')

function startsWith(path: Path, prefix: Path): boolean {
  return prefix.length <= path.length && prefix.every((id, i) => path[i] === id)
}

interface Children {
  /** The children the walk goes to: empty when collapsed or `descend` says no. */
  kids: EntityId[]
  /** All visible children, ignoring collapse; `undefined` when not loaded. */
  all: EntityId[] | undefined
  /** The walk would go below this node but cannot: values or links are missing. */
  missing: boolean
  collapsed: boolean
}

/** One computation over a fixed view of the cache. */
export class TreeStepper {
  private readonly collapsed: Set<string>
  private readonly memo = new Map<string, Children>()

  constructor(
    private readonly view: EntityView,
    readonly root: Path,
    private readonly query: WalkQuery,
  ) {
    this.collapsed = new Set((query.collapsed ?? []).map(pathKey))
  }

  children(path: Path): Children {
    const key = pathKey(path)
    let found = this.memo.get(key)
    if (!found) {
      found = this.read(path, key)
      this.memo.set(key, found)
    }
    return found
  }

  private read(path: Path, key: string): Children {
    const id = path[path.length - 1]!
    const record = this.view.get(id)
    const collapsed = this.collapsed.has(key)
    if (!record?.values) return { kids: [], all: undefined, missing: true, collapsed }
    const values = record.values
    const goes = !collapsed && passes(this.query.descend, id, values)
    const all: EntityId[] = []
    const seen = new Set<EntityId>()
    // Link group by link group, in the order the rules first name each; a
    // link is followed when a rule of its group accepts the child's type.
    const groups = new Map<string, { direction: Direction; types: Set<string>; any: boolean }>()
    for (const [linkType, direction, childType] of rulesOf(this.query.follow, values)) {
      const key = groupKey(linkType, direction)
      if (!groups.has(key)) groups.set(key, { direction, types: new Set(), any: false })
      const group = groups.get(key)!
      if (childType === null) group.any = true
      else group.types.add(childType)
    }
    for (const [key, { direction, types, any }] of groups) {
      const links = record.links[key] ?? (record.local ? [] : undefined)
      if (!links) return { kids: [], all: undefined, missing: goes, collapsed }
      for (const link of links) {
        const other = otherEnd(link, direction)
        if (seen.has(other) || path.includes(other)) continue
        // The server hides untyped entities and follows only the rules' child
        // types; the cache knows an entity's type only when it is loaded, so
        // an unloaded one is shown (and loads).
        const values = this.view.get(other)?.values
        if (values && (!isTyped(values) || !(any || types.has(values[TYPE_KEY] as string)))) continue
        seen.add(other)
        all.push(other)
      }
    }
    return { kids: goes ? all : [], all, missing: false, collapsed }
  }

  row(path: Path): TreeRow {
    const id = path[path.length - 1]!
    const values = this.view.get(id)?.values
    const children = this.children(path)
    return {
      key: pathKey(path),
      path: [...path],
      id,
      depth: path.length - this.root.length,
      values,
      status: values ? 'ready' : 'loading',
      childrenLoading: !!values && children.missing,
      hasChildren: children.all && children.all.length > 0,
      collapsed: children.collapsed,
    }
  }

  emits(path: Path): boolean {
    const id = path[path.length - 1]!
    const values = this.view.get(id)?.values
    // A loading node cannot be tested. With no filter it shows (as loading);
    // with one it waits, rather than show a row that will likely go again.
    if (!values) return !this.query.include
    return passes(this.query.include, id, values)
  }

  /** Whether the node, or the links below it that the walk needs, are not loaded. */
  lacks(path: Path): boolean {
    return !this.view.get(path[path.length - 1]!)?.values || this.children(path).missing
  }

  /**
   * The next path in reading order; `null` at the end. `{ blocked }` when an
   * ancestor's links are missing, so the walk cannot know what comes next.
   */
  next(path: Path): EntityId[] | null | { blocked: EntityId[] } {
    const below = this.children(path).kids
    if (below.length > 0) return [...path, below[0]!]
    for (let i = path.length - 1; i >= this.root.length; i--) {
      const parent = path.slice(0, i)
      const siblings = this.children(parent)
      if (siblings.missing) return { blocked: parent }
      const at = siblings.kids.indexOf(path[i]!)
      if (at >= 0 && at + 1 < siblings.kids.length) return [...parent, siblings.kids[at + 1]!]
    }
    return null
  }

  /**
   * The previous path in reading order; `null` at the root. `{ blocked }`
   * when the parent's links are missing. `incomplete` is set when the result
   * is a node whose subtree is not loaded (its last descendant is not known).
   */
  previous(path: Path): { path: EntityId[]; incomplete: boolean } | null | { blocked: EntityId[] } {
    if (path.length <= this.root.length) return null
    const parent = path.slice(0, -1)
    const siblings = this.children(parent)
    if (siblings.missing) return { blocked: parent }
    const at = siblings.kids.indexOf(path[path.length - 1]!)
    if (at <= 0) return { path: parent, incomplete: false }
    let at_ = [...parent, siblings.kids[at - 1]!]
    for (;;) {
      const below = this.children(at_)
      if (below.missing) return { path: at_, incomplete: true }
      if (below.kids.length === 0) return { path: at_, incomplete: false }
      at_ = [...at_, below.kids[below.kids.length - 1]!]
    }
  }

  /**
   * The anchor as it stands now: the requested path, cut short at the first
   * step that no longer exists (a removed link, a collapsed ancestor). `blocked`
   * is set when the cut is because data is missing.
   */
  resolve(anchor: Path | undefined): { path: EntityId[]; blocked?: EntityId[] } {
    if (!anchor || !startsWith(anchor, this.root)) return { path: [...this.root] }
    const path = [...this.root]
    for (let i = this.root.length; i < anchor.length; i++) {
      const children = this.children(path)
      if (children.missing) return { path, blocked: [...path] }
      if (!children.kids.includes(anchor[i]!)) break
      path.push(anchor[i]!)
    }
    return { path }
  }
}

/** Compute the rows of one window from the cache. Pure: the same view gives the same rows. */
export function computeWindow(view: EntityView, request: WindowRequest): WalkWindow {
  const { root, query, offset, limit } = request
  const lookahead = request.lookahead ?? 50
  const maxSteps = request.maxSteps ?? 100_000
  const stepper = new TreeStepper(view, root, query)
  const missing: WalkWindow['missing'] = {}
  const resolved = stepper.resolve(request.anchor)
  const anchor = resolved.path
  if (resolved.blocked) missing.forward = resolved.blocked

  // Forward: rows 0, 1, 2, ... from the anchor.
  const after: TreeRow[] = []
  let atEnd = false
  const wantAfter = Math.max(0, offset + limit) + lookahead
  if (offset + limit > 0) {
    let path: EntityId[] | null = anchor
    for (let steps = 0; path && after.length < wantAfter && steps < maxSteps; steps++) {
      if (!missing.forward && stepper.lacks(path)) missing.forward = path
      if (stepper.emits(path)) after.push(stepper.row(path))
      const next: ReturnType<TreeStepper['next']> = stepper.next(path)
      if (next && !Array.isArray(next)) {
        missing.forward ??= path
        path = null
        break
      }
      path = next
      if (!path) atEnd = true
    }
  }

  // Backward: rows -1, -2, ... before the anchor, nearest first.
  const before: TreeRow[] = []
  let atStart = false
  const wantBefore = Math.max(1, -offset + lookahead)
  if (offset <= 0) {
    let path: EntityId[] = anchor
    for (let steps = 0; before.length < wantBefore && steps < maxSteps; steps++) {
      const previous = stepper.previous(path)
      if (previous === null) {
        atStart = true
        break
      }
      if ('blocked' in previous) {
        missing.backward ??= path
        break
      }
      if (!missing.backward && (previous.incomplete || stepper.lacks(previous.path))) missing.backward = path
      if (stepper.emits(previous.path)) before.push(stepper.row(previous.path))
      path = previous.path
    }
  }

  const all = [...before.reverse(), ...after]
  const zero = before.length
  const lowest = zero === 0 ? 0 : -zero
  const from = Math.max(offset, lowest)
  const to = Math.min(offset + limit, after.length)
  const rows = to > from ? all.slice(zero + from, zero + to) : []
  return {
    rows,
    first: rows.length > 0 ? from : offset,
    anchor,
    // Data still loading can add rows, so an end is only an end with nothing missing.
    atStart: atStart && !missing.backward && from <= lowest,
    atEnd: atEnd && !missing.forward && offset + limit >= after.length,
    counted: { before: before.length, after: after.length },
    missing,
  }
}

/**
 * The number of rows before the anchor, stepping back to the root. `complete`
 * is false when data is missing or `maxSteps` ran out: then it is a lower
 * bound. The viewport uses it to renumber the rows in one step.
 */
export function countBefore(
  view: EntityView,
  request: Pick<WindowRequest, 'root' | 'anchor' | 'query' | 'maxSteps'>,
): { count: number; complete: boolean } {
  const stepper = new TreeStepper(view, request.root, request.query)
  const maxSteps = request.maxSteps ?? 100_000
  let path: EntityId[] = stepper.resolve(request.anchor).path
  let count = 0
  let complete = true
  for (let steps = 0; ; steps++) {
    if (steps >= maxSteps) return { count, complete: false }
    const previous = stepper.previous(path)
    if (previous === null) return { count, complete }
    if ('blocked' in previous) return { count, complete: false }
    if (previous.incomplete || stepper.lacks(previous.path)) complete = false
    if (stepper.emits(previous.path)) count++
    path = previous.path
  }
}
