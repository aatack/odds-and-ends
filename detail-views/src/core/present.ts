import type { Source } from './graph/cache.ts'
import type { GraphEntity } from './graph/entity.ts'
import { githubView } from './modules/github/view.ts'
import { slackView } from './modules/slack/view.ts'
import { tasksView } from './modules/tasks/view.ts'
import { itemTypes } from './types.ts'
import type { Action, ComposeKind, Entity, Focus, Freshness, ItemType, ModuleInfo, View, ViewRow } from './types.ts'

/**
 * How entities become what is on screen. Pure, and shared: the renderer runs
 * it over its entity cache, a headless caller over the stores, and both see
 * the same thing. Nothing here may import node, Electron or React.
 */

/** Reading other entities while presenting one. Reading is asking. */
export interface Lens {
  /** The item at an id, not yet presented; null while nothing is known of it. */
  read(id: string): Entity | null
  /** The ids under an entity, in link order. */
  children(id: string): string[]
}

/** The half of a module that only works things out: no network, no store. */
export interface ModuleView {
  id: string
  name: string
  root: string
  /** The type an id has by its shape alone, before anything about it has loaded. */
  typeOf(id: string): ItemType | null
  owns(type: ItemType): boolean
  /** What of an entity comes from another service, and how long each part stays fresh. */
  foreign?(id: string, type: ItemType): Freshness | null
  /** Reads bottom up, like chat: a bounded walk keeps the newest end, and the cursor starts there. */
  newestFirst?(type: ItemType): boolean
  /** Whether a row of this type has its children walked until I fold it. Default: no. */
  openByDefault?(type: ItemType): boolean
  /** Fills in what is worked out rather than stored (names, say). */
  present?(entity: Entity, lens: Lens): Entity
  /** Children in the focus view. Default is link order. */
  order?(entity: Entity, children: Entity[], lens: Lens): Entity[]
  compose?(entity: Entity, lens: Lens): ComposeKind | null
  /** What can be done to an entity right now. */
  actions?(entity: Entity, lens: Lens): Action[]
  /** Whether more of it can be loaded from further back, on demand (`Module.older`). */
  older?(entity: Entity): boolean
}

/** The sidebar, in order. */
export const moduleViews: ModuleView[] = [slackView, githubView, tasksView]

export const moduleInfos: ModuleInfo[] = moduleViews.map(({ id, name, root }) => ({ id, name, root }))

const known = new Set<string>(itemTypes)

export function typeOf(id: string, values: Record<string, unknown>): ItemType | null {
  if (typeof values.type === 'string' && known.has(values.type)) return values.type as ItemType
  for (const view of moduleViews) {
    const type = view.typeOf(id)
    if (type) return type
  }
  return null
}

export function moduleOf(type: ItemType): ModuleView | null {
  return moduleViews.find((view) => view.owns(type)) ?? null
}

/**
 * An entity as an item. One with values but no type is a note: text that
 * means nothing more. One nothing is known about, and whose id says nothing
 * either, is not an item yet (null).
 */
export function toItem(entity: GraphEntity): Entity | null {
  const type = typeOf(entity.id, entity.values) ?? (Object.keys(entity.values).length ? 'note' : null)
  if (!type) return null
  const data: Record<string, unknown> = {}
  for (const [key, value] of Object.entries(entity.values)) {
    if (key !== 'type' && !key.startsWith('loaded.')) data[key] = value
  }
  return { id: entity.id, type, data, createdAt: entity.createdAt, updatedAt: entity.editedAt }
}

/** What the cache should load from elsewhere for an entity. */
export function foreignOf(entity: GraphEntity): Freshness | null {
  const type = typeOf(entity.id, entity.values)
  return type ? (moduleOf(type)?.foreign?.(entity.id, type) ?? null) : null
}

export function lensOf(source: Source): Lens {
  return {
    read: (id) => toItem(source.get([id])[id]),
    children: (id) => source.get([id])[id].outboundLinks,
  }
}

export function present(entity: Entity, lens: Lens): Entity {
  return moduleOf(entity.type)?.present?.(entity, lens) ?? entity
}

/** An item named somewhere (a pill), presented. */
export function itemOf(id: string, source: Source): Entity | null {
  const lens = lensOf(source)
  const entity = lens.read(id)
  return entity && present(entity, lens)
}

/**
 * The most rows a view walks to. `order` sorts the children it reaches, so this
 * sits above any one list that is sorted (Slack's workspace: its conversations
 * and every thread in them).
 */
export const viewLimit = 3000

/**
 * Whether an item's children are walked when it is a row (a view's root
 * always is). Notes and tasks open; anything from a service, whose children
 * are many or loaded on demand, stays shut until opened.
 */
export function openByDefault(type: ItemType): boolean {
  return moduleOf(type)?.openByDefault?.(type) ?? type === 'note'
}

/** Which rows are open: what I have folded or unfolded, by entity id, over each type's default. */
export type Folds = Record<string, boolean>

export interface ViewOptions {
  folds?: Folds
  limit?: number
}

const pathKey = (path: readonly string[]): string => path.join('\0')

/**
 * A view: a query rooted at one entity, walked depth first into a tree of rows
 * (`ViewRow`), bounded by `limit`. The root is the first row. A row's children
 * are walked only while it is open (`Folds`, else its type's default); each
 * row's children are presented and put in order by their parent's module.
 *
 * Walking into a row is what asks for its `children` to be loaded
 * (`Source.expand`), so opening a row is what loads what is under it.
 */
export function viewOf(rootId: string, source: Source, options: ViewOptions = {}): View {
  const lens = lensOf(source)
  const folds = options.folds ?? {}
  const limit = options.limit ?? viewLimit
  const loading = source.pending(rootId) || source.loading(rootId)
  const error = source.error(rootId)
  const raw = lens.read(rootId)
  if (!raw) {
    return {
      root: null,
      rows: [],
      complete: true,
      module: null,
      compose: null,
      actions: [],
      older: false,
      startsAtEnd: false,
      loading,
      error: error ?? (loading ? null : 'not found'),
    }
  }
  const root = present(raw, lens)
  const module = moduleOf(root.type)
  const rows: ViewRow[] = []
  let complete = true

  const walkFrom = (path: string[], entity: Entity, parent: Entity | null, above: Entity | null, depth: number): void => {
    if (rows.length >= limit) {
      complete = false
      return
    }
    const id = entity.id
    const graph = source.get([id])[id]
    const childIds = graph.outboundLinks.filter((child) => !path.includes(child))
    const open = depth === 0 || (folds[id] ?? openByDefault(entity.type))
    rows.push({
      key: pathKey(path),
      path,
      depth,
      entity,
      parent,
      above,
      hasChildren: childIds.length > 0,
      open,
      loading: source.pending(id) || source.loading(id),
    })
    if (!open) return
    source.expand(id)
    const presented = childIds.flatMap((child) => {
      const item = lens.read(child)
      return item ? [present(item, lens)] : []
    })
    let children = moduleOf(entity.type)?.order?.(entity, presented, lens) ?? presented
    // Chat keeps its newest end when the bound bites.
    if (depth === 0 && module?.newestFirst?.(entity.type) && children.length > limit) {
      children = children.slice(children.length - limit)
    }
    let previous: Entity | null = null
    for (const child of children) {
      walkFrom([...path, child.id], child, entity, previous, depth + 1)
      previous = child
    }
  }
  walkFrom([rootId], root, null, null, 0)

  return {
    root,
    rows,
    complete,
    module: module?.id ?? null,
    compose: module?.compose?.(root, lens) ?? null,
    actions: module?.actions?.(root, lens) ?? [],
    older: module?.older?.(root) ?? false,
    startsAtEnd: module?.newestFirst?.(root.type) ?? false,
    loading,
    error,
  }
}

/**
 * What a view shows one level deep: its root and its children, flat. For a
 * caller with no tree to draw (the tests, a headless script).
 */
export function focusOf(id: string, source: Source): Focus {
  const view = viewOf(id, source, { folds: {}, limit: viewLimit + 1 })
  return {
    entity: view.root,
    children: view.rows.filter((row) => row.depth === 1).map((row) => row.entity),
    module: view.module,
    compose: view.compose,
    actions: view.actions,
    older: view.older,
    loading: view.loading,
    error: view.error,
  }
}
