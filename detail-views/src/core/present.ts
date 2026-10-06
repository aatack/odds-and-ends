import type { Source } from './graph/cache.ts'
import type { GraphEntity } from './graph/entity.ts'
import { walk } from './graph/walk.ts'
import { githubView } from './modules/github/view.ts'
import { slackView } from './modules/slack/view.ts'
import { tasksView } from './modules/tasks/view.ts'
import { itemTypes } from './types.ts'
import type { Action, ComposeKind, Entity, Focus, Freshness, ItemType, ModuleInfo } from './types.ts'

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
  /** Children are read newest first, so a bounded walk keeps the newest end (chat). */
  newestFirst?(type: ItemType): boolean
  /** Fills in what is worked out rather than stored (names, say). */
  present?(entity: Entity, lens: Lens): Entity
  /** Children in the focus view. Default is link order. */
  order?(entity: Entity, children: Entity[], lens: Lens): Entity[]
  compose?(entity: Entity, lens: Lens): ComposeKind | null
  /** What can be done to an entity right now. */
  actions?(entity: Entity, lens: Lens): Action[]
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

/** An entity as an item: null until something says what type it is. */
export function toItem(entity: GraphEntity): Entity | null {
  const type = typeOf(entity.id, entity.values)
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
 * The most children a focus view walks to. `order` sorts what the walk
 * reached, so this sits above any list that is sorted (Slack's conversations).
 */
export const focusLimit = 1000

/** What the focus view of `id` shows, from whatever `source` has now. */
export function focusOf(id: string, source: Source): Focus {
  const lens = lensOf(source)
  const loading = source.pending(id) || source.loading(id)
  const error = source.error(id)
  const raw = lens.read(id)
  if (!raw) {
    return { entity: null, children: [], module: null, compose: null, actions: [], loading, error: error ?? (loading ? null : 'not found') }
  }
  const view = moduleOf(raw.type)
  source.expand(id)
  const newestFirst = view?.newestFirst?.(raw.type) ?? false
  const walked = walk([id], source.get, { maxDepth: { [id]: 1 }, newestFirst: newestFirst ? [id] : [] }, focusLimit + 1)
  const ids = walked.paths.slice(1).map((path) => path[path.length - 1])
  if (newestFirst) ids.reverse()
  const children = ids.flatMap((child) => {
    const item = lens.read(child)
    return item ? [present(item, lens)] : []
  })
  const entity = present(raw, lens)
  return {
    entity,
    children: view?.order?.(entity, children, lens) ?? children,
    module: view?.id ?? null,
    compose: view?.compose?.(entity, lens) ?? null,
    actions: view?.actions?.(entity, lens) ?? [],
    loading,
    error,
  }
}
