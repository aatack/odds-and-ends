import type { AppEvent } from './graph/events.ts'

/**
 * Shared with the renderer. So are `graph/`, `present.ts` and each module's
 * `view.ts`: everything else in core is node, reached only through
 * `Core.actions`.
 */

/**
 * Links with this scheme are mentions: `mention:<user or channel id>/<entity
 * to open>`, the entity left empty when there is nothing to open.
 */
export const mentionScheme = 'mention:'


/**
 * Every kind of item. Each has three views in the renderer (full, row, pill;
 * `views/kinds.tsx`), and the type checker holds the registry to this list.
 */
export const itemTypes = [
  'slack.home',
  'slack.conversation',
  'slack.message',
  'slack.user',
  'slack.watch',
  'github.home',
  'github.pr',
  'github.check',
  'github.item',
  'github.localApproval',
  'tasks.home',
  'task',
] as const

export type ItemType = (typeof itemTypes)[number]

/**
 * An item as the views see it: an entity's values, with `type` lifted out and
 * whatever its module works out for showing it added (`ModuleView.present`).
 */
export interface Entity {
  id: string
  type: ItemType
  data: Record<string, unknown>
  createdAt: number
  updatedAt: number
}

/**
 * What of an entity is loaded from another service: its own fields (`self`),
 * or what sits under it (`children`), loaded only once something walks into it.
 */
export type LoadPart = 'self' | 'children'

/** Each part loaded from elsewhere, and how many ms it stays fresh. */
export type Freshness = Partial<Record<LoadPart, number>>

export interface LoadRequest {
  id: string
  part: LoadPart
  /** Load even if it is still fresh: a refresh I asked for. */
  force?: boolean
}

export interface LoadResult {
  error: string | null
}

/**
 * The value, on the entity a load was for, saying when that part was last
 * loaded. Kept in the cache store with what it loaded, so clearing one clears
 * the other.
 */
export const loadedKey = (part: LoadPart): string => `loaded.${part}`

/** What the composer under a focus view does, if there is one. */
export type ComposeKind = 'slack' | 'slack-token' | 'task'

/**
 * Something a person can do to an entity. Starting it opens a prompt; Enter
 * confirms (with any text typed), Escape cancels.
 */
export interface Action {
  id: string
  label: string
  prompt: string
  /** Why it can't be done now (already done, say). Shown, but not offered. */
  disabled?: string
}

/**
 * A small status mark beside an item's name, wherever it is mentioned.
 * Worked out by the module, so every view agrees.
 */
export interface Badge {
  shape: 'dot' | 'tick' | 'cross'
  tone: 'red' | 'yellow' | 'green' | 'purple'
  /** What it means, for a tooltip. */
  reason: string
}

export interface Focus {
  entity: Entity | null
  /** Ordered and shaped by the owning module. */
  children: Entity[]
  module: string | null
  compose: ComposeKind | null
  actions: Action[]
  /** Whether more can be loaded from further back, on demand. */
  older: boolean
  loading: boolean
  error: string | null
}

export interface ModuleInfo {
  id: string
  name: string
  root: string
}

/**
 * The entity for a GitHub pull request URL, whatever page of it the link
 * points at; null for anything else. A PR is tracked by its URL.
 */
export function prEntityId(url: string): string | null {
  const match = /^https:\/\/github\.com\/([^/]+)\/([^/]+)\/pull\/(\d+)(?:[/?#]|$)/.exec(url)
  return match ? `github:pr:https://github.com/${match[1]}/${match[2]}/pull/${match[3]}` : null
}

/** What doing something produced: the owned events written, or why it failed. */
export interface Outcome {
  events: AppEvent[]
  error: string | null
}
