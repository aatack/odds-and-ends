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
  'note',
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

/** What the composer under a view does, if there is one. */
export type ComposeKind = 'slack' | 'slack-token'

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
  tone: 'red' | 'yellow' | 'green' | 'purple' | 'gray'
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

/**
 * One line of a view's tree. A row is identified by its path from the view's
 * root, not by its id: the graph isn't a tree, and an entity can show in more
 * than one place at once.
 */
export interface ViewRow {
  /** The path, as one string. */
  key: string
  path: string[]
  /** 0 for the view's root. */
  depth: number
  /** Presented. */
  entity: Entity
  /** The row it hangs off in this view; null for the root. */
  parent: Entity | null
  /** The sibling before it, for rows that read on from the one above (chat). */
  above: Entity | null
  hasChildren: boolean
  /** Whether its children are walked here. */
  open: boolean
  loading: boolean
}

/** A query rooted at one entity, as a tree of rows. */
export interface View {
  root: Entity | null
  /** In reading order; the root first. */
  rows: ViewRow[]
  /** False when the walk stopped at its limit. */
  complete: boolean
  module: string | null
  compose: ComposeKind | null
  actions: Action[]
  older: boolean
  /** Reads bottom up (chat): the cursor starts at the end. */
  startsAtEnd: boolean
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

/**
 * What a note can be besides its text: a heading (`section`), or a task
 * (`open`: true while unticked, false once ticked; absent for a plain note).
 */
export interface NoteValues {
  section?: boolean
  open?: boolean
}

/** What doing something produced: the owned events written, or why it failed. */
export interface Outcome {
  events: AppEvent[]
  error: string | null
}
