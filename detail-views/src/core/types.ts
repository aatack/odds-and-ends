/** Shared with the renderer, which imports nothing else from core. */

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
  'github.home',
  'github.pr',
  'github.check',
  'github.item',
  'github.localApproval',
  'tasks.home',
  'task',
] as const

export type ItemType = (typeof itemTypes)[number]

export interface Entity {
  id: string
  type: ItemType
  data: Record<string, unknown>
  createdAt: number
  updatedAt: number
  /** Null for owned data; set for anything cached from another service. */
  expiresAt: number | null
}

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
}

/**
 * A small status mark beside an item's name, wherever it is mentioned.
 * Worked out by the module, so every view agrees.
 */
export interface Badge {
  shape: 'dot' | 'tick' | 'cross'
  tone: 'red' | 'yellow' | 'green'
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
