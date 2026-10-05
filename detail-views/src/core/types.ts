/** Shared with the renderer, which imports types only. */

export interface Entity {
  id: string
  type: string
  data: Record<string, unknown>
  createdAt: number
  updatedAt: number
  /** Null for owned data; set for anything cached from another service. */
  expiresAt: number | null
}

/**
 * Message text split for display: plain runs, and mentions that open the
 * entity they name (null when the app has nothing to open).
 */
export type TextPart = string | { mention: string; target: string | null }

/** What the composer under a focus view does, if there is one. */
export type ComposeKind = 'slack' | 'slack-token' | 'task'

export interface Focus {
  entity: Entity | null
  /** Ordered and shaped by the owning module. */
  children: Entity[]
  module: string | null
  compose: ComposeKind | null
  loading: boolean
  error: string | null
}

export interface ModuleInfo {
  id: string
  name: string
  root: string
}
