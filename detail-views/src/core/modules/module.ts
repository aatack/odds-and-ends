import type { Store } from '../store.ts'
import type { Action, ComposeKind, Entity } from '../types.ts'

export interface ModuleContext {
  store: Store
  fetch: typeof fetch
  /** Runs the GitHub CLI and returns what it printed. */
  gh(args: string[]): Promise<string>
  /** Reports a failure against an entity; the focus view shows it. Null clears it. */
  setError(id: string, error: string | null): void
}

/** A workflow: one entry in the sidebar, owning a family of entity types. */
export interface Module {
  id: string
  name: string
  /** The entity the sidebar entry focuses. Created owned on first start. */
  root: { id: string; type: string }
  /** Whether this module answers for an entity. */
  owns(entity: Entity): boolean
  /** Makes an entity for an id seen only as a reference (a link, say), if it can. */
  materialise?(id: string): Entity | null
  /** Brings cached children of `id` up to date. Optional for owned-only modules. */
  refresh?(id: string): Promise<void>
  /** How long a refresh of `id` stays fresh, in ms. */
  staleAfter?(id: string): number
  /** Ordering of children in the focus view. Default is link rank. */
  order?(entity: Entity, children: Entity[]): Entity[]
  /** Fills in display fields that are derived, never stored (names, say). */
  present?(entity: Entity): Entity
  compose?(entity: Entity): ComposeKind | null
  /** What can be done to an entity right now. */
  actions?(entity: Entity): Action[]
  /** Does one of `actions`, confirmed, with whatever text was typed. */
  perform?(entity: Entity, action: string, text: string): Promise<void>
  /** What the composer does. */
  submit?(entity: Entity, text: string): Promise<void>
}
