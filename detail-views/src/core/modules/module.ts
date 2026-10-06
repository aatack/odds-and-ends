import type { AppEvent } from '../graph/events.ts'
import type { Lens, ModuleView } from '../present.ts'
import type { Blobs, EventStore, Settings } from '../store.ts'
import type { Entity, ItemType, LoadPart } from '../types.ts'

export interface ModuleContext {
  /** What I make. Events written here are never deleted by the app. */
  owned: EventStore
  /** What other services said. Emptied weekly; anything in it can be loaded again. */
  cache: EventStore
  settings: Settings
  blobs: Blobs
  /** Reads entities from both stores, as the UI would see them. */
  lens: Lens
  fetch: typeof fetch
  /** Runs the GitHub CLI and returns what it printed. */
  gh(args: string[]): Promise<string>
  /** Loads part of an entity through the core, as the UI would ask. */
  load(id: string, part: LoadPart, force?: boolean): Promise<void>
  now(): number
}

/**
 * A workflow: one entry in the sidebar. Its `view` works out what is shown
 * and is shared with the renderer; the rest reaches the outside world.
 */
export interface Module {
  view: ModuleView
  /**
   * Fetches one part of an entity into the cache store. The core marks it
   * loaded afterwards; a load that brings other entities' data with it (a
   * conversation's messages) marks those itself.
   */
  load?(id: string, part: LoadPart, type: ItemType): Promise<void>
  /** Does one of `view.actions`, confirmed, with whatever was typed. Returns the owned events written. */
  perform?(entity: Entity, action: string, text: string): Promise<AppEvent[]>
  /** Loads further back, on demand (only where `view.older` says it can). */
  older?(id: string): Promise<void>
  /** What the composer does. Returns the owned events written. */
  submit?(entity: Entity, text: string): Promise<AppEvent[]>
}
