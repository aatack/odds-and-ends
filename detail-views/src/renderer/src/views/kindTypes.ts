import type { ComponentType, ReactNode } from 'react'
import type { Entity, ItemType } from '../../../core/types.ts'
import type { FocusProps } from './types.ts'

/** An item as a child in another item's view: some detail, not all. */
export interface RowProps {
  entity: Entity
  selected: boolean
  onSelect(id: string): void
  onOpen(id: string): void
  onImage(ref: string | null): void
}

/** An item in a small space: named in text, or heading a view or window. */
export interface PillProps {
  entity: Entity
  /** What to show while the item is still loading, such as the link's text. */
  fallback?: ReactNode
}

/**
 * The three views every item type has:
 * - `Full`: the item focused, filling the view.
 * - `Row`: the item as a child in a list.
 * - `Pill`: the item where there is only a little room. Content only; the
 *   pill's frame and gestures come from `ItemPill` / `HeaderPill`.
 */
export interface ItemViews {
  Full: ComponentType<FocusProps>
  Row: ComponentType<RowProps>
  Pill: ComponentType<PillProps>
}

export type Kinds = Record<ItemType, ItemViews>
