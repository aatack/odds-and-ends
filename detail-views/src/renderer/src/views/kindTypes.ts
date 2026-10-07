import type { ComponentType, ReactNode } from 'react'
import type { Entity, ItemType } from '../../../core/types.ts'
import type { ViewProps } from './types.ts'

/**
 * An item as a child in a view's tree: some detail, not all. The tree draws
 * the row around it (indent, fold mark, selection); this is what is in it.
 */
export interface RowProps {
  entity: Entity
  /** The row it hangs off here, for a row that reads differently by where it is. */
  parent: Entity | null
  /** The sibling above it, for rows that read on from it (chat). */
  above: Entity | null
  onOpen(id: string): void
  onImage(ref: string | null): void
}

/** An item in a small space: named in text, or heading its own view or a peek. */
export interface PillProps {
  entity: Entity
  /** What to show while the item is still loading, such as the link's text. */
  fallback?: ReactNode
}

/** An item heading a view of itself: more than its row. */
export type OverviewProps = ViewProps & { entity: Entity }

/**
 * The views every item type has:
 * - `Pill`: named inline, or heading a view or a peek.
 * - `Overview`: at the root of a view, above its tree.
 * - `Row`: a child in another item's view.
 * - `Detail` (optional): the whole view, in place of overview and tree.
 */
export interface ItemViews {
  Pill: ComponentType<PillProps>
  Overview: ComponentType<OverviewProps>
  Row: ComponentType<RowProps>
  Detail?: ComponentType<OverviewProps>
}

export type Kinds = Record<ItemType, ItemViews>
