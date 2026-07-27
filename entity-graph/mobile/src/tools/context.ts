import { entityRows, viewRows } from '../state/derive'
import { childOrder } from '../state/query'
import { getView } from '../state/store'
import { last, topLevel } from '../state/types'
import type { ToolContext } from './types'

/**
 * The context a call is born with, read off the current state and cache.
 *
 * `extra` is what a gesture supplies that the selection doesn't — the row that was
 * long-pressed, which need not be the row that is selected.
 */
export function currentContext(extra: Record<string, unknown> = {}): ToolContext {
  const view = getView()
  const level = topLevel(view)
  const { rows, selectedPath } = viewRows(view)
  const entities = entityRows(rows)

  const entityId = last(selectedPath) ?? null
  const parentId = selectedPath.length > 1 ? selectedPath[selectedPath.length - 2] : null
  const row = entities.find((r) => r.selected) ?? null

  return {
    rootId: level.rootId,
    direction: level.direction,
    entityId,
    parentId,
    path: selectedPath,
    row,
    rows: entities,
    values: {
      ...(entityId ? { entityId } : {}),
      ...(parentId ? { parentId } : {}),
      rootId: level.rootId,
      ...extra,
    },
  }
}

/**
 * The ordered children of an entity, as the user is looking at them. Empty in a
 * reversed level: inbound links have no order of their own, so there is nothing to
 * insert between.
 */
export const siblingsUnder = (ctx: ToolContext, parentId: string | null): string[] =>
  parentId && ctx.direction === 'out' ? childOrder(parentId) : []

/** The row before the selection among its siblings — what indent moves under. */
export function previousSibling(ctx: ToolContext): string | null {
  if (!ctx.entityId || !ctx.parentId) return null
  const siblings = siblingsUnder(ctx, ctx.parentId)
  const at = siblings.indexOf(ctx.entityId)
  return at > 0 ? siblings[at - 1] : null
}

/**
 * Where the selection goes when the row it sits on leaves the screen: the row
 * immediately above, which is where the eye already is. Null when there is none,
 * where falling back on the parent is all there is.
 */
export function rowAbove(ctx: ToolContext): string[] | null {
  const at = ctx.rows.findIndex((r) => r.selected)
  return at > 0 ? ctx.rows[at - 1].path : null
}

/** An id argument that must be present: blank writes conjure phantom entities. */
export function requireId(v: unknown, label: string): string {
  const value = String(v ?? '').trim()
  if (!value) throw new Error(`${label} is required`)
  return value
}

export const asId = (v: unknown): string => String(v ?? '')
