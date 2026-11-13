// What can be done to an item, by its type and state: the one list both the
// right pane (as buttons; on a phone, the header's menu) and the right-click
// menu's last section draw. Each type's own are part of the type, as its pill,
// card and focus are: `TYPE_ACTIONS`.

import { CircleCheck, RotateCcw, type LucideIcon } from 'lucide-react'
import type { CustomAction } from '../../shared/types'
import { actionIcon } from './actionIcons'
import { api } from './useWorkflows'

export interface ItemAction {
  id: string
  label: string
  /** Shown on hover, where the label alone says too little. */
  title?: string
  icon: LucideIcon
  /** Its colour, as utility classes written out whole (hover included), so Tailwind finds them. */
  tone: string
  /** Shown as a button in the header; the rest are in the menu beside them. */
  important: boolean
  run: () => void
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

type Target = { id: string; type: string; closed: boolean; summary: string; params?: Record<string, unknown> }

/** Each type's own actions, after those every item has (Close, Reopen): none yet. A file's Copy file would go here. */
export const TYPE_ACTIONS: Partial<Record<string, (e: Target, ctx: { act: Act }) => ItemAction[]>> = {}

/**
 * The actions on an item: anything open can be closed (it floats free: its owner and assignees come off it, and come
 * back when it is reopened), anything closed reopened. Then its type's own (`TYPE_ACTIONS`), then the person's own
 * actions for its type (`custom`), from the Types page.
 */
export function itemActions(e: Target, ctx: { act: Act; custom?: CustomAction[] }): ItemAction[] {
  const own: ItemAction[] = e.closed
    ? [{ id: 'reopen', label: 'Reopen', important: true, icon: RotateCcw, tone: '', run: () => void ctx.act(() => api().reopen(e.id)) }]
    : [{ id: 'close', label: 'Close', important: true, icon: CircleCheck, tone: 'text-success-600 hover:text-success-600', run: () => void ctx.act(() => api().close(e.id)) }]
  return [
    ...own,
    ...(TYPE_ACTIONS[e.type]?.(e, ctx) ?? []),
    ...(ctx.custom ?? [])
      .filter((c) => c.type === e.type)
      .map((c) => ({
        id: `custom:${c.id}`,
        label: c.name,
        title: c.description ? `${c.name}: ${c.description}` : c.name,
        icon: actionIcon(c.icon),
        tone: '',
        important: c.important,
        run: () => void ctx.act(() => api().runCustomAction(c.id, e.id)),
      })),
  ]
}
