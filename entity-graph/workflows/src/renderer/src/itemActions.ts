// What can be done to an item, by its type and state: the one list both the
// view header (as icon buttons, the rest in the menu beside them) and the
// right-click menu draw.

import { CircleCheck, CircleX, RotateCcw, SkipForward, type LucideIcon } from 'lucide-react'
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

/**
 * The actions on an item: an open task can be resolved or skipped, a closed one reopened; any other
 * open item cancelled (a bot is deleted from its settings, not here). Then the person's own actions for its
 * type (`custom`), from the Types page.
 */
export function itemActions(
  e: { id: string; type: string; resolved: boolean; summary: string },
  ctx: { act: Act; skip: (id: string) => void; custom?: CustomAction[] },
): ItemAction[] {
  const own: ItemAction[] = e.resolved
    ? e.type === 'task'
      ? [{ id: 'reopen', label: 'Reopen', important: true, icon: RotateCcw, tone: '', run: () => void ctx.act(() => api().reopen(e.id)) }]
      : []
    : e.type === 'task'
      ? [
          { id: 'resolve', label: 'Resolve', important: true, icon: CircleCheck, tone: 'text-success-600 hover:text-success-600', run: () => void ctx.act(() => api().close(e.id)) },
          { id: 'skip', label: 'Skip', important: true, icon: SkipForward, tone: 'text-warning-700 hover:text-warning-700', run: () => ctx.skip(e.id) },
        ]
      : e.type === 'user' || e.type === 'bot'
        ? []
        : [
            {
              id: 'cancel',
              label: 'Cancel',
              important: true,
              icon: CircleX,
              tone: 'text-error-600 hover:text-error-600',
              run: () => void ctx.act(() => api().cancel(e.id)),
            },
          ]
  return [
    ...own,
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
