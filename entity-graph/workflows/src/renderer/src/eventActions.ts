// What can be done to an event, by its type and state: the one list both the
// view header (as icon buttons) and the right-click menu draw.

import { atom } from 'jotai'
import { CircleCheck, CircleX, ExternalLink, Flag, FlagOff, Paperclip, SkipForward, Trash2, type LucideIcon } from 'lucide-react'
import type { CustomAction } from '../../shared/types'
import { actionIcon } from './actionIcons'
import { api } from './useWorkflows'

export interface EventAction {
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

/** The event being attached to a task, while the picker for it is up. */
export const attachingAtom = atom<{ id: string; summary: string } | null>(null)

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * The actions on an event: an open task can be resolved, done outside,
 * skipped, or marked as a priority (`priority` is whether it is one); a directive
 * deleted; any other open event cancelled; anything attached to a task. Then the human's own
 * actions for its type (`custom`), from the Types page.
 */
export function eventActions(
  e: { id: string; type: string; resolved: boolean; summary: string; priority?: boolean },
  ctx: { act: Act; skip: (id: string) => void; attach: (e: { id: string; summary: string }) => void; custom?: CustomAction[] },
): EventAction[] {
  const own: EventAction[] =
    e.resolved
      ? []
      : e.type === 'item'
        ? [
            { id: 'resolve', label: 'Resolve', important: true, icon: CircleCheck, tone: 'text-success-600 hover:text-success-600', run: () => void ctx.act(() => api().close(e.id)) },
            {
              id: 'outside',
              important: true,
              label: 'Done outside',
              title: 'Done outside: you did it yourself, outside this tool.',
              icon: ExternalLink,
              tone: 'text-brand-600 hover:text-brand-600',
              run: () => void ctx.act(() => api().handledOutside(e.id)),
            },
            { id: 'skip', label: 'Skip', important: true, icon: SkipForward, tone: 'text-warning-700 hover:text-warning-700', run: () => ctx.skip(e.id) },
            e.priority
              ? { id: 'priority', label: 'Not a priority', important: false, icon: FlagOff, tone: '', run: () => void ctx.act(() => api().setPriority(e.id, false)) }
              : { id: 'priority', label: 'Mark as a priority', important: false, icon: Flag, tone: '', run: () => void ctx.act(() => api().setPriority(e.id, true)) },
          ]
        : e.type === 'directive'
          ? [{ id: 'cancel', label: 'Delete', important: true, icon: Trash2, tone: 'text-error-600 hover:text-error-600', run: () => void ctx.act(() => api().cancel(e.id)) }]
          : [{ id: 'cancel', label: 'Cancel', important: true, icon: CircleX, tone: 'text-error-600 hover:text-error-600', run: () => void ctx.act(() => api().cancel(e.id)) }]
  return [
    ...own,
    { id: 'attach', label: 'Attach to a task', icon: Paperclip, tone: '', important: false, run: () => ctx.attach({ id: e.id, summary: e.summary }) },
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
