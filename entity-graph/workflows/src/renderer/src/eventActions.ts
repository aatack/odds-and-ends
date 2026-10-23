// What can be done to an event, by its type and state: the one list both the
// view header (as icon buttons) and the right-click menu draw.

import { atom } from 'jotai'
import { CircleCheck, CircleX, ExternalLink, Paperclip, SkipForward, type LucideIcon } from 'lucide-react'
import { api } from './useWorkflows'

export interface EventAction {
  id: string
  label: string
  /** Shown on hover, where the label alone says too little. */
  title?: string
  icon: LucideIcon
  /** Its colour, as utility classes written out whole (hover included), so Tailwind finds them. */
  tone: string
  run: () => void
}

/** The event being attached to a task, while the picker for it is up. */
export const attachingAtom = atom<{ id: string; summary: string } | null>(null)

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * The actions on an event: an open task can be resolved, done outside or
 * skipped; any other open event cancelled; anything attached to a task.
 */
export function eventActions(
  e: { id: string; type: string; resolved: boolean; summary: string },
  ctx: { act: Act; skip: (id: string) => void; attach: (e: { id: string; summary: string }) => void },
): EventAction[] {
  const own: EventAction[] =
    e.resolved
      ? []
      : e.type === 'item'
        ? [
            { id: 'resolve', label: 'Resolve', icon: CircleCheck, tone: 'text-success-600 hover:text-success-600', run: () => void ctx.act(() => api().close(e.id)) },
            {
              id: 'outside',
              label: 'Done outside',
              title: 'Done outside: you did it yourself, outside this tool.',
              icon: ExternalLink,
              tone: 'text-brand-600 hover:text-brand-600',
              run: () => void ctx.act(() => api().handledOutside(e.id)),
            },
            { id: 'skip', label: 'Skip', icon: SkipForward, tone: 'text-warning-700 hover:text-warning-700', run: () => ctx.skip(e.id) },
          ]
        : [{ id: 'cancel', label: 'Cancel', icon: CircleX, tone: 'text-error-600 hover:text-error-600', run: () => void ctx.act(() => api().cancel(e.id)) }]
  return [
    ...own,
    { id: 'attach', label: 'Attach to a task', icon: Paperclip, tone: '', run: () => ctx.attach({ id: e.id, summary: e.summary }) },
  ]
}
