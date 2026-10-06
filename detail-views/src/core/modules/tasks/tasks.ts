import { randomUUID } from 'node:crypto'
import { link, values } from '../../graph/events.ts'
import type { Entity } from '../../types.ts'
import type { Module, ModuleContext } from '../module.ts'
import { tasksView } from './view.ts'

export const me = 'me'

/** Owned data only: things I type in. A task can sit under anything. */
export function tasksModule(context: ModuleContext): Module {
  return {
    view: tasksView,
    async submit(entity: Entity, text: string) {
      const id = randomUUID()
      const now = context.now()
      const events = [...values(id, { type: 'task', text, done: false }, now, me), link(entity.id, id, now, me)]
      context.owned.write(events)
      return events
    },
  }
}
