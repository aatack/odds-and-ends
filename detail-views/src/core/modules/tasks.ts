import { randomUUID } from 'node:crypto'
import type { Store } from '../store.ts'
import type { Entity } from '../types.ts'
import type { Module } from './module.ts'

/** Owned data only: things I type in. A task can sit under anything. */
export function tasksModule(store: Store): Module {
  return {
    id: 'tasks',
    name: 'Tasks',
    root: { id: 'tasks', type: 'tasks.home' },
    owns: (entity) => entity.type === 'tasks.home' || entity.type === 'task',
    order: (_entity, children) =>
      [...children].sort((a, b) => Number(Boolean(a.data.done)) - Number(Boolean(b.data.done))),
    compose: () => 'task',
    async submit(entity: Entity, text: string) {
      const id = randomUUID()
      store.transaction(() => {
        store.put(id, 'task', { text, done: false })
        store.link(entity.id, id)
      })
    },
  }
}
