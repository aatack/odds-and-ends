import type { ModuleView } from '../../present.ts'

/** Owned data only: things I type in. A task can sit under anything. */
export const tasksView: ModuleView = {
  id: 'tasks',
  name: 'Tasks',
  root: 'tasks',
  typeOf: (id) => (id === 'tasks' ? 'tasks.home' : null),
  owns: (type) => type === 'tasks.home' || type === 'task',
  order: (_entity, children) =>
    [...children].sort((a, b) => Number(a.type === 'task' && Boolean(a.data.done)) - Number(b.type === 'task' && Boolean(b.data.done))),
  compose: () => 'task',
}
