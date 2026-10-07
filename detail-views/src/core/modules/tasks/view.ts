import type { ModuleView } from '../../present.ts'

/** Owned data only: things I type in. Under it, notes (Enter) and the tasks made before notes. */
export const tasksView: ModuleView = {
  id: 'tasks',
  name: 'Tasks',
  root: 'tasks',
  typeOf: (id) => (id === 'tasks' ? 'tasks.home' : null),
  owns: (type) => type === 'tasks.home' || type === 'task',
  openByDefault: () => true,
  order: (_entity, children) =>
    [...children].sort((a, b) => Number(a.type === 'task' && Boolean(a.data.done)) - Number(b.type === 'task' && Boolean(b.data.done))),
  present: (entity) => (entity.type === 'tasks.home' ? { ...entity, data: { ...entity.data, text: entity.data.text ?? 'Tasks' } } : entity),
}
