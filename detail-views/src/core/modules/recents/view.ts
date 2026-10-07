import type { ModuleView } from '../../present.ts'

export const recentsIds = { root: 'recents' }

/**
 * What I have looked at lately: the items I pushed a view of, newest first.
 * The links live in the cache store (temporary), re-written at the time of
 * each visit, so a visit moves an item to the front.
 */
export const recentsView: ModuleView = {
  id: 'recents',
  name: 'Recents',
  root: recentsIds.root,
  typeOf: (id) => (id === recentsIds.root ? 'recents.home' : null),
  owns: (type) => type === 'recents.home',
  // Links are read oldest first; the list reads newest first.
  order: (entity, children) => (entity.type === 'recents.home' ? [...children].reverse() : children),
  present: (entity) => ({ ...entity, data: { ...entity.data, text: entity.data.text ?? 'Recents' } }),
}
