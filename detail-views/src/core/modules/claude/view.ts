import type { ModuleView } from '../../present.ts'

export const claudeIds = { root: 'claude' }

/**
 * Claude sessions (`claude -p`): each a session item (its id is Claude's
 * session id), under the Claude root. Under a session is its chat: each
 * prompt, then Claude's response, in order, with a box for the next prompt
 * below.
 */
export const claudeView: ModuleView = {
  id: 'claude',
  name: 'Claude',
  root: claudeIds.root,
  typeOf: (id) => (id === claudeIds.root ? 'claude.home' : null),
  owns: (type) => type.startsWith('claude.'),
  openByDefault: (type) => type === 'claude.home',
  // A session reads like chat: the newest at the bottom, where the cursor starts.
  newestFirst: (type) => type === 'claude.session',
  compose: (entity) => (entity.type === 'claude.session' ? 'claude' : null),
  present(entity) {
    const own = typeof entity.data.text === 'string' && entity.data.text ? entity.data.text : undefined
    if (entity.type === 'claude.home') return { ...entity, data: { ...entity.data, text: own ?? 'Claude' } }
    return entity
  },
}
