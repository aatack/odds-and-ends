import type { ModuleView } from '../../present.ts'

export const claudeIds = { root: 'claude' }

/**
 * Claude sessions (`claude -p`): each a session item (its id is Claude's
 * session id), under the item it was started from and under the Claude root.
 * Under a session, and under the item each prompt was asked from, are its
 * prompts; under each prompt, Claude's response.
 */
export const claudeView: ModuleView = {
  id: 'claude',
  name: 'Claude',
  root: claudeIds.root,
  typeOf: (id) => (id === claudeIds.root ? 'claude.home' : null),
  owns: (type) => type.startsWith('claude.'),
  // A session's prompts, and each prompt's response, show without opening them.
  openByDefault: (type) => type === 'claude.home' || type === 'claude.prompt',
  present(entity) {
    const own = typeof entity.data.text === 'string' && entity.data.text ? entity.data.text : undefined
    if (entity.type === 'claude.home') return { ...entity, data: { ...entity.data, text: own ?? 'Claude' } }
    return entity
  },
}
