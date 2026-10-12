import type { ModuleView } from '../../present.ts'
import type { Entity } from '../../types.ts'

export const claudeIds = { root: 'claude' }

/** Chats, the most recently active first: a prompt starting or an answer coming moves one to the top. */
export function byRecent(sessions: Entity[]): Entity[] {
  return [...sessions].sort((a, b) => b.updatedAt - a.updatedAt)
}

/**
 * Chats in the order Tab goes through them: newest made first. Fixed, unlike
 * the list's (a prompt would move this chat), so Shift+Tab undoes Tab.
 */
export function byMade(sessions: Entity[]): Entity[] {
  return [...sessions].sort((a, b) => b.createdAt - a.createdAt)
}

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
  // A chat's messages show the notes under them without opening them.
  openByDefault: (type) => type === 'claude.home' || type === 'claude.prompt' || type === 'claude.response',
  // A session reads like chat: the newest at the bottom, where the cursor starts.
  newestFirst: (type) => type === 'claude.session',
  compose: (entity) => (entity.type === 'claude.session' ? 'claude' : null),
  order: (entity, children) => (entity.type === 'claude.home' ? byRecent(children) : children),
  present(entity, lens) {
    const own = typeof entity.data.text === 'string' && entity.data.text ? entity.data.text : undefined
    if (entity.type === 'claude.home') return { ...entity, data: { ...entity.data, text: own ?? 'Claude' } }
    if (entity.type === 'claude.session') {
      // The newest message, for the list of chats: its text, and whether it was mine.
      const last = lens.children(entity.id).map((id) => lens.read(id)).findLast((item) => item?.type === 'claude.prompt' || item?.type === 'claude.response')
      if (!last) return entity
      const text = typeof last.data.text === 'string' ? last.data.text : ''
      return { ...entity, data: { ...entity.data, lastText: text, lastMine: last.type === 'claude.prompt' } }
    }
    return entity
  },
}
