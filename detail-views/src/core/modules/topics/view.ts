import type { Lens, ModuleView } from '../../present.ts'
import { byRecent } from '../claude/view.ts'

export const topicsIds = { root: 'topics' }

/**
 * The messages of a topic not yet sent to Claude: the notes under it after
 * its newest response (each dispatch puts one there), in order, by id.
 */
export function unsent(topic: string, lens: Lens): string[] {
  const chat = lens.children(topic)
  const last = chat.findLastIndex((id) => lens.read(id)?.type === 'claude.response')
  return chat.slice(last + 1).filter((id) => {
    const item = lens.read(id)
    return item?.type === 'note' && typeof item.data.text === 'string' && item.data.text.trim() !== ''
  })
}

/**
 * Topics: one global list, under the `topics` root, each a topic item (its
 * name its text). Under a topic is its chat: my messages (notes), and,
 * after each dispatch, Claude's response to every message since the one
 * before. A topic is also the Claude session they run in: its id is the
 * session's, and its `cwd` where it runs.
 */
export const topicsView: ModuleView = {
  id: 'topics',
  name: 'Topics',
  root: topicsIds.root,
  typeOf: (id) => (id === topicsIds.root ? 'topics.home' : null),
  owns: (type) => type === 'topics.home' || type === 'topic',
  // A topic reads like chat: the newest at the bottom, where the cursor starts.
  newestFirst: (type) => type === 'topic',
  compose: (entity) => (entity.type === 'topic' ? 'topic' : null),
  // The most recently active first: a message, a dispatch or an answer moves one to the top.
  order: (entity, children) => (entity.type === 'topics.home' ? byRecent(children) : children),
  present: (entity) => (entity.type === 'topics.home' ? { ...entity, data: { ...entity.data, text: entity.data.text ?? 'Topics' } } : entity),
}
