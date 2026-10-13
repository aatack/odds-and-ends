import type { Lens, ModuleView } from '../../present.ts'
import type { Entity } from '../../types.ts'
import { byRecent } from '../claude/view.ts'

export const topicsIds = { root: 'topics' }

/**
 * What of a topic is not yet sent to Claude: everything under it after its
 * newest response (each dispatch puts one there), in order, by id: my
 * messages (notes, empty ones apart), and whatever was pasted in (a file, a
 * PR, a Slack message, …).
 */
export function unsent(topic: string, lens: Lens): string[] {
  const chat = lens.children(topic)
  const last = chat.findLastIndex((id) => lens.read(id)?.type === 'claude.response')
  return chat.slice(last + 1).filter((id) => {
    const item = lens.read(id)
    // A path says where the next dispatch runs; it is not something to send.
    if (!item || item.type === 'path') return false
    return item.type === 'note' ? typeof item.data.text === 'string' && item.data.text.trim() !== '' : item.type !== 'claude.response'
  })
}

/** The newest path under a topic: where its next dispatch runs. Null when it has none. */
export function latestPath(topic: string, lens: Lens): Entity | null {
  const chat = lens.children(topic)
  for (let at = chat.length - 1; at >= 0; at--) {
    const item = lens.read(chat[at])
    if (item?.type === 'path') return item
  }
  return null
}

/** How an unsent item goes into the prompt: a message as its text, a file by its path, anything else by its id (Claude reads it through the app's MCP server). */
export function promptPart(id: string, lens: Lens): string {
  const item = lens.read(id)
  if (item?.type === 'note') return String(item.data.text)
  if (item?.type === 'file') return `[File: ${String(item.data.path)}]`
  return `[Item: ${id}]`
}

/** Whether Claude is answering in a topic: a response under it is still running. */
export function answering(topic: string, lens: Lens): boolean {
  return lens.children(topic).some((id) => {
    const item = lens.read(id)
    return item?.type === 'claude.response' && item.data.running === true
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
  // A file is only ever pasted into a topic, and a path only ever chosen for one, for now.
  owns: (type) => type === 'topics.home' || type === 'topic' || type === 'file' || type === 'path',
  // A topic reads like chat: the newest at the bottom, where the cursor starts.
  newestFirst: (type) => type === 'topic',
  compose: (entity) => (entity.type === 'topic' ? 'topic' : null),
  // The most recently active first: a message, a dispatch or an answer moves one to the top.
  order: (entity, children) => (entity.type === 'topics.home' ? byRecent(children) : children),
  present: (entity) => (entity.type === 'topics.home' ? { ...entity, data: { ...entity.data, text: entity.data.text ?? 'Topics' } } : entity),
}
