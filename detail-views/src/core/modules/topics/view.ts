import type { Lens, ModuleView } from '../../present.ts'
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
    if (!item) return false
    return item.type === 'note' ? typeof item.data.text === 'string' && item.data.text.trim() !== '' : item.type !== 'claude.response'
  })
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
  // A file is only ever pasted into a topic, for now.
  owns: (type) => type === 'topics.home' || type === 'topic' || type === 'file',
  // A topic reads like chat: the newest at the bottom, where the cursor starts.
  newestFirst: (type) => type === 'topic',
  compose: (entity) => (entity.type === 'topic' ? 'topic' : null),
  // The most recently active first: a message, a dispatch or an answer moves one to the top.
  order: (entity, children) => (entity.type === 'topics.home' ? byRecent(children) : children),
  present: (entity) => (entity.type === 'topics.home' ? { ...entity, data: { ...entity.data, text: entity.data.text ?? 'Topics' } } : entity),
}

// --- Waits ----------------------------------------------------------------------------

/**
 * What a topic waits for, as its `wait` value: a list, each one a Slack
 * conversation (`slack:conv:…`) or thread (its first message) that I want
 * to hear from again, since `since` (Unix ms). What has come into it since
 * (`waitCandidates`) is offered to add to the topic; what I passed over is
 * `dismissed`. Later kinds of wait go beside `slack`.
 */
export interface Wait {
  slack: string
  since: number
  dismissed?: string[]
}

/** A topic's waits, as kept on it (anything malformed dropped). */
export function waitsOf(data: Record<string, unknown>): Wait[] {
  if (!Array.isArray(data.wait)) return []
  return data.wait.filter((wait): wait is Wait => typeof wait?.slack === 'string' && typeof wait.since === 'number')
}

/** The ts in a Slack message id, as Unix ms; NaN for anything else. */
const messageMillis = (id: string): number => {
  const match = /^slack:msg:[^:]+:(\d+\.\d+)$/.exec(id)
  return match ? Number(match[1]) * 1000 : NaN
}

/**
 * What has come into a wait since it was set, oldest first: in a
 * conversation, its own messages (a thread's replies are the thread's), in a
 * thread, its replies. Not mine, not one already in the topic, not one I
 * dismissed, and not a join or leave.
 */
export function waitCandidates(topic: string, wait: Wait, lens: Lens): string[] {
  const self = lens.read('slack')?.data.self
  const inTopic = new Set(lens.children(topic))
  const dismissed = new Set(wait.dismissed ?? [])
  return lens
    .children(wait.slack)
    .filter((id) => messageMillis(id) > wait.since && !inTopic.has(id) && !dismissed.has(id))
    .filter((id) => {
      const data = lens.read(id)?.data
      return !(data && ((self && data.user === self) || data.subtype === 'channel_join' || data.subtype === 'channel_leave'))
    })
    .sort((a, b) => messageMillis(a) - messageMillis(b))
}

/**
 * Whether a topic is put down until something comes: it has waits and
 * nothing has come into any of them. Once anything has, it is up again; it
 * stays waiting until I take the wait away.
 */
export function waiting(topic: string, lens: Lens): boolean {
  const waits = waitsOf(lens.read(topic)?.data ?? {})
  return waits.length > 0 && waits.every((wait) => waitCandidates(topic, wait, lens).length === 0)
}

/**
 * Where a topic's box can send besides a note: for each Slack item in the
 * chat, a conversation is a message to it; a thread's reply, a message in
 * its thread; a message in a conversation, a reply in its thread, or a
 * message to its conversation. Each once, in the chat's order. `on` is the
 * conversation, or the thread's first message.
 */
export interface SendTarget {
  on: string
  channel: string
  threadTs?: string
}

export function sendTargets(topic: string, lens: Lens): SendTarget[] {
  const targets = new Map<string, SendTarget>()
  const add = (target: SendTarget) => targets.set(target.on, targets.get(target.on) ?? target)
  for (const id of lens.children(topic)) {
    const item = lens.read(id)
    if (!item) continue
    if (item.type === 'slack.conversation') add({ on: id, channel: id.slice('slack:conv:'.length) })
    const channel = item.data.channel
    // A message not yet loaded can't say whether it is in a thread: it offers nothing until it has.
    if (item.type !== 'slack.message' || typeof channel !== 'string' || typeof item.data.ts !== 'string') continue
    const thread = typeof item.data.threadTs === 'string' ? item.data.threadTs : item.data.ts
    add({ on: `slack:msg:${channel}:${thread}`, channel, threadTs: thread })
    if (thread === item.data.ts) add({ on: `slack:conv:${channel}`, channel })
  }
  return [...targets.values()]
}
