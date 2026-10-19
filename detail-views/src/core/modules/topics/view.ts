import type { Lens, ModuleView } from '../../present.ts'
import type { Entity } from '../../types.ts'
import { blockers, localApproval } from '../github/view.ts'
import { byRecent } from '../claude/view.ts'

export const topicsIds = { root: 'topics' }

/**
 * How a topic is shown and worked: a **task** (a chat with Claude, the
 * default) or **thoughts** (a tree of notes, as in entity-graph's editor,
 * with nothing of Claude's: no path, no dispatch). Kept as its `display`;
 * none is a task.
 */
export const topicDisplays = ['task', 'thoughts'] as const
export type TopicDisplay = (typeof topicDisplays)[number]

export const displayOf = (data: Record<string, unknown>): TopicDisplay => (data.display === 'thoughts' ? 'thoughts' : 'task')

/** Whether an item is a topic shown as a chat with Claude. */
export const isTaskTopic = (entity: Pick<Entity, 'type' | 'data'> | null | undefined): boolean => entity?.type === 'topic' && displayOf(entity.data) === 'task'

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
  // A task reads like chat: the newest at the bottom, where the cursor starts. Thoughts read as a tree, from the top.
  newestFirst: (type, entity) => type === 'topic' && (!entity || isTaskTopic(entity)),
  compose: (entity) => (isTaskTopic(entity) ? 'topic' : null),
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
  /** A Slack conversation or thread: back when someone else says something there. */
  slack?: string
  /** Another topic: back once it is resolved. */
  topic?: string
  /** A PR: back once it needs me (`prBack`), merged or closed. */
  pr?: string
  /** A snooze: back at this time (Unix ms). A topic has one at most (`snooze`). */
  until?: number
  since: number
  dismissed?: string[]
}

/** What a snooze is on, as `waitOn` names it: a topic has one at most. */
export const snooze = 'snooze'

/** What a wait is on: its Slack conversation or thread, its topic, its PR, or `snooze`. */
export const waitOn = (wait: Wait): string => wait.slack ?? wait.topic ?? wait.pr ?? (wait.until !== undefined ? snooze : '')

/** A topic's waits, as kept on it (anything malformed dropped). */
export function waitsOf(data: Record<string, unknown>): Wait[] {
  if (!Array.isArray(data.wait)) return []
  return data.wait.filter((wait): wait is Wait => (typeof wait?.slack === 'string' || typeof wait?.topic === 'string' || typeof wait?.pr === 'string' || typeof wait?.until === 'number') && typeof wait.since === 'number')
}

/**
 * Whether a wait is back: a Slack one when something came into it
 * (`waitCandidates`), a topic one once that topic is resolved, a PR one
 * once it needs me (`prBack`), a snooze once its time has come.
 */
export function waitReturned(topic: string, wait: Wait, lens: Lens, now: number = Date.now()): boolean {
  if (wait.until !== undefined) return now >= wait.until
  if (wait.topic) return Boolean(lens.read(wait.topic)?.data.resolved)
  if (wait.pr) return prBack(wait.pr, lens).length > 0
  return waitCandidates(topic, wait, lens).length > 0
}

/** What brings a wait on a PR back (`prBackReasons`), from the cache. */
export function prBack(pr: string, lens: Lens): string[] {
  const data = lens.read(pr)?.data
  return data ? prBackReasons(data, Boolean(localApproval(lens, pr))) : []
}

/** What keeps a PR of mine waiting: CI running, someone else's review, auto-merge under way. Anything else on it is mine to do. */
const othersTurn = new Set(['pending', 'awaiting', 'auto'])

/**
 * Why a wait on a PR is back, as the GitHub view names it (`blockers`);
 * empty while it waits, or until the PR has loaded. Merged or closed: back.
 * Mine: waits while CI runs or someone else's review is all that is left
 * (or it merges itself); back for anything I must do (conflicts, CI
 * failing, changes asked for, threads, my own review, ready to merge).
 * Someone else's: back only while it wants my review and is not a draft.
 */
export function prBackReasons(data: Record<string, unknown>, locallyApproved: boolean): string[] {
  const all = blockers(data, locallyApproved)
  if (!all.length) return []
  const ended = all.filter((blocker) => blocker.id === 'merged' || blocker.id === 'closed')
  if (ended.length) return ended.map((blocker) => blocker.label)
  if (data.mine) {
    const mineToDo = all.filter((blocker) => !othersTurn.has(blocker.id))
    return mineToDo.map((blocker) => blocker.label)
  }
  if (data.draft) return []
  const wantsMe = data.reviewRequestedOfMe === true || (!data.approvedByMe && !data.changesRequestedByMe)
  return wantsMe ? [data.reviewRequestedOfMe ? 'Review requested' : 'Not reviewed'] : []
}

const minute = 60_000
const hour = 60 * minute

/**
 * How often a wait on a PR is looked at again (its own loads), by how long
 * it has waited: backing off, as a change gets less likely to come soon.
 * Every 2 minutes for the first hour, every 15 for the first day, then
 * hourly. Null for the rest: a Slack wait loads with every poll of the
 * watch (`Core.pollSlackWaits`), and a topic's or a snooze is local.
 */
export function waitPollEvery(wait: Wait, age: number): number | null {
  if (wait.pr) return age < hour ? 2 * minute : age < 24 * hour ? 15 * minute : hour
  return null
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
  // A topic's wait brings nothing in: it is only back, or not.
  if (!wait.slack) return []
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
export function waiting(topic: string, lens: Lens, now: number = Date.now()): boolean {
  const waits = waitsOf(lens.read(topic)?.data ?? {})
  return waits.length > 0 && waits.every((wait) => !waitReturned(topic, wait, lens, now))
}

/** Whether a topic is snoozed now: a snooze of its own not yet over. */
export const snoozed = (data: Record<string, unknown>, now: number): boolean => waitsOf(data).some((wait) => wait.until !== undefined && wait.until > now)

/**
 * How well `text` matches `query`, fuzzily: every character of the query in
 * order, case aside. Null for no match; higher is better: runs of
 * consecutive characters, characters at the start of a word, and an early
 * first match count for more, gaps against.
 */
export function fuzzyScore(query: string, text: string): number | null {
  const q = query.toLowerCase().replace(/\s+/g, '')
  if (!q) return 0
  const t = text.toLowerCase()
  let score = 0
  let from = 0
  let last = -2
  for (const char of q) {
    const at = t.indexOf(char, from)
    if (at < 0) return null
    const wordStart = at === 0 || /[^a-z0-9]/.test(t[at - 1])
    score += 1 + (at === last + 1 ? 3 : 0) + (wordStart ? 2 : 0) - Math.min(at - from, 5) * 0.1
    if (last < 0) score -= Math.min(at, 10) * 0.05
    last = at
    from = at + 1
  }
  return score
}

/** Items ranked by how well their text matches `query` (`fuzzyScore`), best first; the rest left out. An empty query keeps them all, in order. */
export function fuzzyRank<T>(query: string, items: readonly T[], textOf: (item: T) => string): T[] {
  if (!query.trim()) return [...items]
  return items
    .map((item, index) => ({ item, index, score: fuzzyScore(query, textOf(item)) }))
    .filter((scored): scored is { item: T; index: number; score: number } => scored.score !== null)
    .sort((a, b) => b.score - a.score || a.index - b.index)
    .map((scored) => scored.item)
}

const snoozeUnits: Record<string, number> = { m: minute, h: hour, d: 24 * hour, w: 7 * 24 * hour }

/** How long a snooze typed as `30m`, `6h`, `2d` or `1w` lasts, in ms; null for anything else. */
export function snoozeLength(text: string): number | null {
  const match = /^\s*(\d+(?:\.\d+)?)\s*([mhdw])\s*$/i.exec(text)
  if (!match) return null
  const length = Number(match[1]) * snoozeUnits[match[2].toLowerCase()]
  return length > 0 ? length : null
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
