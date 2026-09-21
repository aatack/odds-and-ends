import type { QueueEntry, QueueView, TopicDetail, TopicId, WaitingEntry } from '@conswap/common/types'
import type { Context } from './context.js'
import { suggestBlockers } from './suggestions.js'
import { childCount, eventTypes, getTopic, parents, readChildren, topicBlockers } from './topics.js'

const readableTypes: Record<string, string> = {
  note: 'a note',
  slack_message: 'a Slack message',
  claude_response: 'a reply from Claude',
  claude_prompt: 'a prompt to Claude',
  github_event: 'something on GitHub',
  system: 'a blocker clearing',
  topic: 'a subtopic',
}

/** The last thing that happened in a topic, as something readable in a list. */
function latestReason(context: Context, id: TopicId): string {
  const row = context.db
    .prepare(
      `SELECT topics.type AS type, topics.text AS text FROM links
       JOIN topics ON topics.id = links.child_id
       WHERE links.parent_id = ? ORDER BY links.created_at DESC LIMIT 1`,
    )
    .get(id) as { type: string; text: string } | undefined
  if (!row) return 'nothing here yet'
  const label = readableTypes[row.type] ?? row.type
  const preview = row.text.replace(/\s+/g, ' ').trim().slice(0, 80)
  return preview.length > 0 ? `${label}: ${preview}` : label
}

export function readTopicDetail(context: Context, id: TopicId, expanded: Set<TopicId>): TopicDetail | null {
  const topic = getTopic(context.db, id)
  if (!topic) return null
  const blockers = topicBlockers(context.db, id, false)
  return {
    topic,
    parents: parents(context.db, id),
    children: readChildren(context.db, id, 3, expanded),
    blockers,
    suggestions: suggestBlockers(context, id),
    blocked: blockers.length > 0,
  }
}

interface QueueRow {
  id: string
}

export function readQueue(context: Context): QueueView {
  const placeholders = [...eventTypes].map(() => '?').join(', ')
  const openRows = context.db
    .prepare(
      `SELECT id FROM topics
       WHERE open = 1 AND type NOT IN (${placeholders})
       ORDER BY resolved ASC, updated_at DESC LIMIT 200`,
    )
    .all(...eventTypes) as QueueRow[]

  const open: QueueEntry[] = []
  for (const row of openRows) {
    const topic = getTopic(context.db, row.id)
    if (!topic) continue
    open.push({ topic, childCount: childCount(context.db, topic.id), reason: latestReason(context, topic.id) })
  }

  const waitingRows = context.db
    .prepare(
      `SELECT DISTINCT topics.id AS id FROM topics
       JOIN blockers ON blockers.topic_id = topics.id
       WHERE topics.open = 0 AND blockers.satisfied_at IS NULL AND blockers.cancelled_at IS NULL
         AND topics.type NOT IN (${placeholders})
       ORDER BY topics.updated_at DESC LIMIT 200`,
    )
    .all(...eventTypes) as QueueRow[]

  const waiting: WaitingEntry[] = []
  for (const row of waitingRows) {
    const topic = getTopic(context.db, row.id)
    if (!topic) continue
    waiting.push({ topic, blockers: topicBlockers(context.db, topic.id, false) })
  }

  return { open, waiting }
}

/**
 * The next thing to look at. Unresolved topics come first; a topic that was signed
 * off but has woken up again is offered once everything else is clear.
 */
export function nextOpenTopic(context: Context, after: TopicId | null): TopicId | null {
  const queue = readQueue(context).open
  if (queue.length === 0) return null
  const unresolved = queue.filter((entry) => !entry.topic.resolved)
  const pool = unresolved.length > 0 ? unresolved : queue
  if (!after) return pool[0]?.topic.id ?? null
  const index = pool.findIndex((entry) => entry.topic.id === after)
  if (index === -1) return pool[0]?.topic.id ?? null
  return pool[(index + 1) % pool.length]?.topic.id ?? null
}
